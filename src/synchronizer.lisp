;;;; synchronizer.lisp --- Synchronizes configurations to external sources.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; Utilities

(defclass synchronizer-handler (closer-mop:funcallable-standard-object)
  ((synchronizer :initarg  :synchronizer
                 :reader    synchronizer-handler-synchronizer
                 :documentation
                 "Stores the synchronizer which created this
                  handler."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :synchronizer (missing-required-initarg 'synchronizer-handler :synchronizer)
   :option       (missing-required-initarg 'synchronizer-handler :option))
  (:documentation
   "Instances of this class are attached to event hooks of
    `option-cell's by synchronizers in order to forward events to the
    event hooks of options."))

(defmethod shared-initialize :after ((instance   synchronizer-handler)
                                     (slot-names t)
                                     &key
                                     option)
  (let ((hook (event-hook option)))
    (closer-mop:set-funcallable-instance-function
     instance
     (lambda (event cell value)
       (declare (ignore cell value))
       (hooks:run-hook hook event (option-name option) option)))))

(defun handler-of (synchronizer)
  (lambda (handler)
    (when (typep handler 'synchronizer-handler)
      (eq synchronizer
          (synchronizer-handler-synchronizer handler)))))

;;; `standard-synchronizer' class

(defclass standard-synchronizer ()
  ((target :initarg  :target
           :reader   synchronizer-target
           :documentation
           "Stores the object into which the synchronizer should
            implement changes it receives via `notify' calls."))
  (:default-initargs
   :target (missing-required-initarg 'standard-synchronizer :target))
  (:documentation
   "Instances of this class are notified of configuration changes via
    calls of `notify' generic function and implement these changes in
    \"target\" objects such as `standard-configuration' instances.

    Note: this synchronizer is not thread-safe."))

(defmethod notify ((sink  standard-synchronizer)
                   (event (eql :added))
                   (name  t)
                   (value t)
                   &key
                   (index 0)
                   &allow-other-keys)
  (declare (ignore value))
  (let+ ((option    (find-option name (synchronizer-target sink)
                                 :if-does-not-exist :create))
         (cell-hook (event-hook (option-%cell option)))
         ((&structure option- values schema-item) option))

    ;; Connect the event hook of OPTION to the event hook of its
    ;; option cell.
    (unless (find-if (handler-of sink) (hooks:hook-handlers cell-hook))
      (hooks:add-to-hook
       cell-hook (make-instance 'synchronizer-handler
                                :synchronizer sink
                                :option       option)))

    ;; Adjust size of VALUES to NUMBER-OF-SOURCES.
    (let ((old-length (length values)))
      (when (> index (- old-length 1))
        (setf values (adjust-array values (1+ index)
                                   :initial-element +no-value+)))))
  (values))

(defmethod notify ((sink  standard-synchronizer)
                   (event (eql :removed))
                   (name  t)
                   (value t)
                   &key &allow-other-keys)
  (let+ (((&structure-r/o synchronizer- target) sink)
         (option (find-option name target))
         (cell-hook (event-hook (option-%cell option))))

    ;; Disconnect option event hook from cell event hook.
    (when-let ((handler (find-if (handler-of sink)
                                 (hooks:hook-handlers cell-hook))))
      (hooks:remove-from-hook cell-hook handler))

    ;; Remove the option named NAME from TARGET.
    (setf (find-option name target) nil))

  (values))

(defmethod notify ((sink  standard-synchronizer)
                   (event (eql :new-value))
                   (name  t)
                   (value t)
                   &rest args &key
                   (index 0)
                   source
                   (raw?  t)
                   &allow-other-keys)
  (let+ ((option       (find-option name (synchronizer-target sink)))
         ((&structure-r/o option- values schema-item) option)
         (value/parsed (if raw?
                           (raw->value schema-item value)
                           value))
         (args/clean   (remove-from-plist args :index :source :raw?)))

    ;; Store parsed value in the cell within VALUES associated to
    ;; SOURCE via INDEX.
    (setf (aref values index)
          (list* value/parsed :source source args/clean))

    ;; Update OPTION's value.
    (%update-value option schema-item values))

  (values))

;;; Utility functions

(defun %update-value (option schema-item values)
  ;; Merge all values stored in VALUES and store the result (if any)
  ;; as value of OPTION.
  (let+ ((effective-values (map 'list #'first (remove +no-value+ values)))
         ((&values value/merged value/merged?)
          (merge-values schema-item effective-values)))
    (if value/merged?
        (setf (option-value option) value/merged)
        ;; TODO remove value
        )))
