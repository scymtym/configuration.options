;;;; synchronizer.lisp --- Synchronizes configurations to external sources.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

(defconstant +no-value+ '..no-value..
  "This object is used to indicate that a value cell is not
   occupied.")

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
    \"target\" objects such as `standard-configuration' instances."))

(defmethod notify ((sink  standard-synchronizer)
                   (event (eql :added))
                   (name  t)
                   (value t)
                   &key
                   (index 0)
                   &allow-other-keys)
  (declare (ignore value))
  (let+ ((option (find-option name (synchronizer-target sink)
                              :if-does-not-exist :create))
         ((&accessors (values      option-values)
                      (schema-item option-schema-item)) option)
         ((&values default default?)
          (option-default option :if-does-not-exist nil)))

    ;; Connect the event hook of OPTION to the event hook of its
    ;; option cell.
    (hooks:add-to-hook
     (event-hook (option-%cell option))
     (lambda (event cell value)
       (declare (ignore cell value))
       (hooks:run-hook
        (event-hook option) event (option-name option) option)))

    ;; Adjust size of VALUES to NUMBER-OF-SOURCES + 1 so OPTION's
    ;; default value can be stored as final element. Save default
    ;; value.
    (let ((old-length (length values)))
      (when (> index (- old-length 2))
        (setf values (adjust-array values (+ index 2)
                                   :initial-element +no-value+))
        (when (plusp old-length)
          (setf (aref values (- (length values) 1))
                (aref values (- old-length 1))))))

    ;; When OPTION has a default, store it as final element of VALUES.
    (when default?
      (setf (aref values (1- (length values)))
            (list default :source :default))

      (%update-value option schema-item values)))

  (values))

(defmethod notify ((sink  standard-synchronizer)
                   (event (eql :removed))
                   (name  t)
                   (value t)
                   &key &allow-other-keys)
  (let+ (((&accessors-r/o (target synchronizer-target)) sink)
         (option (find-option name target)))

    ;; TODO Disconnect option event hook from cell event hook.

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
         ((&accessors-r/o (values      option-values)
                          (schema-item option-schema-item)) option)
         (value/parsed (if raw?
                           (string->value schema-item value)
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