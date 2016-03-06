;;;; configuration.lisp --- Option and configuration classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; `standard-configuration' class

(defclass standard-configuration (list-container-mixin
                                  print-items-mixin)
  ((schema :initarg  :schema
           :reader   configuration-schema
           :documentation
           "Stores the schema object associated to the configuration
            object."))
  (:default-initargs
   :schema (missing-required-initarg 'standard-configuration :schema))
  (:documentation
   "Instances of this class store collections of named options."))

(defmethod option-documentation ((option standard-configuration))
  (option-documentation (configuration-schema option)))

(defmethod find-option :around ((name      t)
                                (container standard-configuration)
                                &key
                                if-does-not-exist
                                &allow-other-keys)
  (case if-does-not-exist
    (:create
     (or (call-next-method name container :if-does-not-exist nil)
         (let* ((schema      (configuration-schema container))
                (schema-item (find-option name schema
                                          :match-wildcards?  t
                                          :if-does-not-exist #'error)))
           (setf (find-option name container)
                 (make-option schema-item name)))))
    (t
     (call-next-method))))

(defmethod documentation ((object standard-configuration)
                          (type   (eql t)))
  (option-documentation object))

;;; `option-cell' class

(defclass option-cell (event-hook-mixin
                       print-items-mixin)
  ((schema-item :initarg  :schema-item
                :reader   option-schema-item
                :documentation
                "Stores the associated schema item which in turn
                 stores the type, default and documentation for the
                 option.")
   (value       :accessor option-%value
                :documentation
                "Stores the value of the option cell.

                 Is unbound when the option cell does not have a
                 value.")
   (values      :type     vector
                :accessor option-values
                :initform (make-array 0)
                :documentation
                "Stores values and additional information regarding
                 their origins for the option as provided by sources
                 in order of decreasing priority. Entries are of the
                 form

                    (VALUE &rest PLIST)

                 where VALUE is a parsed value PLIST contains at least
                 the property :source holding the source object from
                 which VALUE originated. Additional properties may
                 describe the origin of VALUE in more detail."))
  (:default-initargs
   :schema-item (missing-required-initarg 'option-cell :schema-item))
  (:documentation
   "Instances of this class represent all aspects of options but their
    name.

    Options point to cells for value storage and, transitively, the
    associated schema-item. Multiple options can point to one
    `option-cell' instance."))

(defmethod shared-initialize :after ((instance   option-cell)
                                     (slot-names t)
                                     &key
                                     (value nil value-supplied?))
  (when value-supplied?
    (setf (option-value instance) value)))

(macrolet
    ((define-delegation (name)
       `(defmethod ,name ((option option-cell))
          (,name (option-schema-item option)))))

  (define-delegation option-type)
  (define-delegation option-documentation))

(defmethod option-default ((option option-cell)
                           &key
                           if-does-not-exist)
  (option-default (option-schema-item option)
                  :if-does-not-exist if-does-not-exist))

(defmethod option-value ((option option-cell)
                         &key
                         if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (when (slot-boundp option 'value)
    (values (option-%value option) t)))

(defmethod (setf option-value) :before ((new-value t)
                                        (option    option-cell)
                                        &key
                                        if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (validate-value (option-schema-item option) new-value))

(defmethod (setf option-value) ((new-value t)
                                (option    option-cell)
                                &key
                                if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (option-%value option) new-value))


(defmethod (setf option-value) :after ((new-value t)
                                       (option    option-cell)
                                       &key
                                       if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (hooks:run-hook (hooks:object-hook option 'event-hook)
                  :new-value option new-value))

(defmethod print-items append ((object option-cell))
  (let+ ((type (option-type object))
         ((&values value value?)
          (option-value object :if-does-not-exist nil))
         (value (list (if value? (list 1 value) (list 0)))))
    `((:type  ,type  ": ~A" ((:before :value)))
      (:value ,value " ~:{~[<no value>~;= ~S~]~}"))))

;;; `standard-option' class

(defclass standard-option (named-mixin
                           event-hook-mixin
                           print-items-mixin)
  ((cell :initarg  :cell
         :reader   option-%cell
         :documentation
         "Stores the cell which in turn stores the associated schema
          item and can store the actual value of the option."))
  (:default-initargs
   :cell (missing-required-initarg 'standard-option :cell))
  (:documentation
   "Instances of this class associate an option name to an schema item
    and potentially an option value."))

(macrolet
    ((define-delegation (name &optional setf?)
       `(progn
          (defmethod ,name ((option standard-option))
            (,name (option-%cell option)))

          ,@(when setf?
              `((defmethod (setf ,name) ((new-value t) (option standard-option))
                  (setf (,name (option-%cell option)) new-value)))))))

  (define-delegation option-schema-item)
  (define-delegation option-type)
  (define-delegation option-values t)
  (define-delegation option-documentation))

(defmethod option-default ((option standard-option)
                           &key
                           if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (option-default (option-%cell option) :if-does-not-exist nil))

(defmethod option-value ((option standard-option)
                         &key
                         if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (option-value (option-%cell option) :if-does-not-exist nil))

(defmethod (setf option-value) ((new-value t)
                                (option    standard-option)
                                &key
                                if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (setf (option-value (option-%cell option)) new-value))

(defmethod print-items append ((object standard-option))
  (print-items (option-%cell object)))

(defmethod documentation ((object standard-option)
                          (type   (eql t)))
  (option-documentation object))
