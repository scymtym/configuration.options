;;;; configuration.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; `standard-configuration' class

(defclass standard-configuration (list-container-mixin
                                  print-items-mixin)
  ((schema :initarg  :schema
           :reader configuration-schema
           :documentation
           ""))
  (:default-initargs
   :schema (missing-required-initarg 'standard-configuration :schema))
  (:documentation
   "TODO(jmoringe): document"))

;;; `option-cell' class

(defclass option-cell (print-items-mixin)
  ((schema-item :initarg  :schema-item
                :reader   option-schema-item
                :documentation
                "")
   (value       :initarg  :value
                :accessor option-value
                :documentation
                "

Is unbound when the option cell does not have a value."))
  (:default-initargs
   :schema-item (missing-required-initarg
                 'option-cell :schema-item))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod option-type ((option option-cell))
  (option-type (option-schema-item option)))

(defmethod (setf option-value) :before ((new-value t)
                                        (option    option-cell))
  "TODO(jmoringe): document"
  (validate-value (option-schema-item option) new-value))

(defmethod print-items append ((object option-cell))
  `((:type  ,(option-type object)  ": ~A" ((:before :value)))
    (:value ,(option-value object) " = ~S")))

;;; `standard-option' class

(defclass standard-option (named-mixin
                           print-items-mixin)
  ((cell :initarg  :cell
         :reader   option-%cell
         :documentation
         ""))
  (:default-initargs
   :cell (missing-required-initarg 'standard-option :cell))
  (:documentation
   "TODO(jmoringe): document"))

(macrolet
    ((define-delegation (name)
       `(defmethod ,name ((option standard-option))
          (,name (option-%cell option)))))

  (define-delegation option-schema-item)
  (define-delegation option-type)
  (define-delegation option-value)
  #+no (define-delegation (setf option-value) (new-value)))

(defmethod (setf option-value) ((new-value t) (option standard-option))
  (setf (option-value (option-%cell option)) new-value))

(defmethod print-items append ((object standard-option))
  `((:type  ,(option-type object)  ": ~A" ((:before :value)))
    (:value ,(option-value object) " = ~S")))
