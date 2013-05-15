;;;; container.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; `named-mixin' class

(defclass named-mixin ()
  ((name :reader   option-name
         :writer   (setf option-%name)
         :documentation
         "Stores the name of the option."))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name))
  (:documentation
   "This class is intended to be mixed into classes instances of which
    have an associated name."))

(defmethod shared-initialize :after ((instance   named-mixin)
                                     (slot-names t)
                                     &key
                                     (name nil name-supplied?))
  (when name-supplied?
    (setf (option-%name instance) (make-name name))))

(defmethod print-items append ((object named-mixin))
  `((:name ,(option-name object) " ~/options::print-name/"
           ((:before :value) (:before :type)))))

;;; `type-based-conversion-mixin' class

(defclass type-based-conversion-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod value->string ((schema-item type-based-conversion-mixin)
                          (value       string))
  (value->string-using-type schema-item value (option-type schema-item)))

(defmethod string->value ((schema-item type-based-conversion-mixin)
                          (value       string))
  (string->value-using-type schema-item value (option-type schema-item)))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        cons)
                                     &key
                                     inner-type)
  (value->string-using-type
   schema-item value (first type)
   :inner-type (append (rest type) (ensure-list inner-type))))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        cons)
                                     &key
                                     inner-type)
  (string->value-using-type
   schema-item value (first type)
   :inner-type (append (rest type) (ensure-list inner-type))))

;;; `type-based-merging-mixin' class

(defclass type-based-merging-mixin ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod merge-values ((schema-item type-based-merging-mixin)
                         (values      t))
  (merge-values-using-type schema-item values (option-type schema-item)))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        list)
                                    &key
                                    inner-type)
  (merge-values-using-type
   schema-item values (first type)
   :inner-type (append (rest type) (ensure-list inner-type))))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        t)
                                    &key &allow-other-keys)
  (elt values 0))

;;; `list-container-mixin' class

(defclass list-container-mixin ()
  ((options :type     list
            :reader   options
            :accessor %options
            :initform nil
            :documentation
            ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod print-items append ((object list-container-mixin))
  `((:count ,(length (options object)) " (~D)")))

(defmethod find-options ((name      t)
                         (container list-container-mixin))
  "TODO(jmoringe): document"
  (remove name (options container)
          :key  #'option-name
          :test (complement #'name-matches)))

(defmethod find-option ((name      t)
                        (container list-container-mixin)
                        &key &allow-other-keys)
  "TODO(jmoringe): document"
  (find name (options container)
        :key  #'option-name
        :test #'name-equal))

(defmethod (setf find-option) ((new-value t)
                               (name      t)
                               (container list-container-mixin)
                               &key &allow-other-keys)
  (push new-value (%options container))
  new-value)

(defmethod (setf find-option) :after ((new-value t)
                                      (name      t)
                                      (container list-container-mixin)
                                      &key &allow-other-keys)
  (setf (%options container)
        (sort (options container) #'name< :key #'option-name)))
