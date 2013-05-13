;;;; schema.lisp --- Schema item and schema classes.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; `standard-schema' class

(defclass standard-schema (list-container-mixin
                           print-items-mixin)
  ((children :type     list
             :reader   schema-children
             :accessor %children
             :initform nil
             :documentation
             "Stores named child schemas. Elements are of the form

                (NAME . CHILD-SCHEMA)

              where NAME is a wild name."))
  (:documentation
   "Instances of this class store named schema items which describe
    option names, option types and other properties. In addition,
    schema instances can contain child schemas."))

(defmethod print-items append ((object standard-schema))
  `((:child-count ,(length (schema-children object)) " (C ~D)"
                  ((:after :count)))))

(defmethod find-option ((name      t)
                        (container standard-schema)
                        &key
                        if-does-not-exist
                        &allow-other-keys)
  (let+ ((option (find name (options container)
                       :key  #'option-name
                       :test (lambda (name query) (name-matches query name))))
         (child  (find name (%children container)
                       :key  #'car
                       :test (lambda (name query) (name-matches query name))))
         ((&flet+ process-child ((key . child))
            (let ((sub-name (subseq name (1- (length key)))))
              (find-option sub-name child :if-does-not-exist nil)))))
    (cond
      ((not child)
       option)
      ((not option)
       (process-child child))
      ((name< (car child) (option-name option))
       (process-child child))
      (t
       option))))

(defmethod (setf find-child) ((new-value t)
                              (key       wildcard-name)
                              (schema    standard-schema))
  "TODO(jmoringe): document"
  (push (cons key new-value) (%children schema)))

(defmethod (setf find-child) :after ((new-value t)
                                     (key       t)
                                     (schema    standard-schema))
  (setf (%children schema) (sort (%children schema) #'name< :key #'car)))

(defmethod make-configuration ((schema standard-schema))
  (make-instance 'standard-configuration :schema schema))

;;; `standard-schema-item' class

(defclass standard-schema-item (named-mixin
                                type-based-validation-mixin
                                type-based-conversion-mixin
                                type-based-merging-mixin
                                print-items-mixin)
  ((type         :initarg  :type
                 :reader   option-type
                 :documentation
                 "Stores the type of the option as an expression
                  similar to a CL type.")
   (option-class :initarg  :option-class
                 :type     symbol
                 :reader   option-class
                 :documentation
                 "")
   (default      :initarg  :default
                 :reader   option-default)
   (description  :initarg  :description
                 :type     (or null string)
                 :reader   option-description
                 :initform nil
                 :documentation
                 ""))
  (:default-initargs
   :type         (missing-required-initarg 'standard-schema-item 'type)
   :option-class 'standard-option)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod option-has-default? ((option standard-schema-item))
  (slot-boundp option 'default))

(defmethod make-option ((schema-item standard-schema-item)
                        (name        list))
  "TODO(jmoringe): document"
  #+no (unless (option-name-equal name (option-name schema-item))
    (error "Cannot make option ~S" name))

  ;;; TODO(jmoringe, 2013-03-01): use `option-class' for cell instead of option?
  (let ((cell (make-instance 'option-cell
                             :schema-item schema-item)))
    (make-instance (option-class schema-item)
                   :name name
                   :cell cell)))
