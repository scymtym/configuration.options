;;;; schema.lisp --- Schema item and schema classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; `standard-schema' class

(defclass standard-schema (list-container-mixin
                           describe-via-map-options-mixin
                           documentation-mixin
                           print-items-mixin)
  ((children :type     list
             :accessor %children
             :reader   schema-children/alist
             :initform '()
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

(defmethod map-options ((function  function)
                        (container standard-schema))
  (let+ (((&labels one-schema (schema &optional (prefix '()))
            (mapc (lambda (option)
                    (funcall function option
                             :container schema
                             :prefix    prefix))
                  (options schema))
            (mapc (lambda+ ((name . child))
                    (one-schema child (merge-names prefix name)))
                  (schema-children/alist schema)))))
    (one-schema container)))

(defmethod find-option ((name      t)
                        (container standard-schema)
                        &key
                        (interpret-wildcards? nil)
                        &allow-other-keys)
  (let ((result nil))
    (map-matching-options
     (lambda (option &key container prefix)
       (declare (ignore container))
       (let ((name (if prefix
                       (merge-names prefix (option-name option))
                       (option-name option))))
         (when (or (not result)
                   (not (name< (car result) name)))
           (setf result (cons name option)))))
     name container :interpret-wildcards? interpret-wildcards?)
    (cdr result)))

(defmethod schema-children ((container standard-schema))
  (mapcar #'cdr (%children container)))

(defmethod find-child ((name      t)
                       (container standard-schema)
                       &key &allow-other-keys)
  (cdr (find name (%children container)
             :key  #'car
             :test #'name-equal)))

(defmethod (setf find-child) ((new-value t)
                              (key       sequence)
                              (schema    standard-schema)
                              &key &allow-other-keys)
  (let+ (((&accessors (children %children)) schema))
    (setf children (delete key children :key #'car :test #'name-equal))
    (when new-value
      (push (cons key new-value) children)))
  new-value)

(defmethod (setf find-child) :after ((new-value t)
                                     (key       t)
                                     (schema    standard-schema)
                                     &key &allow-other-keys)
  (setf (%children schema) (sort (%children schema) #'name< :key #'car)))

(defmethod make-configuration ((schema standard-schema))
  (make-instance 'standard-configuration :schema schema))

;;; `standard-schema-item' class

(defclass standard-schema-item (named-mixin
                                documentation-mixin
                                type-based-validation-mixin
                                type-based-conversion-mixin
                                type-based-merging-mixin
                                print-items-mixin)
  ((type         :initarg  :type
                 :reader   option-type
                 :documentation
                 "Stores the type of the option as an expression
                  similar to a CL type.")
   (default      :type     function
                 :accessor option-%default
                 :documentation
                 "Stores a function returning the default value of the
                  schema item.

                  Is unbound when the schema item does not have a
                  default value.")
   (option-class :initarg  :option-class
                 :type     symbol
                 :reader   option-class
                 :documentation
                 "Stores the name of the class that should be used for
                  making options corresponding to this schema item."))
  (:default-initargs
   :type         (missing-required-initarg 'standard-schema-item 'type)
   :option-class 'standard-option)
  (:documentation
   "Instances of this class associate an name or name pattern to a
    type and optionally a default value."))

(defmethod shared-initialize :after ((instance   standard-schema-item)
                                     (slot-names t)
                                     &key
                                     (default nil default-supplied?))
  (when default-supplied?
    (setf (option-%default instance) default)))

(defmethod describe-object ((object standard-schema-item) stream)
  (let+ (((&structure-r/o option- type documentation) object)
         ((&values default default?)
          (option-default object :if-does-not-exist nil)))
    (format stream "Type    ~A~%~
                    Default ~:[<no default>~*~;~<~@;~S~:>~]~@[~%~
                    ~/configuration.options::print-documentation/~]"
            type default? (list default) documentation)))

(defmethod print-items append ((object standard-schema-item))
  (let+ ((type (option-type object))
         ((&values default default?)
          (option-default object :if-does-not-exist nil))
         (default (list (if default? (list 1 default) (list 0)))))
    `((:type    ,type    ": ~A" ((:before :default)))
      (:default ,default " ~:{~[<no default>~;default ~S~]~}"))))

(defmethod option-default ((option standard-schema-item)
                           &key
                           if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (when (slot-boundp option 'default)
    (values (funcall (option-%default option)) t)))

(defmethod (setf option-%default) :around ((new-value t)
                                           (option    standard-schema-item))
  (typecase new-value
    (function (call-next-method))
    (t        (setf (option-%default option) (constantly new-value)))))

(defmethod (setf option-%default) :before ((new-value function)
                                           (option    standard-schema-item))
  (validate-value option (funcall new-value)))

(defmethod make-option ((schema-item standard-schema-item)
                        (name        sequence)
                        &key
                        (option-class      (option-class schema-item))
                        (option-cell-class 'option-cell))
  (when (typep name 'wild-name) ; TODO proper conditions
    (error "~@<~A cannot make an option with wild name ~
            ~/configuration.options:print-name/.~@:>"
           schema-item name))
  (unless (name-matches
           (merge-names (make-name "**") (option-name schema-item))
           name)
    (error "~@<~A with name ~/configuration.options:print-name/ cannot ~
            make an option with name ~
            ~/configuration.options:print-name/.~@:>"
            schema-item (option-name schema-item) name))

  (let ((cell (make-instance option-cell-class :schema-item schema-item)))
    (make-instance option-class :name name :cell cell)))
