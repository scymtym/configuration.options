;;;; schema.lisp --- Schema item and schema classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; `standard-schema' class

(defclass standard-schema (list-container-mixin
                           documentation-mixin
                           print-items-mixin)
  ((children :type     list
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
                        (match-wildcards? nil)
                        &allow-other-keys)
  (unless match-wildcards?
    (return-from find-option (call-next-method)))

  (let+ ((option   (find name (options container)
                         :key  #'option-name
                         :test (lambda (name query) (name-matches query name))))
         ;; TODO add :start :end to name-matches instead?
         ((&flet name-matches* (name query)
            (iter (for i :from (length name) :downto (length query))
                  (thereis (name-matches query (subseq name 0 i))))))
         (children (remove name (%children container)
                           :key  #'car
                           :test (complement #'name-matches*)))
         ((&flet+ process-child ((key . child))
            (let* ((index    (if (ends-with-subseq
                                  '(:wild-inferiors) (name-components key))
                                 (1- (length name))
                                 (length key)))
                   (sub-name (subseq name index)))
              (when-let ((option (find-option sub-name child
                                              :match-wildcards?  match-wildcards?
                                              :if-does-not-exist nil)))
                (return-from find-option option))))))
    (cond
      ((not children)
       option)
      ((not option)
       (mapc #'process-child children)
       nil)
      ((name< (car (first children)) (option-name option))
       (mapc #'process-child children)
       nil)
      (t
       option))))

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
    (push (cons key new-value) children))
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
   (default      :accessor option-%default
                 :documentation
                 "Stores the default value of the schema item. Is
                  unbound when the schema item does not have a default
                  value.")
   (option-class :initarg  :option-class
                 :type     symbol
                 :reader   option-class
                 :documentation
                 "TODO"))
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
    (values (option-%default option) t)))

(defmethod (setf option-%default) :before ((new-value t)
                                           (option    standard-schema-item))
  (validate-value option new-value))

(defmethod make-option ((schema-item standard-schema-item)
                        (name        sequence))
  (unless (name-matches
           (merge-names (make-name "**") (option-name schema-item))
            name)
    (error "~@<Schema item with name ~/options:print-name/ cannot make ~
            option with name ~/options:print-name/.~@:>"
           (option-name schema-item) name))

  ;; TODO(jmoringe, 2013-03-01): use `option-class' for cell instead of option?
  (let ((cell (make-instance 'option-cell
                             :schema-item schema-item)))
    (make-instance (option-class schema-item)
                   :name name
                   :cell cell)))
