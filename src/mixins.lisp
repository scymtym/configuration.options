;;;; container.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

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
  `((:name ,(option-name object) " ~/options:print-name/"
           ((:before :value) (:before :type)))))

;;; `event-hook-mixin' class

(defclass event-hook-mixin ()
  ((event-hook :type     list
               :initform '()
               :documentation
               "Stores the handlers of the event hook of the
                object."))
  (:documentation
   "This class is intended to be mixed into classes which have to emit
    or relay events using the event hooks mechanism."))

(defmethod event-hook ((object event-hook-mixin))
  (hooks:object-hook object 'event-hook))

;;; `documentation-mixin' class

(defclass documentation-mixin ()
  ((documentation :initarg  :documentation
                  :type     (or null string)
                  :accessor option-documentation
                  :initform nil
                  :documentation
                  "Stores nil or the documentation string associated
                   to the option."))
  (:documentation
   "This class is intended to be mixed into all classes instances of
    which can have an associated documentation string."))

(defmethod documentation ((option documentation-mixin)
                          (type   (eql t)))
  (option-documentation option))

;;; `type-based-validation-mixin' class

(defclass type-based-validation-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into schema item classes
    which have to perform validation of values based their types.

    This behavior is implemented by a method on `validate-value' which
    calls `validate-value-using-type' with the `option-type' of the
    schema item.

    Default behavior is provided for types of the forms

    * (integer ...)
    * (and ...)
    * (or ...)"))

(defmethod validate-value ((schema-item type-based-validation-mixin)
                           (value       t)
                           &key
                           if-invalid)
  (declare (ignore if-invalid))
  (validate-value-using-type
   schema-item value (option-type schema-item)))

(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                      (value       t)
                                      (type        cons)
                                      &key
                                      inner-type)
  (if (member (first type) '(integer rational float real member))
      (call-next-method)
      (validate-value-using-type
       schema-item value (first type)
       :inner-type (append (rest type) (ensure-list inner-type)))))

(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                      (value       t)
                                      (type        t)
                                      &key
                                      inner-type)
  (declare (ignore inner-type))
  (typep value type))

(macrolet
    ((define-composite-validation (type operator)
       `(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                              (value       t)
                                              (type        (eql ',type))
                                              &key
                                              inner-type)
        (,operator (curry #'validate-value-using-type schema-item value)
                   inner-type))))

  (define-composite-validation and every)
  (define-composite-validation or  some))

;;; `type-based-merging-mixin' class

;; TODO store merge strategy in separate slot instead of in type?
(defclass type-based-merging-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into schema item classes
    which have to perform merging of values based their types.

    This behavior is implemented by a method on `merges-values' which
    calls `merge-value-using-type' with the `option-type' of the
    schema item.

    Default behavior is provided for some types:

    * t: use the value with the highest priority; ignore other
      values
    * (list [:inherit? INHERIT]): depending on INHERIT either use the
      value with highest priority or look for :inherit markers in
      values and concatenate values appropriately."))

(defmethod merge-values ((schema-item type-based-merging-mixin)
                         (values      sequence))
  (if (emptyp values)
      (values nil nil)
      (values (merge-values-using-type
               schema-item values (option-type schema-item))
              t)))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        cons)
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

;;; `type-based-conversion-mixin' class

(defclass type-based-conversion-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into schema item classes
    which have to perform parsing and unparsing of values based their
    types.

    This behavior is implemented by a methods on `value->string' and
    `string->value' which call `value->string-using-type' and
    `string->value-using-type' respectively with the `option-type' of
    the schema item.

    Default behavior is provided for some types:

    * boolean
    * integer
    * string
    * member
    * pathname
    * (list TYPE [:inherit? INHERIT])
    * (or ...)
    * (and ...)"))

(defmethod value->string ((schema-item type-based-conversion-mixin)
                          (value       t))
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

;;; `list-container-mixin' class

(defclass list-container-mixin (event-hook-mixin)
  ((options :type     list
            :reader   options
            :accessor %options
            :initform nil
            :documentation
            "Stores a sorted list of named options. The contained
             options are sorted according to `name<'."))
  (:documentation
   "This class is intended to be mixed into classes which act as a
    container of options."))

(defmethod print-items append ((object list-container-mixin))
  `((:count ,(length (options object)) " (~D)")))

(defmethod find-options ((name      t)
                         (container list-container-mixin))
  (remove name (options container)
          :key  #'option-name
          :test (complement #'name-matches)))

(defmethod find-option ((name      t)
                        (container list-container-mixin)
                        &key &allow-other-keys)
  (find name (options container)
        :key  #'option-name
        :test #'name-equal))

(defmethod (setf find-option) :before ((new-value t)
                                       (name      t)
                                       (container list-container-mixin)
                                       &key &allow-other-keys)
  (let+ (((&accessors (options %options)) container))
    (setf options (delete name options
                          :key  #'option-name
                          :test #'name-equal))))

(defmethod (setf find-option) ((new-value t)
                               (name      t)
                               (container list-container-mixin)
                               &key &allow-other-keys)
  (when new-value
    (let+ (((&accessors (options %options)) container))
      (push new-value options)))
  new-value)

(defmethod (setf find-option) :after ((new-value t)
                                      (name      t)
                                      (container list-container-mixin)
                                      &key &allow-other-keys)
  (setf (%options container)
        (sort (options container) #'name< :key #'option-name))

  (hooks:run-hook
   (event-hook container) (if new-value :added :removed) name new-value))
