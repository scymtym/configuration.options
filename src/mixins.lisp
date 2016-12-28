;;;; mixins.lisp --- Mixins for configuration and schema classes.
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
  `((:name ,(option-name object) " ~/configuration.options:print-name/"
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
                          (string      string))
  (string->value-using-type schema-item string (option-type schema-item)))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        cons)
                                     &key
                                     inner-type)
  (let+ (((&flet dispatch-on-first ()
            (value->string-using-type
             schema-item value (first type)
             :inner-type (append (rest type) (ensure-list inner-type)))))
         ((&values expansion expanded?) (typexpand-1-unless-builtin type)))

    (cond
      ((not expanded?)
       (dispatch-on-first))
      ((ignore-errors
        (value->string-using-type schema-item value expansion)))
      (t
       (dispatch-on-first)))))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        cons)
                                     &key
                                     inner-type)
  (let+ (((&flet dispatch-on-first ()
            (string->value-using-type
             schema-item string (first type)
             :inner-type (append (rest type) (ensure-list inner-type)))))
         ((&values expansion expanded?) (typexpand-1-unless-builtin type)))
    (cond
      ((not expanded?)
       (dispatch-on-first))
      ((ignore-errors
        (string->value-using-type schema-item string expansion)))
      (t
       (dispatch-on-first)))))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        symbol)
                                     &key
                                     inner-type)
  (assert (not inner-type))
  (let+ (((&values expansion expanded?) (typexpand-1-unless-builtin type)))
    (if expanded?
        (value->string-using-type schema-item value expansion)
        (call-next-method))))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        symbol)
                                     &key
                                     inner-type)
  (assert (not inner-type))
  (let+ (((&values expansion expanded?) (typexpand-1-unless-builtin type)))
    (if expanded?
        (string->value-using-type schema-item string expansion)
        (call-next-method))))

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

(defmethod map-options ((function  function)
                        (container list-container-mixin))
  (mapc function (options container)))

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

;;; `describe-via-map-options-mixin'

(defclass describe-via-map-options-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into option container classes
    that need a method on `describe-opbject'."))

(defmethod describe-object ((object describe-via-map-options-mixin) stream)
  ;; We build a hash table which maps parents to children and then
  ;; perform a tree traversal.
  (let+ ((tree (make-hash-table :test #'equal))
         ((&labels add-item (name &optional item)
            (ensure-gethash
             name tree
             (let* ((node        (list name item '()))
                    (parent-name (butlast name))
                    (parent-node (when name
                                   (add-item parent-name))))
               (when parent-node
                 (push node (third parent-node)))
               node))))
         ((&labels+ print-first-line (stream &ign (name item &ign))
            (print-name stream (last name) t)
            item))
         ((&labels+ print-rest (stream &ign (&ign item &ign))
            (typecase item
              (standard-schema
               (when-let ((documentation (option-documentation item)))
                 (print-documentation stream documentation)))
              (t
               (describe-object item stream)))))
         ((&flet+ child< ((left-name  &ign  left-children)
                          (right-name &ign right-children))
            (cond
              ((and (not left-children) right-children)
               t)
              ((and left-children (not right-children))
               nil)
              ((name< left-name right-name)
               t))))
         ((&labels+ node-children ((&ign &ign children))
            (sort (copy-list children) #'child<))))
    (map-options
     (lambda (option &key prefix container &allow-other-keys)
       (let ((name (name-components
                    (merge-names prefix (option-name option)))))
         (add-item prefix container)
         (add-item name   option)))
     object)
    (when-let ((root (gethash '() tree)))
      (let ((*print-pretty* t)
            (*print-circle* nil))
        (utilities.print-tree:print-tree
         stream root
         (utilities.print-tree:make-node-printer
          #'print-first-line #'print-rest #'node-children))))))
