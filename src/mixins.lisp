;;;; mixins.lisp --- Mixins for configuration and schema classes.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

(defmacro define-dispatch-methods (name (class value-parameter value-specializer))
  `(progn
     (defmethod ,name ((schema-item      ,class)
                       (,value-parameter ,value-specializer)
                       (type             cons)
                       &key inner-type)
       ;; Try to find a method specialized on the `car' of TYPE (by
       ;; comparing the number of applicable method for the `car' of
       ;; TYPE with the number for the unspecialized case). If there
       ;; is no specialized method, try to expand TYPE and retry if
       ;; there is an expansion.
       (let+ (((&flet methods (type)
                 (compute-applicable-methods
                  #',name (list schema-item ,value-parameter type))))
              ((&flet method-for-first? (type-first)
                 (> (length (methods type-first))
                    (length (methods 'no-such-type))))))
         (if (method-for-first? (first type))
             (,name schema-item ,value-parameter (first type)
                    :inner-type (append (rest type) (ensure-list inner-type)))
             (let+ (((&values expansion expanded?)
                     (typexpand-1-unless-builtin type)))
               (if expanded?
                   (,name schema-item ,value-parameter expansion)
                   (call-next-method))))))

     (defmethod ,name ((schema-item      ,class)
                       (,value-parameter ,value-specializer)
                       (type             symbol)
                       &key inner-type)
       ;; Since TYPE is a symbol, we can only get here if there is no
       ;; specialized method. Try macroexpanding.
       (assert (not inner-type))
       (let+ (((&values expansion expanded?) (typexpand-1-unless-builtin type)))
         (if expanded?
             (,name schema-item ,value-parameter expansion)
             (call-next-method))))))

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
    * (list [ELEMENT-TYPE [:inherit? INHERIT]])
    * (and ...)
    * (or ...)"))

(defmethod validate-value ((schema-item type-based-validation-mixin)
                           (value       t)
                           &key
                           if-invalid)
  (declare (ignore if-invalid))
  (validate-value-using-type
   schema-item value (option-type schema-item)))

(define-dispatch-methods validate-value-using-type
    (type-based-validation-mixin value t))

(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                      (value       t)
                                      (type        t)
                                      &key inner-type)
  (cond
    ((consp type)
     (typep value type))
    (inner-type
     (let ((type (list* type inner-type)))
       (declare (dynamic-extent type))
       (typep value type)))
    (t
     (typep value type))))

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

    * (list [ELEMENT-TYPE [:inherit? INHERIT]]): depending on INHERIT
      either use the value with highest priority or look for :inherit
      markers in values and concatenate values appropriately."))

(defmethod merge-values ((schema-item type-based-merging-mixin)
                         (values      sequence))
  (if (emptyp values)
      (values nil nil)
      (values (merge-values-using-type
               schema-item values (option-type schema-item))
              t)))

(define-dispatch-methods merge-values-using-type
    (type-based-merging-mixin values sequence))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        t)
                                    &key inner-type)
  (declare (ignore inner-type))
  (elt values 0))

;;; `type-based-conversion-mixin' class

(defclass type-based-conversion-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into schema item classes
    which have to perform parsing and unparsing of values based their
    types.

    This behavior is implemented by a methods on `value->string' and
    `raw->value' which call `value->string-using-type' and
    `raw->value-using-type' respectively with the `option-type' of the
    schema item.

    Default behavior is provided for some types:

    * boolean
    * integer
    * string
    * member
    * pathname
    * (list ELEMENT-TYPE [:inherit? INHERIT])
    * (or ...)
    * (and ...)"))

(defmethod value->string ((schema-item type-based-conversion-mixin)
                          (value       t))
  (value->string-using-type schema-item value (option-type schema-item)))

(defmethod raw->value ((schema-item type-based-conversion-mixin)
                       (raw         t))
  (raw->value-using-type schema-item raw (option-type schema-item)))

(define-dispatch-methods value->string-using-type
    (type-based-conversion-mixin value t))
(define-dispatch-methods raw->value-using-type
    (type-based-conversion-mixin raw   t))

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
  (let ((*print-circle* nil)
        (*print-pretty* t))
    (format stream "~<~A~@:_~
                      ~@:_~
                      Tree:~@:_~
                      ~2@T~@<~/configuration.options:print-option-container-tree/~@:>~
                    ~@:>"
            (list object object))))

(defun print-option-container-tree (stream object &optional colon? at?)
  (declare (ignore colon? at?))
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
    (if-let ((root (gethash '() tree)))
      (utilities.print-tree:print-tree
       stream root
       (utilities.print-tree:make-node-printer
        #'print-first-line #'print-rest #'node-children))
      (format stream "<empty>"))))
