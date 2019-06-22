;;;; mop.lisp --- Integration with the metaobject protocol.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; General idea:
;;;;
;;;; 1. Automatically derive a schema from a class metaobject
;;;;
;;;; 2. Convert options contained in a configuration for the schema
;;;;    into initargs for the class
;;;;
;;;; 1. is implemented by methods on the generic functions
;;;;
;;;; * `class-schema',
;;;; * `class-schema-items'
;;;; * `class-schema-slots'
;;;; * `slot-suitable-for-schema?'
;;;; * `slot-schema-item'
;;;; * `slot-schema-item-type'
;;;;
;;;; These generic functions collaborate as follows:
;;;;
;;;;           class: class           slot: slot-definition
;;;;                │                           │
;;;;   class-schema │                           │
;;;;   ────────────>│                           │
;;;;                │─┐class-schema-items       │
;;;;                ┃<┘                         │
;;;;                ┃─┐class-schema-slots       │
;;;;                ║<┘                         │
;;;;                ║                           │
;;;;                ║ slot-suitable-for-schema? │
;;;;                ║──────────────────────────>│
;;;;                ║                           │
;;;;                ║ slot-schema-item          │
;;;;                ║──────────────────────────>┃
;;;;                ║                           ┃
;;;;                ║                           ┃─┐slot-schema-item-type
;;;;                ║                           ┃<┘
;;;;                ║                           ┃───>item: schema-item
;;;;                ║                           │             │
;;;;                ║───>schema: schema         │             │
;;;;                ║          │                │             │
;;;;
;;;; 2. is implemented by methods on the generic function
;;;;    `option-initarg'.

(cl:defpackage #:configuration.options.mop
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options)

  (:import-from #:configuration.options
   #:typexpand-1-unless-builtin

   #:option-cell
   #:option-%cell)

  ;; Option initarg protocol
  (:export
   #:option-initarg)

  ;; Class schema protocol
  (:export
   #:class-schema
   #:class-schema-items
   #:class-schema-slots

   #:slot-suitable-for-schema?

   #:slot-schema-item
   #:slot-schema-item-type)

  (:documentation
   "Automatically derive a schema for a class based on its slots."))

(cl:in-package #:configuration.options.mop)

;;; Option initarg protocol

(defgeneric option-initarg (option class)
  (:documentation
   "Return the initarg symbol that should be used when building an
    initarg for CLASS corresponding to OPTION and its value.

    Methods implementing specialized behavior can specialize this
    method for option, cell and schema-item subclasses."))

;; Default behavior

(defmethod option-initarg ((option standard-option) (class t))
  (option-initarg (option-%cell option) class))

(defmethod option-initarg ((option option-cell) (class t))
  (option-initarg (option-schema-item option) class))

(defmethod option-initarg ((option standard-schema-item) (class t))
  (let ((component #+sbcl (last-elt (option-name option))
                   #-sbcl (lastcar (name-components (option-name option)))))
    (unless (stringp component)
      (error "~@<Cannot derive initarg for non-string name component ~S.~@:>"
             component))
    (make-keyword (string-upcase component))))

;;; Class schema protocol

(defgeneric class-schema (class &key documentation)
  (:documentation
   "Return a schema object for CLASS.

    If supplied, DOCUMENTATION is used as the documentation of the
    returned schema. If DOCUMENTATION is not supplied, the
    documentation of CLASS is used."))

(defgeneric class-schema-items (class)
  (:documentation
   "Return a list of schema item descriptions for CLASS.

    The returned item descriptions are of the form

      (KIND NAME ITEM)

    where KIND is :item, NAME is the name of the item within the
    schema and ITEM is the `schema-item' object."))

(defgeneric class-schema-slots (class)
  (:documentation
   "Return the list of effective slots of CLASS for which
    `schema-item's can be constructed."))

(defgeneric slot-suitable-for-schema? (class slot)
  (:documentation
   "Return true if an item for SLOT should be included in a schema for
    CLASS."))

(defgeneric slot-schema-item (class slot)
  (:documentation
   "Return a suitable schema item object for SLOT in CLASS."))

(defgeneric slot-schema-item-type (class slot slot-type)
  (:documentation
   "Return a type specifier for the schema item representing SLOT with
    type SLOT-TYPE in CLASS."))

;; Default behavior

(defmethod class-schema ((class class)
                         &key
                         (documentation (documentation class t)))
  (let ((schema          (make-instance 'standard-schema))
        (names-and-items (class-schema-items class)))
    (setf (option-documentation schema) documentation)
    (loop :for (kind name item) :in names-and-items
          :do (ecase kind
                (:item
                 (setf (find-option name schema) item))))
    schema))

(defmethod class-schema-items ((class class))
  (mapcar (curry #'slot-schema-item class) (class-schema-slots class)))

(defmethod slot-suitable-for-schema? ((class class)
                                      (slot  c2mop:effective-slot-definition))
  (let+ (((&accessors-r/o (name       c2mop:slot-definition-name)
                          (allocation c2mop:slot-definition-allocation)
                          (initargs   c2mop:slot-definition-initargs))
          slot))
    (and (eq allocation :instance)
         (or initargs
             (member name (%class-initargs class) :test #'string=)))))

(defmethod class-schema-slots ((class class))
  (let ((class (c2mop:ensure-finalized class)))
    (remove-if-not (curry #'slot-suitable-for-schema? class)
                   (c2mop:class-slots class))))

(defmethod slot-schema-item ((class class)
                             (slot  c2mop:effective-slot-definition))
  (let+ (((&accessors-r/o (name         c2mop:slot-definition-name)
                          (type         c2mop:slot-definition-type)
                          (initfunction c2mop:slot-definition-initfunction))
          slot)
         (default-initargs     (c2mop:class-default-initargs class))
         (default-initfunction (third (%default-initarg-for-slot
                                       slot default-initargs)))
         (name                 (string-downcase name))
         (type                 (slot-schema-item-type class slot type))
         ((&flet good-initfunction (function)
            (when function
              (ignore-errors
                (funcall function)
                function))))
         (default              (good-initfunction
                                (or default-initfunction initfunction)))
         (documentation        (documentation slot t)))
    (list :item
          name
          (apply #'make-instance 'standard-schema-item
                 :name name :type type
                 (append
                  (when default
                    (list :default default))
                  (when documentation
                    (list :documentation documentation)))))))

(defmethod slot-schema-item-type ((class class)
                                  (slot  t)
                                  (type  t))
  ;; FIXME this method is quite hacky and has ill-defined,
  ;; implementation-dependent semantics
  (labels ((rec (type)
             (let+ (((&values expanded expanded?)
                     (typexpand-1-unless-builtin type)))
               (if expanded?
                   (rec expanded)
                   (typecase expanded
                     ;; Translate type specifier
                     ;;
                     ;;   (or null (cons T))
                     ;;
                     ;; to pseudo-type specifier
                     ;;
                     ;;   (list T).
                     ((cons (eql or)
                            (cons (eql null)
                                  (cons (cons (eql cons)) null)))
                      `(list ,(rec (second (third expanded)))))
                     ;; Recursively translate `and' and `or' compound type
                     ;; specifiers.
                     ((cons (member and or))
                      `(,(first expanded) ,@(mapcar #'rec (rest expanded))))
                     (t
                      expanded))))))
    (rec type)))

;;; Utility functions

;;; Return a list of keywords each of which is an acceptable initarg
;;; of CLASS.
(defun %class-initargs (class)
  (let+ ((i-i-methods (c2mop:compute-applicable-methods-using-classes
                       #'initialize-instance (list class)))
         (s-i-methods (c2mop:compute-applicable-methods-using-classes
                       #'shared-initialize (list class (find-class 't))))
         ((&flet method-initargs (method)
            (mapcar #'caar
                    (nth-value 3 (parse-ordinary-lambda-list
                                  (c2mop:method-lambda-list method)))))))
    (remove-duplicates
     (append (mappend #'method-initargs (append i-i-methods s-i-methods))
             (mapcar #'first (c2mop:class-default-initargs class)))
     :test #'eq)))

;;; If either an initarg of SLOT matches an element of
;;; DEFAULT-INITARGS or the name of SLOT matches an element of
;;; DEFAULT-INITARGS, return that element.
(defun %default-initarg-for-slot (slot default-initargs)
  (or (find-if (lambda+ ((initarg &ign &ign))
                 (member initarg (c2mop:slot-definition-initargs slot)))
               default-initargs)
      (find-if (lambda+ ((initarg &ign &ign))
                 (string= initarg (c2mop:slot-definition-name slot)))
               default-initargs)))
