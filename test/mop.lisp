;;;; mop.lisp --- Test for the integration with the metaobject protocol.
;;;;
;;;; Copyright (C) 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.mop.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:configuration.options.mop)

  (:import-from #:configuration.options
   #:name-equal

   #:option-name
   #:option-type
   #:option-default
   #:option-documentation

   #:options
   #:find-option)

  (:import-from #:configuration.options.test
   #:set-equal/name-equal

   #:*simple-option*
   #:*simple-configuration*)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the mop module."))

(cl:in-package #:configuration.options.mop.test)

(def-suite configuration.options.mop
  :description
  "Test suite for the integration with the metaobject protocol.")

(defun run-tests ()
  (run! 'configuration.options.mop))

;;; Utilities

(defun make-slot-definition (name slot-initargs class-initargs)
  (let+ (((&key
           (initform nil initform-supplied?)
           &allow-other-keys)
          slot-initargs)
         (class (apply #'make-instance 'standard-class
                       class-initargs))
         (slot  (apply #'make-instance
                       'c2mop:standard-effective-slot-definition
                       :class class
                       :name  name
                       (append
                        (when initform-supplied?
                          (list :initform     initform
                                :initfunction (compile
                                               nil `(lambda () ,initform))))
                        (remove-from-plist slot-initargs :initform)))))
    (values slot class)))

(in-suite configuration.options.mop)

;;; Option initarg protocol

(test option-initarg.smoke
  "Smoke test for the `option-initarg' generic function."

  (mapc (lambda+ ((option expected-initarg))
          (let+ ((option (find-option option *simple-configuration*))
                 ((&flet do-it ()
                    (option-initarg option 'foo))))
            (case expected-initarg
              (error (signals error (do-it)))
              (t     (is (eq expected-initarg (do-it)))))))

        '(("foo"       :foo)
          ("bar"       :bar)
          ("foo.fez"   :fez)
          ("sub.whoop" :whoop))))

;;; Class schema protocol

(defclass mock ()
  ((slot-3)
   (slot-1 :initarg    :slot-1)
   (slot-2 :initarg    :slot-2
           :initarg    :slot-2-b)
   (slot-4 :initarg    :slot-4
           :allocation :class)
   (slot-5)
   (slot-6))
  (:default-initargs
   :slot-5 1))

(defmethod shared-initialize :after ((instance   mock)
                                     (slot-names t)
                                     &key
                                     (slot-6 2))
  (declare (ignore slot-6)))

(test class-schema.smoke
  "Smoke test for the `class-schema' generic function."

  ;; Check which slots are selected for the schema.
  (let* ((schema  (class-schema (find-class 'mock)))
         (options (options schema)))
    (is (set-equal/name-equal '("slot-1" "slot-2" "slot-5" "slot-6")
                              (mapcar #'option-name options))))

  ;; Repeat to test operation on finalized class.
  (finishes (class-schema (find-class 'mock))))

(test slot-schema-item.smoke
  "Smoke test for the `slot-schema-item' generic function."

  (mapc
   (lambda+ (((name &rest slot-initargs)
              (&rest class-initargs)
              (expected-name expected-type
               &key
               ((:default       expected-default)       nil expected-default?)
               ((:documentation expected-documentation)))))
     (let+ (((&values slot class)
             (make-slot-definition name slot-initargs class-initargs))
            ((kind child-name schema-item)
             (slot-schema-item (c2mop:ensure-finalized class) slot))
            ((&structure-r/o option- name type documentation) schema-item)
            ((&values default default?)
             (option-default schema-item :if-does-not-exist nil)))
       (is (eq :item kind))
       (is (string= expected-name child-name))
       (is (name-equal (list expected-name) name))
       (is (equal expected-type type))
       (is (eq expected-default? default?))
       (when expected-default?
         (is (equal expected-default default)))
       (is (equal expected-documentation documentation))))

   `(((foo)                      () ("foo" t))
     ((foo :type integer)        () ("foo" integer))
     ((foo :initform 1)          () ("foo" t :default 1))
     ((foo :documentation "bla") () ("foo" t :documentation "bla"))
     ((foo :documentation "bla") () ("foo" t :documentation "bla"))
     ((foo :initargs (:foo))
      (:direct-default-initargs ((:foo 1 ,(constantly 1))))
      ("foo" t :default 1)))))

(deftype string-or-integer ()
  `(or string integer))

(deftype list-of (element-type)
  `(or null (cons ,element-type list)))

(test slot-schema-item-type.smoke
  "Smoke test for the `slot-schema-item-type' generic function."

  (mapc
   (lambda+ ((type expected-type))
     (let+ (((&values slot class)
             (make-slot-definition 'foo (list :type type) '()))
            ((&ign &ign schema-item) (slot-schema-item class slot))
            (type (option-type schema-item)))
       (is (equal type expected-type))))
   '(;; Atomic type specifiers.
     (integer                                integer)

     ;; Expansion of `deftype's but not standard types.
     (string-or-integer                      (or string integer))
     ((vector string)                        (vector string))

     ;; Hack for homogeneous lists.
     ((or null (cons integer))               (list integer))

     ;; Compound type specifiers `and' and `or'.
     ((and integer string)                   (and integer string))
     ((and integer (or null (cons integer))) (and integer (list integer)))

     ((or integer string)                    (or integer string))
     ((or integer (or null (cons integer)))  (or integer (list integer)))

     ;; Multiple transforms.
     ((list-of string)                       (list string)))))
