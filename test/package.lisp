;;;; package.lisp --- Package definition for unit tests of the options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:configuration.options)

  (:import-from #:configuration.options
   #:name
   #:wild-name

   #:type-based-validation-mixin
   #:type-based-merging-mixin
   #:type-based-conversion-mixin
   #:list-container-mixin)

  (:import-from #:configuration.options.sources
   #:initialize)

  (:export
   #:make-random-string

   #:mock-source

   #:mock-sink
   #:sink-calls

                        #:*simple-sub-schema*
                        #:*simple-schema*
   #:simple-schema-item #:*simple-schema-item*

   #:simple-option      #:*simple-option*)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the configuration.options
    system."))

(cl:in-package #:configuration.options.test)

;;; Test suite

(def-suite options
  :description
  "Root unit test suite for the options system.")

(defun run-tests ()
  (let ((results (run 'options)))
    (explain! results)
    (results-status results)))

;;; Utilities

(defun make-random-string (&key (case :upper) (length 20))
  "Return a random string of length LENGTH."
  (let ((base (case case
                (:upper (char-code #\A))
                (:lower (char-code #\a)))))
    (map-into (make-string length)
              (lambda () (code-char (+ base (random 26)))))))

(defun collect-map-options-calls (container)
  (let+ ((calls '())
         ((&flet collect-call (&rest args)
            (push args calls))))
    (map-options #'collect-call container)
    (nreverse calls)))

;;; Simple schema and schema-item for tests

(define-schema *simple-sub-schema*
  "Simple schema for inclusion in a parent schema."
  ("whoop" :type 'string)
  (:wild :type 'integer))

(define-schema *simple-schema*
  "Simple configuration options for tests."
  ("foo" :type 'integer :default 1
         :documentation
         "This option controls foo.")
  ("bar" :type 'boolean)
  ("foo" ("fez" :type 'integer))
  ("bar" ("fez" :type 'pathname))
  ("baz" ("foo" :type 'string))
  (:wild :type 'boolean)
  (("wild" :wild-inferiors) :type 'symbol)
  ("sub" *simple-sub-schema*))

(defun simple-schema-item (&key (name '("simple" "option")))
  (make-instance 'standard-schema-item
                 :name name
                 :type 'integer
                 :documentation
                 "A simple option."))

(defparameter *simple-schema-item* (simple-schema-item)
  "Simple schema-item for tests.")

;;; Simple option for tests

(defun simple-option (&key (name '("simple" "option") name-supplied?))
  (let ((schema-item (if name-supplied?
                         (simple-schema-item :name name)
                         *simple-schema-item*)))
    (make-option schema-item (option-name schema-item))))

(defparameter *simple-option* (simple-option)
  "Simple option for tests.")

;;; Mock source and sink classes

(defclass mock-source ()
  ())

(defmethod initialize ((sink   mock-source)
                       (schema (eql :intentional-error)))
  (error "~@<Intentional error.~@:>"))

(defclass mock-sink ()
  ((calls :type     list
          :accessor sink-calls
          :initform '())))

(defmethod notify ((sink  mock-sink)
                   (event t)
                   (name  t)
                   (value t)
                   &rest args &key)
  (let ((args/clean (remove-from-plist
                     args :raw? :source :bounds :option)))
    (appendf (sink-calls sink)
             (list (list* event name value args/clean)))))

(defmethod notify ((sink  mock-sink)
                   (event (eql :intentional-error))
                   (name  t)
                   (value t)
                   &key)
  (error "~@<Intentional error.~@:>"))
