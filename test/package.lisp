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

   #:mock-sink
   #:sink-calls

   #:+simple-schema+)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the options system."))

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

;;; Simple schema for tests

(define-schema +simple-schema+
  "Simple configuration options for tests."
  ("foo" :type 'integer :default 1
         :documentation
         "This option controls foo.")
  ("bar" :type 'boolean)
  ("foo" ("fez" :type 'integer))
  ("bar" ("fez" :type 'pathname))
  ("baz" ("foo" :type 'string)))

;;; Mock sink class

(defclass mock-sink ()
  ((calls :type     list
          :accessor sink-calls
          :initform '())))

(defmethod initialize ((sink   mock-sink)
                       (schema (eql :intentional-error)))
  (error "~@<Intentional error.~@:>"))

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
