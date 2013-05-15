;;;; package.lisp --- Package definition for unit tests of the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:eos

   #:options)

  (:import-from #:options
   #:name
   #:wild-name)

  (:import-from #:options.sources
   #:initialize)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the options system."))

(cl:in-package #:options.test)

;;; Test suite

(def-suite options
  :description
  "Root unit test suite for the options system.")

(defun run-tests ()
  (run! 'options))

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
  (appendf (sink-calls sink)
           (list (list* event name value
                        (remove-from-plist args :raw? :source)))))

(defmethod notify ((sink  mock-sink)
                   (event (eql :intentional-error))
                   (name  t)
                   (value t)
                   &key)
  (error "~@<Intentional error.~@:>"))
