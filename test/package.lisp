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

  (:export
   #:options.root)

  (:documentation
   "This package contains unit tests for the options system."))

(cl:in-package #:options.test)

(def-suite options
  :description
  "Root unit test suite for the options system.")

(defun run-tests ()
  (run! 'options))
