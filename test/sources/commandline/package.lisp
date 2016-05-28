;;;; package.lisp --- Package definition for unit tests of the sources.commandline module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.commandline.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:fiveam

   #:configuration.options
   #:configuration.options.sources

   #:configuration.options.test
   #:configuration.options.sources.test)

  (:shadow
   #:run-tests)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the sources.commandline module."))

(cl:in-package #:configuration.options.sources.commandline.test)

;;; Test suite

(def-suite options.sources.commandline
  :description
  "Root test suite for the sources.commandline module.")

(defun run-tests ()
  (run! 'options.sources.commandline))
