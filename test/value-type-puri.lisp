;;;; value-type-puri.lisp --- Unit tests for puri instances as values.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.puri.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options

   #:fiveam)

  (:import-from #:configuration.options.test
   #:check-validate-value
   #:check-value<->string))

(cl:in-package #:configuration.options.puri.test)

(def-suite configuration.options.puri
  :description
  "Test suite for the `puri' value type.")

(defun run-tests ()
  (run! 'configuration.options.puri))

(in-suite configuration.options.puri)

(test validate-value
  "Test for methods on `validate-value' for `puri:uri' objects."

  (mapc (lambda+ ((value expected))
          (check-validate-value value 'puri:uri expected))
        `((nil                 nil)
          (t                   nil)
          (1                   nil)
          (,(puri:uri "http:") t))))

(test value<->string
  "Test `value->string' and `raw->value' methods for `puri:uri' type."

  (mapc (lambda+ ((string value))
          (check-value<->string 'puri:uri string value :value-test #'puri:uri=))
        `(("1 2"    option-syntax-error)
          ("//"     option-syntax-error)
          (":"      option-syntax-error)
          ("http:/" ,(puri:uri "http:")))))
