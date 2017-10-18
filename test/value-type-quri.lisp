;;;; value-type-quri.lisp --- Unit tests for quri instances as values.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.quri.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options

   #:fiveam)

  (:import-from #:configuration.options.test
   #:check-validate-value
   #:check-value<->string))

(cl:in-package #:configuration.options.quri.test)

(def-suite configuration.options.quri
  :description
  "Test suite for the `puri' value type.")

(defun run-tests ()
  (run! 'configuration.options.quri))

(in-suite configuration.options.quri)

(test validate-value
  "Test for methods on `validate-value' for `quri:uri' objects."

  (mapc (lambda+ ((value expected))
          (check-validate-value value 'quri:uri expected))
        `((nil                 nil)
          (t                   nil)
          (1                   nil)
          (,(quri:uri "http:") t))))

(test value<->string
  "Test `value->string' and `raw->value' methods for `quri:uri' type."

  (mapc (lambda+ ((string value))
          (check-value<->string 'quri:uri string value :value-test #'quri:uri=))
        `(#+TODO-maybe ("1 2"         option-syntax-error)
          #+TODO-maybe ("//"          option-syntax-error)
          #+TODO-maybe (":"           option-syntax-error)
          ("http:"       ,(quri:uri "http:"))
          ("http://[::]" ,(quri:uri "http://[::]")))))
