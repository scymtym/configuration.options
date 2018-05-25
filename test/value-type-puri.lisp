;;;; value-type-puri.lisp --- Unit tests for puri instances as values.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.puri.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options

   #:fiveam)

  (:import-from #:configuration.options
   #:proper-puri)

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

  (mapc (lambda+ ((type value expected))
          (check-validate-value value type expected))
        `((puri:uri    nil                     nil)
          (puri:uri    t                       nil)
          (puri:uri    1                       nil)
          (puri:uri    ,(puri:uri "http:")     t)

          (proper-puri ,(puri:uri "foo")       nil)
          (proper-puri ,(puri:uri "/foo/bar")  nil)
          (proper-puri ,(puri:uri "http:")     t)
          (proper-puri ,(puri:uri "http:/foo") t))))

(test value<->string
  "Test `value->string' and `raw->value' methods for `puri:uri' and
   `proper-puri' types."

  (mapc (lambda+ ((type string value))
          (check-value<->string type string value :value-test #'puri:uri=))
        `((puri:uri    "1 2"       option-syntax-error)
          (puri:uri    "//"        option-syntax-error)
          (puri:uri    ":"         option-syntax-error)
          (puri:uri    "http:/"    ,(puri:uri "http:"))

          (proper-puri "foo"       option-syntax-error)
          (proper-puri "/foo/bar"  option-syntax-error)
          (proper-puri "http:/"    ,(puri:uri "http:"))
          (proper-puri "http:/foo" ,(puri:uri "http:/foo")))))
