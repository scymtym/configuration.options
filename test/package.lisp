;;;; package.lisp --- Package definition for unit tests of the options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016, 2017 Jan Moringen
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

   #:map-query-alignments

   #:type-based-validation-mixin
   #:type-based-merging-mixin
   #:type-based-conversion-mixin
   #:list-container-mixin)

  (:import-from #:configuration.options.sources
   #:initialize)

  (:export
   #:gen-ascii-name

   #:mock-source

   #:mock-sink
   #:sink-calls

                                                #:*simple-sub-schema*
   #:empty-schema                               #:*simple-schema*
                         #:simple-schema-item   #:*simple-schema-item*

   #:empty-configuration #:simple-configuraiton #:*simple-configuration*
                         #:simple-option        #:*simple-option*

   #:check-validate-value
   #:check-merge-values
   #:check-value<->string)

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
  (run! 'options))
