;;;; package.lisp --- Package definition for unit tests of the service-provider module.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.service-provider.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:configuration.options
   #:configuration.options.service-provider)

  (:import-from #:configuration.options.service-provider
   #:provider-designator-member
   #:%provider-designator->child-name)

  (:import-from #:configuration.options.test
   #:check-validate-value
   #:check-value<->string)

  (:import-from #:service-provider.test
   #:with-service)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the service-provider module."))

(cl:in-package #:configuration.options.service-provider.test)

;;; Test suite

(def-suite configuration.options.service-provider
  :description
  "Root test suite for the service-provider module.")

(defun run-tests ()
  (run! 'configuration.options.service-provider))

;;; Utilities

(defun instantiate-provider (service provider &rest options)
  (let+ ((schema        (service-schema service))
         (configuration (make-configuration schema))
         (synchronizer  (make-instance 'standard-synchronizer
                                       :target configuration))
         (source        (configuration.options.sources:make-source :defaults))
         (child-name    (parse-name (%provider-designator->child-name provider)))
         ((&flet value (name value)
            (setf (option-value (find-option name configuration)) value)))
         ((&flet provider-value (name value)
            (let ((name (merge-names child-name (parse-name name))))
              (value name value)))))
    ;; Pull in default values.
    (configuration.options.sources:initialize source schema)
    (configuration.options.sources:process source synchronizer)

    ;; Select provider and set the associated options.
    (value "provider" provider)
    (loop :for (name value) :on options :by #'cddr
       :do (provider-value name value))

    ;; Instantiate the provider.
    (service-provider:make-provider service configuration)))

;;; Some classes to be used as providers

(defclass bar ()
  ((a :initarg :a :type string)))

(defclass baz ()
  ((b :initarg :b :type (integer 0 10)))
  (:documentation
   "An implementation of the BAZ algorithm."))
