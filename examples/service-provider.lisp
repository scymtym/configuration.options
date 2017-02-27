;;;; service-provider.lisp --- Demonstrates integration with the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.examples.service-provider
  (:use
   #:cl)

  (:import-from #:service-provider
   #:find-service
   #:define-service

   #:register-provider/class

   #:make-provider)

  (:import-from #:configuration.options
   #:find-option

   #:option-value

   #:make-configuration

   #:standard-synchronizer)

  (:import-from #:configuration.options.sources
   #:make-source
   #:initialize
   #:process)

  (:import-from #:configuration.options.service-provider
   #:service-schema))

(cl:in-package #:configuration.options.examples.service-provider)

;;; Define a service and with one class-based provider.

(define-service my-service
  (:documentation
   "My service is used for demonstration purposes."))

(defclass my-provider ()
  ((slot-1 :initarg  :slot-1
           :type     integer
           :initform 5))
  (:documentation
   "My provider of my service."))

(register-provider/class 'my-service :my-provider :class 'my-provider)

(describe (find-service 'my-service))

;;; Make a configuration schema for the service.

(defparameter *my-service-schema* (service-schema 'my-service))

(describe *my-service-schema*)

;;; Make and populate a configuration for the schema. Use the
;;; configuration to make and configure a provider.

(let* ((schema        *my-service-schema*)
       (configuration (make-configuration schema))
       (source        (make-source :defaults))
       (synchronizer  (make-instance 'standard-synchronizer
                                     :target configuration)))
  (initialize source schema)
  (process source synchronizer)

  (setf (option-value (find-option "provider"           configuration)) :my-provider
        (option-value (find-option "my-provider.slot-1" configuration)) -5)

  (make-provider 'my-service configuration))
