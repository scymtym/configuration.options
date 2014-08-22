;;;; package.lisp --- Package definition for service-provider module.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.service-provider
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options)

  (:import-from #:service-provider
   #:service-designator
   #:provider-designator

   #:service-name
   #:service-providers
   #:service-providers/alist

   #:find-service

   #:standard-service

   #:provider-name

   #:find-provider
   #:make-provider

   #:provider-class
   #:class-provider)

  (:import-from #:configuration.options
   #:type-based-validation-mixin
   #:type-based-conversion-mixin)

  (:import-from #:configuration.options.mop
   #:class-schema

   #:option-initarg)

  ;; Types
  (:export
   #:provider-designator-member)

  ;; Service and provider schema protocol
  (:export
   #:service-schema
   #:provider-schema)

  ;; Provider configuration protocol
  (:export
   #:make-provider-using-configuration)

  (:documentation
   "Instantiate service providers according to configuration options."))
