;;;; protocol.lisp --- Protocol provided by the service-provider module.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider)

;;; Service and provider schema protocol
;;;
;;; This protocol allows generating configuration schemas for services
;;; defined using the `architecture.service-provider' system. Such a
;;; schema contains a schema-item for selecting a provider of the
;;; service and sub-schemas for all providers of the service. A
;;; configuration conforming to such a schema can be used to
;;; instantiate and configure a provider of the service.
;;;
;;; Currently only works for class-based providers with declared slot
;;; types and simple initarg processing. Apart from that, neither the
;;; service definition nor its providers need to know or do anything
;;; special.

(defgeneric service-schema (service &key documentation)
  (:documentation
   "Return a configuration schema for SERVICE.

    SERVICE can be either a service instance or a
    `service-designator'.

    DOCUMENTATION, if supplied, replaces the default, automatically
    generated documentation string for the created schema.

    The returned schema contains a schema-item of the appropriate
    `member' type for selecting one of the available providers of
    SERVICE along with schema-items for configuring individual
    providers."))

(defgeneric provider-schema (service provider &key documentation)
  (:documentation
   "Return a configuration schema for PROVIDER of SERVICE.

    DOCUMENTATION, if supplied, replaces the default, automatically
    generated documentation string for the created provider."))

;; Default behavior

(defmethod service-schema ((service symbol) &rest args &key documentation)
  (declare (ignore documentation))
  (check-type service service-designator)

  (apply #'service-schema (find-service service) args))

;;; Provider configuration protocol
;;;
;;; This is basically an extension of the `make-provider' generic
;;; function that uses a configuration object to compute initargs for
;;; the created provider.

(defgeneric make-provider-using-configuration (service provider configuration)
  (:documentation
   "Make and return a provider of SERVICE according to PROVIDER and CONFIGURATION.

    PROVIDER is a `service-provider:provider-designator' that selects
    the provider of SERVICE.

    CONFIGURATION is a configuration the options of which should be
    turned into initargs for the created provider."))
