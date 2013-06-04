;;;; protocol.lisp --- Protocol functions provided by the sources module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

;;; Source protocol

(defgeneric initialize (source schema)
  (:documentation
   "Initialize SOURCE with SCHEMA. SOURCE can, for example, examine
    the options in SCHEMA and create corresponding internal data
    structures, or user-facing interfaces, help texts, etc."))

;; One-shot sources

(defgeneric process (source sink)
  (:documentation
   "Process the configuration information in SOURCE providing the
    resulting configuration options to SINK."))

;; Default behavior

(define-condition-translating-method initialize ((source t) (schema t))
  ((error initialization-error)
   :source source
   :schema schema))

(defmethod initialize ((source t) (schema t))
  (values))

(define-condition-translating-method process ((source t) (sink t))
  ((error processing-error)
   :source source
   :sink   sink))

;;; Source service and construction protocol

(service-provider:define-service source
  (:documentation
   "This service is provided by implementations which process
    configuration sources such as configuration files or environment
    variables.

    Providers implement some or all of the above source protocols."))

(defgeneric make-source (spec &key &allow-other-keys)
  (:documentation
   "Construct and return an object implementing the source protocol
    according to SPEC and the remaining keyword arguments.

    For example, when SPEC is a symbol naming a provider of the
    `source' service, the remaining keyword arguments are used as
    initargs."))

;; Default behavior

(defmethod make-source ((spec symbol)
                        &rest args &key)
  (apply #'service-provider:make-provider 'source spec args))

;;; Source cascade protocol

(defgeneric source-sources (source)
  (:documentation
   "Return the inferior sources managed by SOURCE."))

;; Default behavior

(defmethod source-sources ((source t))
  '())

;;; Syntax protocol

(defgeneric process-content (syntax source sink)
  (:documentation
   "Process content of SOURCE assuming syntax SYNTAX and provide
    resulting options to SINK."))

;;; Syntax service and construction protocol

(service-provider:define-service syntax
  (:documentation
   "Providers of this service interpret the content of textual sources
    of configuration information according to particular syntactic
    rules and provide the resulting options to sinks.

    Providers implement the above syntax protocol."))

(defgeneric make-syntax (spec &key &allow-other-keys)
  (:documentation
   "Construct and return an object implementing the syntax protocol
    according to SPEC and the remaining keyword arguments.

    For example, when SPEC is a symbol naming a provider of the
    `syntax' service, the remaining keyword arguments are used as
    initargs."))

;; Default behavior

(defmethod make-syntax ((spec symbol)
                        &rest args &key)
  (apply #'service-provider:make-provider 'syntax spec args))
