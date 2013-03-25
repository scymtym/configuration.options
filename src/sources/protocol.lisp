;;;; protocol.lisp --- Protocol functions provided by the sources module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

;;; Sink protocol

(defgeneric notify (sink event name &optional value)
  (:documentation
   "SINK is notified about some change regarding an option.

EVENT can be, for example, :added, :removed, :changed.

EVENT      NAME        VALUE
:added     OPTION-NAME
:removed   OPTION-NAME
:new-value OPTION-NAME RAW-NEW-VALUE"))

#+no (defgeneric raw-value (source option)
  (:documentation
   "TODO(jmoringe): document"))

;;; Source service and construction protocol

(service-provider:define-service source
  (:documentation
   "TODO

Providers implement the above source protocol."))

;; TODO(jmoringe, 2013-03-12): source should get schema (e.g. clon)
(defgeneric make-source (spec
                         &key &allow-other-keys)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod make-source ((spec symbol)
                        &rest args &key)
  "TODO(jmoringe): document"
  (apply #'service-provider:make-provider 'source spec args))

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

(defgeneric make-syntax (spec
                         &key &allow-other-keys)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod make-syntax ((spec symbol)
                        &rest args &key)
  "TODO(jmoringe): document"
  (apply #'service-provider:make-provider 'syntax spec args))
