;;;; protocol.lisp --- Protocol provided by the RSB integration.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.rsb)

;;; Configuration event processing protocol
;;;
;;; This protocol is implemented by RSB-based sources which emit
;;; configuration change events (i.e. `configuration.options:notify')
;;; for configuration change events (i.e. RSB events) received via RSB
;;; and thus update location configurations based on remote
;;; configuration updates.

(defgeneric handle-event-using-data (source sink event data)
  (:documentation
   "SOURCE relays the configuration information contained in EVENT and
    DATA to SINK using appropriate calls of
    `configuration.options:notify'."))

;;; Configuration request protocol
;;;
;;; This protocol is implemented by RSB-based sources which request
;;; receiving configuration events from remote participants. These
;;; events can be processed using the "Configuration event processing"
;;; protocol.

(defgeneric request-configuration (source)
  (:documentation
   "Request receiving configuration events from a remote
    participant.

    For example, when a configuration server is available, it can act
    upon configuration requests by looking up a specific configuration
    for the requesting client and sending corresponding configuration
    events to the client."))
