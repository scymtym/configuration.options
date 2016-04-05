;;;; package.lisp --- Package definition for rsb module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.rsb
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:configuration.options
   #:configuration.options.sources)

  ;; Variables
  (:export
   #:*configuration-control-scope*
   #:*configuration-changes-scope*
   #:*schema-control-scope*)

  ;; Configuration event processing protocol
  (:export
   #:handle-event-using-data)

  ;; Configuration request protocol
  (:export
   #:request-configuration)

  (:documentation
   "This package contains facilities for remote configuration and
    introspection. The underlying protocol consists of exchanges of
    RSB events."))
