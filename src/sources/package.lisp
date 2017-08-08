;;;; package.lisp --- Package definition for sources module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:configuration.options)

  (:import-from #:configuration.options
   #:wild-name)

  (:import-from #:configuration.options.debug
   #:+config-debug-variable-suffix+

   #:output)

  ;; Conditions
  (:export
   #:schema-condition
   #:schema-condition-schema

   #:source-condition
   #:source-condition-source

   #:sink-condition
   #:sink-condition-sink

   #:initialization-error

   #:processing-error)

  ;; Source protocol and source creation
  (:export
   #:initialize
   #:process

   #:make-source)

  ;; Cascade source protocol
  (:export
   #:source-sources)

  ;; Syntax protocol and syntax creation
  (:export
   #:process-content

   #:make-syntax)

  ;; Utilities
  (:export
   #:configuration-files
   #:configuration-file-specs)

  (:documentation
   "This package contains configuration options sources.

    Sources such as configuration files, environment variables and
    commandline options provide option discovery and option value
    events to sinks (usually a synchronizer) which usually integrate
    the information into a configuration object."))
