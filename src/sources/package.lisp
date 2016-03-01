;;;; package.lisp --- Package definition for sources module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options.sources
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:options)

  (:import-from #:options
   #:wild-name)

  ;; Condition
  (:export
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

  (:documentation
   "TODO"))
