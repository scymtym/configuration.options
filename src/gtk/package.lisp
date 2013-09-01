;;;; package.lisp --- Package definition for gtk module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.gtk
  (:use
   #:cl
   #:alexandria ; TODO temp but probably needed
   #:split-sequence
   #:iterate
   #:let-plus

   #:configuration.options
   #:configuration.options.sources)

  ;; associated option mixin
  (:export
   #:widget-option)

  ;; associated configuration mixin
  (:export
   #:widget-configuration)

  (:export
   #:option-tree-store)

  (:export
   #:option-tree-view)

  (:export
   #:option-editor)

  ;; TODO editor creation protocol?
  (:export
   #:make-editor-using-type)

  (:export ; TODO separate library and program
   #:main))
