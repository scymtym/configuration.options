;;;; package.lisp --- Package definition for sources.commandline module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.commandline
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:configuration.options
   #:configuration.options.sources)

  (:import-from #:configuration.options
   #:wild-name)

  (:documentation
   "This package contains a commandline options source."))
