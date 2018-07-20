;;;; package.lisp --- Package definition for sources.commandline module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.commandline
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:more-conditions
   #:print-items

   #:configuration.options
   #:configuration.options.sources)

  (:import-from #:configuration.options
   #:wild-name)

  (:documentation
   "This package contains a commandline options source."))
