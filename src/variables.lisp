;;;; variables.lisp --- Variables provided by the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

(defvar *configuration* nil
  "When bound to a configuration object, provides the default
   configuration for the `value' and setf `value' functions. ")
