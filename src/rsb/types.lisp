;;;; types.lisp --- Types used in the RSB integration.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.rsb)

;;; Types

(deftype endpoint-name ()
  "Name of a configuration endpoint which can be configured via RSB."
  'list #|of string|#)
