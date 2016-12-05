;;;; variables.lisp --- Variables provided by the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; Internal variables and constants

(defconstant +no-value+ '..no-value..
  "This object is used to indicate that a value cell is not
   occupied.")

;;; User-facing variables

(defvar *configuration* nil
  "When bound to a configuration object, provides the default
   configuration for the `value' and setf `value' functions. ")
