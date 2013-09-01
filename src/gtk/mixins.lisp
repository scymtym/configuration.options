;;;; mixins.lisp --- Mixins for TODO.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.gtk)

(defclass associated-configuration-mixin ()
  ((configuration :accessor widget-configuration
                  :documentation
                  ""))
  (:documentation
   "TODO(jmoringe): document"))

(defclass associated-option-mixin ()
  ((option :accessor widget-option
           :documentation
           ""))
  (:documentation
   "TODO(jmoringe): document"))
