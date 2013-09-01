;;;; protocol.lisp --- Protocol provided by the gtk module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.gtk)

;;; Editor creation protocol

(defgeneric make-editor (widget option)
  (:documentation
   "Make and return a GTK-based widget for editing the value of OPTION.

    May, for example, call `make-editor-using-type' with the type of
    OPTION."))

(defgeneric make-editor-using-type (widget option type)
  (:documentation
   "Make and return a GTK-based widget for editing the value of OPTION
    which is of TYPE."))
