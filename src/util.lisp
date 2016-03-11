;;;; util.lisp --- Utilities used by the configuration.options system.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

(defun print-documentation (stream documentation &optional colon? at?)
  "`pprint-fill' the words in DOCUMENTATION onto STREAM."
  (declare (ignore colon? at?))
  (let ((*print-escape* nil))
    (pprint-fill stream (split-sequence #\Space documentation) nil)))
