;;;; source-environment-variable.lisp --- Unit tests for the environment variables source.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test environment-variables-source.smoke
  (let* ((prefix        (concatenate 'string (make-random-string) "_"))
         (rest          (make-random-string))
         (rest/downcase (string-downcase rest))
         (name          (format nil "~A~A" prefix rest))
         (value         "value"))
    (with-environment-variable (name value)
      (with-source-and-sink ((:environment-variables :prefix prefix)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          `(:added     (,rest/downcase) nil)
          `(:new-value (,rest/downcase) ,value))))))
