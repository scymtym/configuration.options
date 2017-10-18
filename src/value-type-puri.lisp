;;;; value-type-puri.lisp --- Conversion between strings and parsed URIs.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       puri:uri)
                                     (type        (eql 'puri:uri))
                                     &key &allow-other-keys)
  (with-output-to-string (stream)
    (puri:render-uri value stream)))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (value       string)
                                  (type        (eql 'puri:uri))
                                  &key &allow-other-keys)
  (puri:uri value))
