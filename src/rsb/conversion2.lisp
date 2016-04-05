;;;; conversion2.lisp --- Conversion functions for RSB-related types.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.rsb)

;;; type `rsb:scope'

(defmethod value->string-using-type ((schema-item configuration.options::type-based-conversion-mixin)
                                     (value       rsb:scope)
                                     (type        (eql 'rsb:scope))
                                     &key &allow-other-keys)
  (rsb:scope-string value))

(defmethod string->value-using-type ((schema-item configuration.options::type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'rsb:scope))
                                     &key &allow-other-keys)
  (rsb:make-scope value))

;;; type `puri:uri'

(defmethod value->string-using-type ((schema-item configuration.options::type-based-conversion-mixin)
                                     (value       puri:uri)
                                     (type        (eql 'puri:uri))
                                     &key &allow-other-keys)
  (with-output-to-string (stream)
    (puri:render-uri value stream)))

(defmethod string->value-using-type ((schema-item configuration.options::type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'puri:uri))
                                     &key &allow-other-keys)
  (puri:uri value))
