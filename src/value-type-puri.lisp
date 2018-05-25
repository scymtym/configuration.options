;;;; value-type-puri.lisp --- Conversion between strings and parsed URIs.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; `puri:uri' value type

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

;;; `proper-puri' value type

(defun proper-puri-p (thing)
  (and (typep thing 'puri:uri) (puri:uri-scheme thing)))

(deftype proper-puri ()
  '(and puri:uri (satisfies proper-puri-p)))

(setf (get 'proper-puri 'dont-expand) t)

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       puri:uri)
                                     (type        (eql 'proper-puri))
                                     &rest args &key &allow-other-keys)
  (unless (proper-puri-p value)
    (error "~@<~S is not a \"proper\" URI because it does not have a ~
           scheme component.~@:>"
           value))
  (apply #'value->string-using-type schema-item value 'puri:uri args))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (value       string)
                                  (type        (eql 'proper-puri))
                                  &rest args &key &allow-other-keys)
  (if-let ((colon (position #\: value)))
    (apply #'raw->value-using-type schema-item value 'puri:uri args)
    (error "~@<\"~A\" is not a \"proper\" URI because it does not have ~
            a scheme component.~@:>"
           value)))
