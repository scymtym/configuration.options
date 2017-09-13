;;;; value-type.lisp --- An option type for provider designators.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider)

;;; type `provider-designator-member'
;;;
;;; A `member'-like compound type-specifier for provider-designators
;;; of a given service.

(deftype provider-designator-member (&rest values)
  (labels ((value->type (value)
             (typecase value
               (symbol `(eql ,value))
               (cons   `(cons ,(value->type (car value))
                              ,(value->type (cdr value)))))))
    `(or ,@(mapcar #'value->type values))))

(defun provider-designator/cons->string (designator)
  (format nil "~(~{~A~^/~}~)" designator))

(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                      (value       t)
                                      (type        (eql 'provider-designator-member))
                                      &key inner-type &allow-other-keys)
  (typep value (list* type inner-type)))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       symbol)
                                     (type        (eql 'provider-designator-member))
                                     &key &allow-other-keys)
  (string-downcase value))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       cons)
                                     (type        (eql 'provider-designator-member))
                                     &key &allow-other-keys)
  (provider-designator/cons->string value))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'provider-designator-member))
                                  &key inner-type &allow-other-keys)
  (flet ((maybe-print (value)
           (etypecase value
             (cons   (provider-designator/cons->string value))
             (symbol (string-downcase value)))))
    (or (find raw inner-type :key #'maybe-print :test #'string=)
        (error "~@<~S does not designate a provider.~@:>" raw))))
