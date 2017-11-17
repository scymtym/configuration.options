;;;; value-types.lisp --- Conversion and validation methods for some value types.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; type `null'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        (eql 'null))
                                     &key &allow-other-keys)
  (if (null value)
      "nil"
      (error "~@<~S is not ~S which is the only valid value of type ~
              ~S.~@:>"
             value nil type)))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'null))
                                  &key &allow-other-keys)
  (if (string= raw "nil")
      nil
      (error "~@<~S is not \"nil\" which is the only valid ~
              representation of values of type ~S.~@:>"
             raw type)))

;;; type `boolean'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        (eql 'boolean))
                                     &key &allow-other-keys)
  (ecase value
    ((t)   "true")
    ((nil) "false")))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'boolean))
                                  &key &allow-other-keys)
  (cond
    ((string= raw "true")  t)
    ((string= raw "false") nil)
    (t                     (error "~@<~S is not a Boolean value~@:>"
                                  raw))))

;;; type `integer'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       integer)
                                     (type        (eql 'integer))
                                     &key &allow-other-keys)
  (with-standard-io-syntax
    (princ-to-string value)))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'integer))
                                  &key
                                  inner-type)
  (let ((value (parse-integer raw)))
    (%maybe-check-detailed-type
     value type inner-type
     "~@<~S is not within the bounds specified by type ~S.~@:>")
    value))

;;; type `string'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'string))
                                     &key &allow-other-keys)
  value)

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'string))
                                  &key inner-type)
  (%maybe-check-detailed-type
   raw type inner-type
   "~@<~S is not of the length specified by type ~S.~@:>")
  raw)

;;; type `member'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       symbol)
                                     (type        (eql 'member))
                                     &key &allow-other-keys)
  (with-standard-io-syntax
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :invert)
      (princ-to-string value))))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'member))
                                  &key
                                  inner-type)
  (or (find raw inner-type :test #'string-equal)
      (error "~@<~S is not one of ~{~A~^, ~}.~@:>"
             raw inner-type)))

;;; type `pathname'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       pathname)
                                     (type        (eql 'pathname))
                                     &key &allow-other-keys)
  (namestring value))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'pathname))
                                  &key &allow-other-keys)
  (values (parse-namestring raw)))

;;; type `list'

(defmethod validate-value-using-type ((schema-item type-based-validation-mixin)
                                      (value       t)
                                      (type        (eql 'list))
                                      &key
                                      inner-type)
  (let+ (((&optional (element-type t) &rest &ign) inner-type))
    (and (listp value)
         (or (eq element-type t)
             (every (of-type element-type) value)))))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        (eql 'list))
                                    &key
                                    inner-type)
  (let+ (((&plist-r/o (inherit? :inherit?)) (rest inner-type))
         ((&flet remove-marker (list)
            (remove :inherit list :count 1 :from-end t))))
    (if inherit?
        (remove-marker
         (reduce
          (lambda (left &optional right)
            (if (ends-with :inherit left)
                (concatenate 'list (remove-marker left) right)
                left))
          values))
        (elt values 0))))

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       list)
                                     (type        (eql 'list))
                                     &key
                                     inner-type)
  (format nil "~{~A~^:~}~@[:~]"
          (mapcar (lambda (component)
                    (value->string-using-type
                     schema-item component (first inner-type)))
                  (remove :inherit value))
          (ends-with :inherit value :test #'eq)))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         list)
                                  (type        (eql 'list))
                                  &key
                                  inner-type)
  (let ((element-type (first inner-type)))
    (mapcar (lambda (component)
              (if (eq component :inherit)
                  component
                  (raw->value-using-type schema-item component element-type)))
            raw)))

(defmethod raw->value-using-type ((schema-item type-based-conversion-mixin)
                                  (raw         string)
                                  (type        (eql 'list))
                                  &rest args &key inner-type)
  (let+ (((&plist-r/o (inherit? :inherit?)) (rest inner-type))
         (components (split-sequence #\: raw))
         (components (if (and inherit?
                              (not (length= 1 components))
                              (ends-with "" components :test #'string=))
                         (remove-if
                          (conjoin #'stringp #'emptyp)
                          (substitute-if :inherit #'emptyp components
                                         :count 1 :from-end t))
                         components)))
    (apply #'raw->value-using-type schema-item components type
           args)))

;;; types `or' and `and'

(macrolet
    ((define-composite-methods (type operator)
       `(progn
          (defmethod validate-value-using-type
              ((schema-item type-based-validation-mixin)
               (value       t)
               (type        (eql ',type))
               &key
               inner-type)
            (,operator (curry #'validate-value-using-type schema-item value)
                       inner-type))

          (defmethod value->string-using-type
              ((schema-item type-based-conversion-mixin)
               (value       t)
               (type        (eql ',type))
               &key
               inner-type)
            (iter (for type1 in inner-type)
                  (let+ (((&values value error?)
                          (ignore-errors (value->string-using-type
                                          schema-item value type1))))
                    (when (not error?)
                      (return-from value->string-using-type value))))
            (error "~@<Could not ~S ~A ~S ~S.~@:>"
                   'value->string schema-item value
                   (list* type inner-type)))

          (defmethod raw->value-using-type
              ((schema-item type-based-conversion-mixin)
               (raw         t)
               (type        (eql ',type))
               &key
               inner-type)
            (iter (for type1 in inner-type)
                  (let+ (((&values value error?)
                          (ignore-errors (raw->value-using-type
                                          schema-item raw type1))))
                    (when (not error?)
                      (return-from raw->value-using-type value))))
            (error "~@<~S is not valid for any of ~{~S~^, ~}.~@:>"
                   raw inner-type)))))

  (define-composite-methods or  some)
  (define-composite-methods and every))

;;; Utility functions

(defun %maybe-check-detailed-type (value type inner-type format-control)
  (or (not inner-type)
      (let ((type (list* type inner-type)))
        (declare (dynamic-extent type))
        (typep value type))
      (error format-control value (list* type inner-type))))
