;;;; value-types.lisp --- Conversion and validation methods for some value types.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; type `boolean'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        (eql 'boolean))
                                     &key &allow-other-keys)
  (ecase value
    ((t)   "true")
    ((nil) "false")))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'boolean))
                                     &key &allow-other-keys)
  (cond
    ((string= value "true")  t)
    ((string= value "false") nil)
    (t                       (error "~@<~S is not a Boolean value~@:>"
                                    value))))

;;; type `integer'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       integer)
                                     (type        (eql 'integer))
                                     &key &allow-other-keys)
  (with-standard-io-syntax
    (princ-to-string value)))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'integer))
                                     &key
                                     inner-type)
  (let ((value (parse-integer value))
        (type  (list* type inner-type)))
    (declare (dynamic-extent type))
    (unless (typep value type)
      (error "~@<~S is not within the bounds specified by type ~S.~@:>"
             value type))
    value))

;;; type `string'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'string))
                                     &key &allow-other-keys)
  value)

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'string))
                                     &key &allow-other-keys)
  value)

;;; type `member'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       symbol)
                                     (type        (eql 'member))
                                     &key &allow-other-keys)
  (with-standard-io-syntax
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :invert)
      (princ-to-string value))))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        (eql 'member))
                                     &key
                                     inner-type)
  (or (find string inner-type :test #'string-equal)
      (error "~@<~S is not one of ~{~A~^, ~}.~@:>"
             string inner-type)))

;;; type `pathname'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       pathname)
                                     (type        (eql 'pathname))
                                     &key &allow-other-keys)
  (namestring value))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        (eql 'pathname))
                                     &key &allow-other-keys)
  (parse-namestring string))

;;; type `list'

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

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        (eql 'list))
                                     &key
                                     inner-type)
  (let+ ((element-type (first inner-type))
         ((&plist-r/o (inherit? :inherit?)) (rest inner-type))
         (components (split-sequence #\: string)))
   (mapcar (lambda (component)
             (if (eq component :inherit)
                 component
                 (string->value-using-type schema-item component element-type)))
           (if (and inherit?
                    (not (length= 1 components ))
                    (ends-with "" components :test #'string=))
               (remove-if
                (conjoin #'stringp #'emptyp)
                (substitute-if :inherit #'emptyp components :count 1 :from-end t))
               components))))

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

          (defmethod string->value-using-type
              ((schema-item type-based-conversion-mixin)
               (value  string)
               (type   (eql ',type))
               &key
               inner-type)
            (iter (for type1 in inner-type)
                  (let+ (((&values value1 error?)
                          (ignore-errors (string->value-using-type
                                          schema-item value type1))))
                    (when (not error?)
                      (return-from string->value-using-type value1))))
            (error "~@<~S is not valid for any of ~{~S~^, ~}.~@:>"
                   value inner-type)))))

  (define-composite-methods or  some)
  (define-composite-methods and every))
