;;;; conversion.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; type `boolean'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       t)
                                     (type        (eql 'boolean))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  (ecase value
    ((t)   "true")
    ((nil) "false")))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'boolean))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  (cond
    ((member value '("true" "1") :test #'string=)
     t)
    ((member value '("false" "0") :test #'string=)
     nil)
    (t
     (error "The value string ~S is invalid for option ~A of type ~S."
            value schema-item type))))

;;; type `integer'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       integer)
                                     (type        (eql 'integer))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  (princ-to-string value))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'integer))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  (parse-integer value))

;;; type `string'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'string))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  value)

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (value       string)
                                     (type        (eql 'string))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  value)

;;; type `member'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       symbol)
                                     (type        (eql 'member))
                                     &key
                                     inner-type)
  (declare (ignore inner-type))

  (princ-to-string value))

(defmethod string->value-using-type ((schema-item type-based-conversion-mixin)
                                     (string      string)
                                     (type        (eql 'member))
                                     &key
                                     inner-type)
  (if (member string inner-type :test #'string=)
      (make-keyword string)
      (error #+later 'invalid-option-value "The value string ~S is invalid for option ~A. One of ~{~A~^, ~} expected."
             string schema-item inner-type)))

;;; type `list'

(defmethod value->string-using-type ((schema-item type-based-conversion-mixin)
                                     (value       list)
                                     (type        (eql 'list))
                                     &key
                                     inner-type)
  (format nil "~{~A~^:~}"
          (mapcar (lambda (component)
                    (value->string-using-type
                     schema-item component (first inner-type)))
                  value)))

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
           (if (and inherit? (ends-with "" components :test #'string=))
               (remove-if
                (conjoin #'stringp #'emptyp)
                (substitute-if :inherit #'emptyp components :count 1 :from-end t))
               components))))

(defmethod merge-values-using-type ((schema-item type-based-merging-mixin)
                                    (values      sequence)
                                    (type        (eql 'list))
                                    &key
                                    inner-type)
  (let+ (((&plist-r/o (inherit? :inherit?)) (rest inner-type)))
    (if inherit?
        (reduce
         (lambda (left &optional right)
           (if (ends-with :inherit left)
               (concatenate
                'list (remove :inherit left :count 1 :from-end t) right)
               left))
         values)
        (elt values 0))))
