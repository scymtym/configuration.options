;;;; protocol.lisp --- Protocol provided by the options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; Name protocol

(defgeneric name-components (name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric name-equal (left right)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric name-matches (query name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric name-< (left right)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric merge-names (left right)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod name-components ((name list))
  name)

(defmethod name-equal ((left t) (right t))
  (equal left right))

(defmethod name-matches ((query t) (name t))
  (name-equal query name))

(defmethod name-< ((left t) (right t))
  (< (length (name-components left))
     (length (name-components right))))

(defmethod merge-names ((left t) (right t))
  (concatenate (type-of left) left right))

(defmethod merge-names ((left (eql nil)) (right t))
  (concatenate 'list left right))

;;; Value protocol

(defgeneric value (option-or-name
                   &key
                   configuration
                   if-does-not-exist)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric (setf value) (new-value option-or-name
                          &key
                          configuration
                          if-does-not-exist)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric value-using-configuration (option configuration)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric (setf value-using-configuration) (new-value option configuration)
  (:documentation
   "TODO(jmoringe): document"))

;; Default behavior

(defmethod value ((option-or-name t)
                  &key
                  (configuration     *configuration*)
                  (if-does-not-exist #'error))
  "TODO(jmoringe): document"
  (let ((option-or-value
          (find-option option-or-name configuration
                       :if-does-not-exist if-does-not-exist)))
    (if (eq option-or-value if-does-not-exist)
        option-or-value
        (option-value option-or-value))))

;;; Option container protocol
;;;
;;; CONTAINER can be (at least) a schema or a configuration.

(defgeneric options (container)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric find-options (query container)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric find-option (name container
                         &key
                         if-does-not-exist)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric (setf find-option) (new-value name container
                                &key
                                if-exists)
  (:documentation
   "TODO(jmoringe): document"))
;; when using (setf find-option) to add an option to a configuration,
;; consult the schema to check whether the option is valid:
;; * options are allowed in that particular subtree
;; * type is correct
;; * value is of correct type

;; Default behavior

(defmethod find-option :around ((name list) (container t)
                                &key
                                (if-does-not-exist #'error))
  (labels
      ((recur ()
         (or (call-next-method)
             (etypecase if-does-not-exist
               (null
                nil)
               ((or (eql error) function)
                (error 'no-such-option
                       :name      name
                       :container container)))
             #+later (error-behavior-restart-case (if-does-not-exist
                                           (no-such-option
                                            :name      name
                                            :container container))
               (retry ()
                 (recur))
               (use-value (value)
                 value)))))
    (recur)))

(defmethod find-option ((name symbol) (container t)
                        &rest args &key &allow-other-keys)
  (apply #'find-option (list name) container args))

;;; Schema protocol

(defgeneric make-configuration (schema)
  (:documentation
   "TODO(jmoringe): document"))

;;; Configuration protocol

(defgeneric configuration-schema (configuration)
  (:documentation
   "TODO(jmoringe): document"))

;;; Option-like protocol
;;
;; OPTION can be (at least) an option or a schema items.

(defgeneric option-name (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-type (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-has-default? (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-default (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-description (option)
  (:documentation
   "TODO(jmoringe): document"))

;;; Schema item protocol

(defgeneric option-class (schema-item)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric make-option (schema-item name)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric validate-value (schema-item value &key if-invalid)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric merge-values (schema-item values)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric merge-values-using-type (schema-item values type &key inner-type)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric value->string (schema-item value)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric string->value (schema-item string)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric value->string-using-type (schema-item value type &key inner-type)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric string->value-using-type (schema-item string type &key inner-type)
  (:documentation
   "TODO(jmoringe): document"))

;;; Option protocol

(defgeneric option-configuration (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-schema-item (option)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric option-value (option)
  (:documentation
   "TODO(jmoringe): document"))

;;; TODO(jmoringe, 2012-02-22): always supported?
(defgeneric (setf option-value) (new-value option)
  (:documentation
   "TODO(jmoringe): document"))
