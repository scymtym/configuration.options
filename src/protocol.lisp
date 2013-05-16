;;;; protocol.lisp --- Protocol provided by the options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; Name protocol
;;;
;;; Names are generally similar to pathnames: a sequence of name
;;; components which are either strings or one of the symbols :wild
;;; and :wild-inferiors.

(defgeneric make-name (thing)
  (:documentation
   "Return a name corresponding to THING which can be a name string, a
    sequence of `name-component's or a name."))

(defgeneric name-components (name)
  (:documentation
   "Return a sequence of the components of NAME."))

(defgeneric name-equal (left right)
  (:documentation
   "Return non-nil when LEFT and RIGHT are equal."))

(defgeneric name-matches (query name)
  (:documentation
   "Return non-nil when QUERY matches name. This can be the case
    either when QUERY and NAME are equal or when QUERY contains :wild
    or :wild-inferiors components matching components of NAME."))

(defgeneric name< (left right)
  (:documentation
   "Return non-nil when LEFT is BEFORE in the following ordering:
    components induce a lexicographical ordering where :wild-inferiors
    go before :wild which in turn goes before all other
    components."))

(defgeneric merge-names (left right)
  (:documentation
   "Construct and return a new name by concatenating the components of
    LEFT and RIGHT."))

;; Default behavior

(defmethod make-name ((thing t))
  (error 'type-error
         :datum         thing
         :expected-type '(or string sequence wildcard-name)))

(defmethod name-components ((name list))
  name)

(defmethod name-equal ((left t) (right t))
  (equal (name-components left) (name-components right)))

(defmethod name-matches ((query t) (name t))
  (name-equal query name))

(defmethod name< ((left t) (right t))
  (let+ (((&labels+ recur ((&optional left-first  &rest left-rest)
                           (&optional right-first &rest right-rest))
            (etypecase left-first
              (null
               (not (null right-first)))
              (string
               (typecase right-first
                 (null   nil)
                 (string (cond
                           ((string= left-first right-first)
                            (recur left-rest right-rest))
                           ((string< left-first right-first)
                            t)))
                 (t      t)))           ; RIGHT-FIRST is :wild or :wild-inferiors
              ((eql :wild)
               (typecase right-first
                 ((or null string) nil)
                 ((eql :wild)      (recur left-rest right-rest))
                 (t                t))) ; RIGHT-FIRST is :wild-inferiors
              ((eql :wild-inferiors)
               (when (eq right-first :wild-inferiors)
                 (recur left-rest right-rest)))))))
    (recur (name-components left) (name-components right))))

(defmethod merge-names ((left t) (right t))
  (concatenate (type-of left) left right))

(defmethod merge-names ((left (eql nil)) (right t))
  (concatenate (type-of right) left right))

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
   "Return a sequence of the options contained in CONTAINER."))

(defgeneric find-options (query container)
  (:documentation
   "Find and return a sequence of options in CONTAINER matching QUERY
    which can be a name with wildcard components."))

(defgeneric find-option (name container
                         &key
                         if-does-not-exist
                         if-exists)
  (:documentation
   "Find and return the option named NAME in CONTAINER.

    IF-DOES-NOT-EXIST controls the behavior in case there is no option
    named NAME. Acceptable values are functions accepting a condition
    object and nil.

    IF-EXISTS is accepted for parity with `(setf find-option)'."))

(defgeneric (setf find-option) (new-value name container
                                &key
                                if-does-not-exist
                                if-exists)
  (:documentation
   "Store the option NEW-VALUE under the name NAME in container.

    IF-DOES-NOT-EXIST is accepted for parity with `find-option'.

    IF-EXISTS controls the behavior in case an option named NAME is
    already stored in CONTAINER. Acceptable values are :supersede
    and :keep."))

;; when using (setf find-option) to add an option to a configuration,
;; consult the schema to check whether the option is valid:
;; * options are allowed in that particular subtree
;; * type is correct
;; * value is of correct type

;; Default behavior

(defmethod find-option :around ((name t) (container t)
                                &key
                                (if-does-not-exist #'error)
                                &allow-other-keys)
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
   "Return the type of OPTION. The returned type is an expression
    similar to a CL type."))

(defgeneric string->value-using-type (schema-item string type &key inner-type)
  (:documentation
   "Return the name object naming OPTION."))

;;; Option protocol

(defgeneric option-configuration (option)
  (:documentation
   "Return the name object naming OPTION."))

(defgeneric option-schema-item (option)
  (:documentation
   "Return the name object naming OPTION."))

(defgeneric option-value (option)
  (:documentation
   "Return the name object naming OPTION."))

;;; TODO(jmoringe, 2012-02-22): always supported?
(defgeneric (setf option-value) (new-value option)
  (:documentation
   "Return the name object naming OPTION."))
