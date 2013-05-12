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
   "Return the name object naming OPTION."))

(defgeneric option-type (option)
  (:documentation
   "Return the type of option."))

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
   "Determine whether VALUE is valid for SCHEMA-ITEM, signaling an
    error or if it is invalid.

    IF-INVALID controls the behavior in case VALUE is invalid for
    SCHEMA-ITEM:

    nil

      Return nil.

    'error, #'error

      Signal an error which indicates VALUE being invalid.

    a function

      Call the function with an error object which indicates VALUE
      being invalid as the sole argument."))

(defgeneric validate-value-using-type (schema-item value type
                                       &key inner-type)
  (:documentation
   "Like `validate-value' but may incorporate TYPE into the decision
    whether VALUE is valid for SCHEMA-ITEM."))

(defgeneric merge-values (schema-item values)
  (:documentation
   "Merge the sequence of values VALUES into a single value in the
    appropriate way for the type of SCHEMA-ITEM and return two values:
    1) nil or the merged value 2) t if the merge produced a value and
    nil if the merge did not produce a value."))

(defgeneric merge-values-using-type (schema-item values type
                                     &key inner-type)
 (:documentation
   "Like `merge-values' but may incorporate TYPE into the decision how
    to merge VALUES."))

(defgeneric value->string (schema-item value)
  (:documentation
   "Return a string representation of VALUE taking into account
    properties of SCHEMA-ITEM."))

(defgeneric string->value (schema-item string)
  (:documentation
   "Parse STRING and return a value object taking into properties of
   SCHEMA-ITEM."))

(defgeneric value->string-using-type (schema-item value type &key inner-type)
  (:documentation
   "Like `value->string' but may incorporate TYPE, besides
    SCHEMA-ITEM, into the conversion of VALUE into a string
    representation."))

(defgeneric string->value-using-type (schema-item string type &key inner-type)
  (:documentation
   "Like `string->value' but may incorporate TYPE, besides
    SCHEMA-ITEM, into the parsing of STRING into a value object."))

;; Default behavior

(defmethod validate-value :around ((schema-item t)
                                   (value       t)
                                   &key
                                   (if-invalid #'error))
  (labels ((handle-invalid (&optional cause)
             (error-behavior-restart-case
                 (if-invalid
                  (option-value-error
                   :option schema-item
                   :value  value
                   :cause  cause))
               (retry ()
                 (recur))
               (use-value (value)
                 value)))
           (recur ()
             (or (handler-bind
                     (((or simple-error option-value-error)
                        #'handle-invalid))
                   (call-next-method))
                 (handle-invalid))))
    (recur)))

(defmethod string->value :around ((schema-item t) (string t))
  (with-condition-translation (((error option-syntax-error)
                                :option schema-item
                                :value  string))
    (call-next-method)))

(defmethod string->value-using-type :around ((schema-item t) (string t) (type t)
                                             &key inner-type)
  (with-condition-translation (((error option-syntax-error)
                                :option schema-item
                                :value  string
                                :type   (if inner-type
                                            (list* type inner-type)
                                            type)))
    (call-next-method)))

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

;;; Sink protocol

(defgeneric notify (sink event name value
                    &key
                    raw?
                    source
                    &allow-other-keys)
  (:documentation
   "SINK is notified about some change regarding the option named
    NAME.

    EVENT can be, for example, :added, :removed, :new-value. For these
    three, the remaining parameters are interpreted as follows:

    EVENT      NAME        VALUE
    :added     OPTION-NAME SHOULD-BE-IGNORED
    :removed   OPTION-NAME SHOULD-BE-IGNORED
    :new-value OPTION-NAME [RAW-]NEW-VALUE

    RAW? indicates whether VALUE is an unparsed string value or
    whether it has already been parsed.

    The value of the keyword parameter SOURCE usually is the source
    object that produced the event, but may be nil."))

;; Default behavior

(defmethod notify :around ((sink t) (event t) (name t) (value t)
                           &key
                           source
                           &allow-other-keys)
  (restart-case
      (with-condition-translation
          (((error notification-error)
            :sink   sink
            :event  event
            :name   name
            :value  value
            :source source))
        (call-next-method))
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Skip notifying ~A of this ~
                                particular event.~@:>"
                        sink))
      (declare (ignore condition)))))
