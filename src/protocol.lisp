;;;; protocol.lisp --- Protocol provided by the options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; Name protocol
;;;
;;; Names are generally similar to pathnames: a sequence of name
;;; components which are either strings or one of the symbols :wild
;;; and :wild-inferiors.

(defgeneric make-name (thing)
  (:documentation
   "Return a name corresponding to THING which can be a name string, a
    sequence of `name-component's or a name.

    The second return value is true if something other than
    THING (i.e. the result of coercing THING to a name) is returned
    and false if THING is returned."))

(defgeneric name-components (name)
  (:documentation
   "Return a sequence of the components of NAME."))

(defgeneric name-equal (left right &key start1 end1 start2 end2)
  (:documentation
   "Return non-nil when LEFT and RIGHT are equal.

    In case LEFT is not equal to RIGHT, return two values: 1) nil 2)
    the position of the mismatch in LEFT.

    START1 and END1, if supplied, select a subseqeuence of LEFT to be
    used in the equality test.

    START2 and END2, if supplied, select a subseqeuence of RIGHT to be
    used in the equality test."))

(defgeneric name-matches (query name &key start1 end1 start2 end2)
  (:documentation
   "Return non-nil when QUERY matches NAME.

    This can be the case either when QUERY and NAME are equal or when
    QUERY contains :wild or :wild-inferiors components matching
    components of NAME.

    START1 and END1, if supplied, select a subseqeuence of QUERY to be
    used in the matching computation.

    START2 and END2, if supplied, select a subseqeuence of NAME to be
    used in the matching computation."))

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

(defmethod name-equal ((left t) (right t) &key start1 end1 start2 end2)
  (if-let ((position (mismatch (name-components left) (name-components right)
                               :test   #'equal
                               :start1 (or start1 0) :end1 end1
                               :start2 (or start2 0) :end2 end2)))
    (values nil position)
    t))

(defmethod name-matches :around ((query t) (name t) &key start1 end1 start2 end2)
  (flet ((check-bounding-indices (sequence start end)
           (let* ((length (length sequence))
                  (start  (or start 0))
                  (end    (or end length)))
             (unless (<= 0 start end length)
               (error 'simple-type-error
                      :datum            (cons start end)
                      :expected-type    `(cons (integer 0 ,length)
                                               (integer 0 ,length))
                      :format-control   "~@<The bounding indices ~S and ~S ~
                                          are bad for a sequence of ~
                                          length ~D~@:>."
                      :format-arguments (list start end length))))))
    (check-bounding-indices query start1 end1)
    (check-bounding-indices name  start2 end2)
    (call-next-method)))

(defmethod name-matches ((query t) (name t) &key start1 end1 start2 end2)
  (name-equal query name :start1 start1 :end1 end1 :start2 start2 :end2 end2))

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
  #+sbcl (concatenate (class-of left) left right)
  #-sbcl (append (name-components left) (name-components right)))

(defmethod merge-names ((left (eql nil)) (right t))
  right)

;;; Value protocol

(defgeneric value (option-or-name
                   &key
                   configuration
                   if-does-not-exist
                   if-no-value)
  (:documentation
   "Return the value of OPTION-OR-NAME in CONFIGURATION.

    OPTION-OR-NAME is either an option object or an option name
    designating an option in CONFIGURATION.

    If CONFIGURATION is not supplied, the value of *CONFIGURATION* is
    used. An error is signaled if CONFIGURATION is null.

    IF-DOES-NOT-EXIST controls the behavior in case OPTION-OR-NAME is
    an option name (not an option object) and does not designate an
    option in CONFIGURATION. For a description of return values,
    signaled conditions and IF-DOES-NOT-EXIST, see `find-option'.

    IF-NO-VALUE controls the behavior in case OPTION-OR-NAME does not
    have a value. For a description of returns values, signaled
    conditions and IF-NO-VALUE, see IF-DOES-NOT-EXIST in the
    description of `option-value'."))

(defgeneric (setf value) (new-value option-or-name
                          &key
                          configuration
                          if-does-not-exist
                          if-no-value)
  (:documentation
   "Set value of OPTION-OR-NAME in CONFIGURATION to NEW-VALUE.

    OPTION-OR-NAME is either an option object or an option name
    designating an option in CONFIGURATION.

    If CONFIGURATION is not supplied, the value of *CONFIGURATION* is
    used. An error is signaled if CONFIGURATION is null.

    IF-DOES-NOT-EXIST controls the behavior in case OPTION-OR-NAME is
    an option name (not an option object) and does not designate an
    option in CONFIGURATION. For a description of return values,
    signaled conditions and IF-DOES-NOT-EXIST, see `find-option'.

    IF-NO-VALUE is accepted for parity with `value'."))

;; Default behavior

(flet ((check-configuration (context configuration)
         (unless configuration
           (error "~@<No configuration object supplied to ~S. Is ~S ~
                   not bound to a configuration object?~@:>"
                  context '*configuration*))))

;; Name coercion

  (defmethod value :around ((option-or-name sequence)
                            &key
                            (configuration     *configuration*)
                            (if-does-not-exist #'error)
                            (if-no-value       if-does-not-exist))
    (check-configuration 'value configuration)

    (let ((option (find-option option-or-name configuration
                               :if-does-not-exist if-does-not-exist)))
      (if (eq option if-does-not-exist)
          option
          (value option
                 :configuration configuration
                 :if-no-value   if-no-value))))

  (defmethod (setf value) :around ((new-value      t)
                                   (option-or-name sequence)
                                   &key
                                   (configuration     *configuration*)
                                   (if-does-not-exist #'error)
                                   if-no-value)
    (declare (ignore if-no-value))
    (check-configuration '(setf value) configuration)

    (let ((option (find-option option-or-name configuration
                               :if-does-not-exist if-does-not-exist)))
      (if (eq option if-does-not-exist)
          option
          (setf (value option :configuration configuration)
                new-value))))

;; Default behavior

  (defmethod value ((option-or-name t)
                    &key
                    configuration
                    if-does-not-exist
                    (if-no-value      #'error))
    (declare (ignore configuration if-does-not-exist))
    (option-value option-or-name :if-does-not-exist if-no-value))

  (defmethod (setf value) ((new-value      t)
                           (option-or-name t)
                           &key
                           configuration
                           if-does-not-exist
                           if-no-value)
    (declare (ignore configuration if-does-not-exist if-no-value))
    (setf (option-value option-or-name) new-value))

) ; flet

;;; Event hook protocol
;;;
;;; Objects with event hooks emit events for handlers with
;;; lambda-lists of the form
;;;
;;;   (EVENT NAME VALUE &rest PROPERTIES &key)

(defgeneric event-hook (object)
  (:documentation
   "Return the event hook, a `hooks:object-hook', for OBJECT."))

;;; Option container protocol
;;;
;;; CONTAINER can be (at least) a schema or a configuration.

(defgeneric options (container)
  (:documentation
   "Return a sequence of the options contained in CONTAINER."))

(defgeneric map-options (function container)
  (:documentation
   "Call FUNCTION for each option in CONTAINER.

    FUNCTION is called with at least one argument: the option. Keyword
    arguments may follow.

    If CONTAINER is a schema object, FUNCTION is called with the
    following keyword arguments:

    :prefix

      Specifies the option name prefix of the child container in which
      the current option is contained (the prefix is empty for options
      contained in CONTAINER itself).

    :container

      Specifies the container in which the current option
      resides (either CONTAINER or child containers thereof)."))

(defgeneric map-matching-options (function query container
                                  &key
                                  interpret-wildcards?)
  (:documentation
   "Call FUNCTION for each option matching QUERY in CONTAINER.

    See `map-options' for a description of FUNCTION.

    QUERY has to be a (potentially wildcard) name against which
    options in CONTAINER are matched.

    INTERPRET-WILDCARDS? controls whether and how wild components in
    QUERY and in names of items in CONTAINER should be
    interpreted. The following values are accepted:

    nil

      Wildcards in neither QUERY nor CONTAINER are interpreted. That
      is, names only match if their components, including wildcard
      components are identical.

    :query

      Wildcards in QUERY are interpreted, allowing arbitrary name
      components in corresponding positions in names in CONTAINER to
      match.

    :container

      Wildcards in names in CONTAINER are interpreted, allowing
      arbitrary name components in corresponding positions in QUERY to
      match."))

(defgeneric find-options (query container)
  (:documentation
   "Find and return a sequence of options in CONTAINER matching QUERY
    which can be a name with wildcard components.

    Matching options can appear in any order in the returned
    sequence.

    If CONTAINER has child containers (as can be the case for schema
    objects), matching options in ancestor containers (i.e. transitive
    children) are also found and returned."))

(defgeneric find-option (name container
                         &key
                         if-does-not-exist
                         if-exists)
  (:documentation
   "Find and return the option named NAME in CONTAINER.

    IF-DOES-NOT-EXIST controls the behavior in case there is no option
    named NAME:

    nil

      Return nil.

    other value (but not one of the following)

      Return IF-DOES-NOT-EXIST.

    'warn, #'warn

      Signal a `option-missing-warning' indicating that an option
      named NAME does not exist in CONTAINER and return nil.

    'error, #'error

      Signal an `option-missing-error' indicating that an option named
      NAME does not exist in CONTAINER.

    a function

      Call the function with an `option-missing-error' instance
      indicating that an option named NAME does not exist in
      CONTAINER.

    IF-EXISTS is accepted for parity with `(setf find-option)'."))

(defgeneric (setf find-option) (new-value name container
                                &key
                                if-does-not-exist
                                if-exists)
  (:documentation
   "Store the option NEW-VALUE under the name NAME in container.

    IF-DOES-NOT-EXIST acts similarly to what is described for
    `find-option' w.r.t. signaling conditions, but does not influence
    the return value.

    IF-EXISTS controls the behavior in case an option named NAME is
    already stored in CONTAINER:

    :supersede

      Replace the existing option named NAME in container with
      NEW-VALUE.

    :keep

      Do not store NEW-VALUE and keep the option currently associated
      to NAME in CONTAINER.

    'warn, #'warn

      Signal a `option-exists-warning' indicating that an option named
      NAME already exists in CONTAINER.

    'error, #'error

      Signal an `option-exists-error' indicating that an option named
      NAME already exists in CONTAINER.

    a function

      Call the function with an `option-exists-error' instance
      indicating that an option named NAME already exists in
      CONTAINER."))

;; when using (setf find-option) to add an option to a configuration,
;; consult the schema to check whether the option is valid:
;; * options are allowed in that particular subtree
;; * type is correct
;; * value is of correct type

;; Name coercion

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro if-name (var then else)
    (check-type var symbol)
    (with-gensyms (changed?)
      `(let+ (((&values ,var ,changed?) (make-name ,var)))
         (if ,changed? ,else ,then)))))

(defmethod map-matching-options :around ((function  function)
                                         (query     sequence)
                                         (container t)
                                         &rest args &key &allow-other-keys)
  (if-name query
    (call-next-method)
    (apply #'map-matching-options function query container args)))

(defmethod find-options :around ((query sequence) (container t))
  (if-name query
    (call-next-method)
    (find-options query container)))

(defmethod find-option :around ((name sequence) (container t)
                                &rest args &key &allow-other-keys)
  (if-name name
    (call-next-method)
    (apply #'find-option name container args)))

(defmethod (setf find-option) :around ((new-value t)
                                       (name      sequence)
                                       (container t)
                                       &rest args &key &allow-other-keys)
  (if-name name
    (call-next-method)
    (apply #'(setf find-option) new-value name container args)))

;; Default behavior

(defmethod map-options ((function t) (container t))
  (map-options (ensure-function function) container))

(defmethod map-matching-options ((function t) (query t) (container t)
                                 &rest args &key
                                 interpret-wildcards?)
  (declare (ignore interpret-wildcards?))
  (apply #'map-matching-options
         (ensure-function function) query container args))

(defmethod map-matching-options ((function function) (query t) (container t)
                                 &key
                                 (interpret-wildcards? :query))
  (check-type interpret-wildcards? wildcard-interpretation)
  (map-options
   (named-lambda match (option &rest args &key (prefix '()) &allow-other-keys)
     (let+ ((name (option-name option))
            ((&flet match-rest (offset)
               (when (case interpret-wildcards?
                       ((nil)      (name-equal query name :start1 offset))
                       (:query     (name-matches query name :start1 offset))
                       (:container (name-matches name query :start2 offset)))
                 (apply function option args)
                 (return-from match)))))
       (cond
         ((null prefix)
          (match-rest 0))
         ((eq interpret-wildcards? nil)
          (match-rest (or (nth-value
                           1 (name-equal query prefix :end1 (length prefix)))
                          (length prefix))))
         ((eq interpret-wildcards? :query)
          (map-query-alignments
           (lambda (total? end1 end2)
             (declare (ignore total?))
             (when (= end2 (length prefix))
               (match-rest end1)))
           (name-components query)  0 (length query)
           (name-components prefix) 0 (length prefix)))
         (t ; implies (eq interpret-wildcards? :container)
          (map-query-alignments
           (lambda (total? end1 end2)
             (declare (ignore total?))
             (when (= end1 (length prefix))
               (match-rest end2)))
           (name-components prefix) 0 (length prefix)
           (name-components query)  0 (length query))))))
   container))

(defmethod find-options ((query t) (container t))
  (let ((result '()))
    (map-options (lambda (option &key (prefix '()) &allow-other-keys)
                   (let ((name (merge-names prefix (option-name option))))
                     (when (name-matches query name)
                       (push option result))))
                 container)
    result))

(defmethod find-option :around ((name t) (container t)
                                &key
                                (if-does-not-exist #'error)
                                &allow-other-keys)
  (tagbody
   :start
     (return-from find-option
       (or (call-next-method)
           (error-behavior-restart-case
               (if-does-not-exist
                (option-missing-error
                 :name      name
                 :container container)
                :warning-condition option-missing-warning)
             (retry ()
               :report (lambda (stream)
                         (format stream "~@<Retry finding an item ~
                                         named ~A in ~A.~@:>"
                                 name container))
               (go :start))
             (use-value (value)
               :report (lambda (stream)
                         (format stream "~@<Use a particular value ~
                                         instead of the missing item ~
                                         named ~A in ~A.~@:>"
                                 name container))
               value))))))

(defmethod (setf find-option) :around ((new-value (eql nil))
                                       (name      t)
                                       (container t)
                                       &key
                                       if-does-not-exist
                                       (if-exists        :supersede))
  ;; For IF-DOES-NOT-EXIST, on the values NIL, #'error and #'warn are
  ;; interesting since we return NEW-VALUE in any case (unless we
  ;; signal an error).
  (when if-does-not-exist
    (find-option name container :if-does-not-exist if-does-not-exist))
  (call-next-method new-value name container :if-exists if-exists))

(defmethod (setf find-option) :around ((new-value t)
                                       (name      t)
                                       (container t)
                                       &key
                                       if-does-not-exist
                                       (if-exists        #'error))
  (declare (ignore if-does-not-exist))
  (let (existing)
    (cond
      ((eq if-exists :supersede)
       (call-next-method))
      ((not (setf existing (find-option name container
                                        :if-does-not-exist nil)))
       (call-next-method))
      ((eq if-exists :keep)
       existing)
      (t
       (error-behavior-restart-case
           (if-exists (option-exists-error
                       :name      name
                       :existing  existing
                       :container container)
                      :warning-condition option-exists-warning)
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Replace existing option ~A ~
                                     with ~A.~@:>"
                             existing new-value))
           (declare (ignore condition))
           (call-next-method))
         (keep ()
           :report (lambda (stream)
                     (format stream "~@<Keep existing option ~A.~@:>"
                             existing))))))))

;;; Schema protocol

(defgeneric schema-children (schema)
  (:documentation
   "Return a sequence of the schema items contained in SCHEMA."))

(defgeneric find-child (name schema
                        &key
                        if-does-not-exist
                        if-exists)
  (:documentation
   "Find and return the child schema stored under NAME in SCHEMA.

    IF-DOES-NOT-EXIST controls the behavior in case there is no child
    schema named NAME in SCHEMA:

    nil

      Return nil.

    other value (but not one of the following)

      Return IF-DOES-NOT-EXIST.

    'warn, #'warn

      Signal a `child-missing-warning' indicating that a child named
      NAME does not exist in SCHEMA and return nil.

    'error, #'error

      Signal a `child-missing-error' indicating that a child named
      NAME does not exist in SCHEMA.

    a function

      Call the function with a `child-missing-error' instance
      indicating that a child NAME NAME does not exist in SCHEMA.

    IF-EXISTS is accepted for parity with `(setf find-child)'."))

(defgeneric (setf find-child) (new-value name schema
                               &key
                               if-does-not-exist
                               if-exists)
  (:documentation
   "Store the child schema NEW-VALUE under the name NAME in SCHEMA.

    IF-DOES-NOT-EXIST acts similarly to what is described for
    `find-child' w.r.t. signaling conditions, but does not influence
    the return value.

    IF-EXISTS controls the behavior in case a child schema name is
    already stored in SCHEMA:

    :supersede

      Replace the existing child named NAME in SCHEMA with NEW-VALUE.

    :keep

      Do not store NEW-VALUE and keep the child currently associated
      to NAME in SCHEMA.

    'warn, #'warn

      Signal a `child-exists-warning' indicating that a child named
      NAME already exists in SCHEMA.

    'error, #'error

      Signal a `child-exists-error' indicating that a child named NAME
      already exists in SCHEMA.

    a function

      Call the function with a `child-exists-error' indicating that a
      child named NAME already exists in SCHEMA."))

(defgeneric make-configuration (schema)
  (:documentation
   "Make and return a configuration object the option objects in which
    comply to schema."))

;; Name coercion

(defmethod find-child :around ((name sequence) (schema t)
                               &rest args &key &allow-other-keys)
  (if-name name
    (call-next-method)
    (apply #'find-child name schema args)))

(defmethod (setf find-child) :around ((new-value t) (name sequence) (schema t)
                                       &rest args &key &allow-other-keys)
  (if-name name
    (call-next-method)
    (apply #'(setf find-child) new-value name schema args)))

;; Default behavior

(defmethod find-child :around ((name t) (schema t)
                               &key
                               (if-does-not-exist #'error)
                               &allow-other-keys)
  (tagbody
   :start
     (return-from find-child
       (or (call-next-method)
           (error-behavior-restart-case
               (if-does-not-exist
                (child-missing-error
                 :name      name
                 :container schema)
                :warning-condition child-missing-warning)
             (retry ()
               :report (lambda (stream)
                         (format stream "~@<Retry finding a child ~
                                         named ~A in ~A.~@:>"
                                 name schema))
               (go :start))
             (use-value (value)
               :report (lambda (stream)
                         (format stream "~@<Use a particular value ~
                                         instead of the missing child ~
                                         named ~A in ~A.~@:>"
                                 name schema))
               value))))))

(defmethod (setf find-child) :around ((new-value (eql nil))
                                      (name      t)
                                      (schema    t)
                                      &key
                                      if-does-not-exist
                                      (if-exists        :supersede))
  ;; For IF-DOES-NOT-EXIST, only the values nil, #'error and #'warn
  ;; are interesting since we return NEW-VALUE in any case (unless we
  ;; signal an error).
  (when if-does-not-exist
    (find-child name schema :if-does-not-exist if-does-not-exist))
  (call-next-method new-value name schema :if-exists if-exists))

(defmethod (setf find-child) :around ((new-value t)
                                      (name      t)
                                      (schema    t)
                                      &key
                                      if-does-not-exist
                                      (if-exists        #'error))
  (declare (ignore if-does-not-exist))
  (let (existing)
    (cond
      ((eq if-exists :supersede)
       (call-next-method))
      ((not (setf existing (find-child name schema
                                       :if-does-not-exist nil)))
       (call-next-method))
      ((eq if-exists :keep)
       existing)
      (t
       (error-behavior-restart-case
           (if-exists (child-exists-error
                       :name      name
                       :existing  existing
                       :container schema)
                      :warning-condition child-exists-warning)
         (continue (&optional condition)
           :report (lambda (stream)
                     (format stream "~@<Replace existing child ~A with ~
                                     ~A.~@:>"
                             existing new-value))
           (declare (ignore condition))
           (call-next-method))
         (keep ()
           :report (lambda (stream)
                     (format stream "~@<Keep existing child ~A.~@:>"
                             existing))))))))

;;; Configuration protocol

(defgeneric configuration-schema (configuration)
  (:documentation
   "Return the schema object governing CONFIGURATION."))

;;; Option-like protocol
;;;
;;; OPTION can be (at least) an option or a schema items.

(defgeneric option-name (option)
  (:documentation
   "Return the name object naming OPTION."))

(defgeneric option-type (option)
  (:documentation
   "Return the type of OPTION. The returned type is an expression
    similar to a CL type."))

(defgeneric option-default (option &key if-does-not-exist)
  (:documentation
   "Return two values describing the default value of OPTION: 1) nil
    or the default value of OPTION 2) nil if OPTION does not have a
    default value and t if OPTION has a default value.

    IF-DOES-NOT-EXIST controls the behavior in case OPTION does not
    have default value:

    nil

      Return the two values nil, nil.

    other value (but not one of the following)

      Return the two values IF-DOES-NOT-EXIST, nil.

    'warn, #'warn

      Signal a `value-missing-warning' indicating that OPTION does not
      have a default value and return the two values nil, nil.

    'error, #'error

      Signal a `value-missing-error' indicating that OPTION does not
      have a default value.

    a function

      Call the function with a `value-missing-error' instance
      indicating that OPTION does not have a default value."))

(defgeneric option-documentation (option)
  (:documentation
   "Return nil or the documentation string of OPTION."))

;; Default behavior

(defmethod option-default :around ((option t)
                                   &key
                                   (if-does-not-exist #'error))
  (tagbody
   :start
     (let+ (((&values default default?) (call-next-method)))
       (when default?
         (return-from option-default (values default default?))))
     (return-from option-default
       (error-behavior-restart-case
           (if-does-not-exist
            (value-missing-error
             :option option
             :which :default)
            :warning-condition value-missing-warning)
         (retry ()
           :report (lambda (stream)
                     (format stream "~@<Retry obtaining the default ~
                                     value of ~A.~@:>"
                             option))
           (go :start))
         (use-value (value)
           :report (lambda (stream)
                     (format stream "~@<Use a particular value ~
                                     instead of the missing default ~
                                     value of ~A.~@:>"
                             option))
           value)))))

;;; Schema item protocol

(defgeneric option-class (schema-item)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric make-option (schema-item name)
  (:documentation
   "Make and return an option object according to SCHEMA-ITEM and
    NAME.

    The new option will be named NAME and point to a new option cell
    which will in turn point to SCHEMA-ITEM for type, default,
    documentation, etc."))

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

;; Name coercion

(defmethod make-option :around ((schema-item t) (name sequence))
  (if-name name
    (call-next-method)
    (make-option schema-item name)))

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
               (continue (&optional condition)
                 :report (lambda (stream)
                           (format stream "~@<Ignore the problem.~@:>"))
                 (declare (ignore condition))
                 t))))
    (or (handler-bind
            (((or simple-error option-value-error)
               #'handle-invalid))
          (call-next-method))
        (handle-invalid))))

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
   "Return the configuration object containing OPTION."))

(defgeneric option-schema-item (option)
  (:documentation
   "Return the schema item corresponding to OPTION."))

(defgeneric option-value (option
                          &key
                          if-does-not-exist)
  (:documentation
   "Return up to three values describing the value of OPTION:
    1) nil or the value of OPTION
    2) nil if OPTION does not have a value and t if OPTION has a value
    3) if OPTION has a value and a single source provided it, that
       source.

    IF-DOES-NOT-EXIST controls the behavior in case OPTION does not
    have value:

    nil

      Return the two values nil, nil.

    other value (but not one of the following)

      Return the two values IF-DOES-NOT-EXIST, nil.

    'warn, #'warn

      Signal a `value-missing-warning' indicating that OPTION does not
      have a default value and return the two values nil, nil.

    'error, #'error

      Signal a `value-missing-error' indicating that OPTION does not
      have a value.

    a function

      Call the function with a `value-missing-error' instance
      indicating that OPTION does not have a value."))

(defgeneric (setf option-value) (new-value option
                                 &key
                                 if-does-not-exist
                                 if-invalid)
  (:documentation
   "Set the current value of OPTION to NEW-VALUE.

    IF-DOES-NOT-EXIST is accepted for parity with `option-value'.

    IF-INVALID controls the behavior in case NEW-VALUE is not a valid
    value for OPTION. See the description of the if-invalid keyword
    parameter of the `validate-value' generic function."))

(defgeneric option-values (option)
  (:documentation
   "Return the (potentially empty) sequence of values from which the
    effective value of OPTION has been constructed via merging.

    Entries are of the form

      (VALUE &rest PLIST)

    where VALUE is a parsed value and PLIST contains at least the
    property :source holding the source object from which VALUE
    originated. Additional properties may describe the origin of VALUE
    in more detail."))

;; Default behavior

(defmethod option-value :around ((option t)
                                 &key
                                 (if-does-not-exist #'error))
  (tagbody
   :start
     (let+ (((&values value value? source) (call-next-method)))
       (when value?
         (return-from option-value (values value value? source))))
     (return-from option-value
       (error-behavior-restart-case
           (if-does-not-exist
            (value-missing-error
             :option option
             :which  :value)
            :warning-condition value-missing-warning)
         (retry ()
           :report (lambda (stream)
                     (format stream "~@<Retry obtaining the value of ~
                                     ~A.~@:>"
                             option))
           (go :start))
         (use-value (value)
           :report (lambda (stream)
                     (format stream "~@<Use a particular value instead ~
                                     of the missing value of ~
                                     ~A.~@:>"
                             option))
           (values value t 'use-value))))))

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
