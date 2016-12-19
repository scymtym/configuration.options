;;;; source.lisp --- Source for commandline options.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.commandline)

(defclass commandline-source ()
  ((prefix    :initarg  :prefix
              :type     (or null string)
              :reader   source-prefix
              :initform nil
              :documentation
              "A string which is prepended to option names to form
               commandline option names.")
   (arguments :initarg  :arguments
              :type     (or t list)
              :reader   source-arguments
              :initform t
              :documentation
              "Either t or the list of commandline arguments that
               should be processed by the source. If t, the \"real\"
               commandline arguments are used.")
   ;; The following slots are implementation details.
   (context   :reader   source-context
              :accessor source-%context
              :documentation
              "Stores the constructed context.")
   (synopsis  :accessor source-%synopsis
              :documentation
              "Stores the constructed synopsis.")
   (mapping   :initarg  :mapping
              :type     list
              :accessor source-%mapping
              :initform '()
              :documentation
              "Stores a mapping of the form

                 (OPTION . NAME)

               where OPTION is a commandline option and NAME is an
               option name."))
  (:documentation
   "This source obtains option values from commandline arguments.

    It generates commandline options specifications based on a given
    schema and extracts option values from commandline arguments."))

(service-provider:register-provider/class
 'configuration.options.sources::source :commandline
 :class 'commandline-source)

(defmethod initialize ((source commandline-source) (schema t))
  (let+ (((&structure
           source- arguments (context %context) (synopsis %synopsis))
          source)
         (global? t) ; TODO
         (seen-wild? nil)
         ((&labels do-option (schema-item &optional (prefix '()))
            ;; Cannot create concrete commandline options for wildcard
            ;; schema items. Return a textual description of the
            ;; option instead.
            (when (typep (option-name schema-item) 'wild-name)
              (setf seen-wild? t)
              (return-from do-option
                (list (net.didierverna.clon:make-text
                       :contents (format nil "  --~@[~A~]~{~A~^-~}~@[ ~A~]"
                                         (source-prefix source)
                                         (name-components
                                          (option-name schema-item)) ; TODO prefix
                                         (option-documentation schema-item))))))

            ;; Non-wildcard schema item => create an appropriate
            ;; command option.
            (let+ (((&structure-r/o option- name type documentation)
                    schema-item)
                   (name-and-prefix (merge-names prefix name))
                   ((&values default default?)
                    (option-default schema-item :if-does-not-exist nil))
                   ((&values constructor initargs)
                    (make-option-using-type source schema-item type))
                   (long-name (format nil "~@[~A~]~{~A~^-~}"
                                      (source-prefix source)
                                      (name-components name-and-prefix)))
                   (option (apply constructor
                                  :long-name long-name
                                  (append
                                   (when default?
                                     (list :default-value default))
                                   (when documentation
                                     (list :description documentation))
                                   initargs))))
              (push (cons option name-and-prefix) (source-%mapping source))
              (list option))))
         ((&labels+ do-schema ((name . schema) prefix)
            ;; TODO check name = () <=> prefix = ()?
            (let* ((name-and-prefix (merge-names prefix name))
                   (root?           (not name))
                   (options         (mapcan (rcurry #'do-option name-and-prefix)
                                            (options schema))))
              (when (or options root?)
                (list
                 (apply #'net.didierverna.clon:make-group
                        :header (or (option-documentation schema)
                                    "Accepted options")
                        (mappend
                         (curry #'list :item)
                         (append
                          ;; TODO think about this. probably better to
                          ;; let clients decide whether they want
                          ;; --help or not
                          #+no (when root?
                                 (list (net.didierverna.clon:make-flag :long-name "help")))
                          options
                          (mapcan (rcurry #'do-schema name-and-prefix)
                                  (configuration.options::schema-children/alist schema)))))))))))

    ;; Create synopsis and context based on SCHEMA. Wildcard options
    ;; may required use of "postfix" syntax.
    (setf synopsis (net.didierverna.clon:make-synopsis
                    :item         (first (do-schema (cons '() schema) '()))
                    :postfix      (when seen-wild? "[WILDCARD-OPTIONS]")
                    :make-default global?)
          context  (apply #'net.didierverna.clon:make-context
                          :synopsis     synopsis
                          :make-current global?
                          (unless (eq arguments t)
                            (list :cmdline (list* "prog"
                                                  "--clon-error-handler=none"
                                                  arguments)))))
    (values)))

(defgeneric make-option-using-type (source option type &key inner-type)
  (:documentation
   "Make and return a suitable clon commandline option for OPTION,
    TYPE and potentially INNER-TYPE."))

(macrolet
    ((define-make-option-using-type-method (specializer &body body)
       `(defmethod make-option-using-type ((source commandline-source)
                                           (option t)
                                           (type   ,specializer)
                                           &key inner-type)
          (declare (ignorable inner-type))
          ,@body)))

  (define-make-option-using-type-method cons
    (make-option-using-type
     source option (first type)
     :inner-type (append (rest type) (ensure-list inner-type))))

  (define-make-option-using-type-method (eql 'member)
    (values #'net.didierverna.clon:make-enum (list :enum inner-type)))

  (define-make-option-using-type-method (eql 'boolean)
    #'net.didierverna.clon:make-switch)

  (define-make-option-using-type-method (eql 'string)
    #'net.didierverna.clon:make-stropt)

  (define-make-option-using-type-method (eql 'pathname)
    (values #'net.didierverna.clon:make-path (list :type :file #+TODO-maybe :directory)))

  (define-make-option-using-type-method (eql 'list)
    (typecase inner-type
      ((cons (eql pathname))
       (values #'net.didierverna.clon:make-path (list :type nil #+TODO-maybe :directory-list #+or :file-list)))
      (t
       (call-next-method source option type :inner-type nil))))

  (define-make-option-using-type-method t
    (values #'net.didierverna.clon:make-lispobj
            (list :typespec (if inner-type
                                (list* type inner-type)
                                type)))))

(defmethod process ((source commandline-source) (sink t))
  ;; Obtain configuration options from commandline options.
  (let+ (((&structure-r/o source- prefix context (mapping %mapping)) source)
         ((&flet notify (name value option &key (raw? nil))
            (notify sink :added     name nil
                    :source source :option option)
            (notify sink :new-value name value
                    :source source :option option :raw? raw?))))
    ;; Try to retrieve all options defined in the schema. For options
    ;; which are actually present, notify SINK.
    (iter (for (option . name) in mapping)
          (restart-case
              (let+ (((&values value source)
                      (net.didierverna.clon:getopt :option  option
                                               :context context)))
                (when (and source (not (eq source :default)))
                  (log:info "~@<Identified commandline option ~
                             ~/configuration.options:print-name/ -> ~A ~
                             with value ~S.~@:>"
                            name option value)
                  (notify name value option)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip option ~A.~@:>" option))
              (declare (ignore condition)))))

    ;; Analyze remainder to find commandline arguments corresponding
    ;; to wildcard commandline options.
    (let* ((prefix/dashes (concatenate 'string "--" prefix))
           (strip-start   (length prefix/dashes)))
      (iter (generate option in           (net.didierverna.clon:remainder
                                           :context context))
            (for      name-and-value next (next option))
            (restart-case
                (let+ (
                       (index)
                       ((&values name value)
                        (cond
                          ((not (starts-with-subseq
                                 prefix/dashes name-and-value))
                           nil)
                          ((setf index (position #\= name-and-value))
                           (values (subseq name-and-value strip-start index)
                                   (subseq name-and-value (1+ index))))
                          (t
                           (values (subseq name-and-value strip-start)
                                   (next option)))))
                       (name/parsed (when name
                                      (make-name (split-sequence #\- name)))))

                  (when name/parsed
                    (log:info "~@<Identified positional option ~
                               ~/configuration.options:print-name/ -> ~
                               ~A with value ~S.~@:>"
                              name/parsed name value) ; TODO also report unrecognized options?
                    (notify name/parsed value name :raw? t)))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Skip positional option ~S.~@:>" name-and-value))
                (declare (ignore condition))))))))
