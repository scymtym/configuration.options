;;;; source-commandline.lisp --- Source for commandline options.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

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
   "This sources generates commandline options specifications based on
    a given schema and extracts option values from commandline
    arguments."))

(service-provider:register-provider/class
 'source :commandline :class 'commandline-source)

(defmethod initialize ((source commandline-source) (schema t))
  (let+ (((&accessors (arguments source-arguments)
                      (context   source-%context)
                      (synopsis  source-%synopsis)) source)
         (global? t) ; TODO
         (seen-wild? nil)
         ((&labels do-option (schema-item)
            ;; Cannot create concrete commandline options for wildcard
            ;; schema items. Return a textual description of the
            ;; option instead.
            (when (typep (option-name schema-item) 'options::wild-name)
              (setf seen-wild? t)
              (return-from do-option
                (list (net.didierverna.clon:make-text
                       :contents (format nil "~@[~A~]~{~A~^-~}"
                                         (source-prefix source)
                                         (name-components
                                          (option-name schema-item)))))))

            ;; Non-wildcard schema item => create an appropriate
            ;; command option.
            (let+ (((&accessors-r/o (name          option-name)
                                    (type          option-type)
                                    (documentation option-documentation))
                    schema-item)
                   ((&values default default?)
                    (option-default schema-item :if-does-not-exist nil))
                   ((&values constructor initargs)
                    (make-option-using-type source schema-item type))
                   (long-name (format nil "~@[~A~]~{~A~^-~}"
                                      (source-prefix source)
                                      (name-components name)))
                   (option (apply constructor
                                  :long-name long-name
                                  (append
                                   (when default?
                                     (list :default-value default))
                                   (when documentation
                                     (list :description documentation))
                                   initargs))))
              (push (cons option name) (source-%mapping source))
              (list option))))
         ((&labels do-schema (schema &optional prefix)
            (let ((root?   (not prefix))
                  (options (mapcan #'do-option (options schema))))
              (when (or options root?)
                (list
                 (apply #'net.didierverna.clon:make-group
                        :header (or (option-documentation schema)
                                    "Accepted options")
                        (mappend
                         (curry #'list :item)
                         (append
                          (when root?
                            (list (net.didierverna.clon:make-flag :long-name "help")))
                          options
                          (mapcan (rcurry #'do-schema '("foo")) ; TODO
                                  (schema-children schema)))))))))))

    ;; Create synopsis and context based on SCHEMA. Wildcard options
    ;; may required use of "postfix" syntax.
    (setf synopsis (net.didierverna.clon:make-synopsis
                    :item         (first (do-schema schema))
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
    (values #'net.didierverna.clon:make-path (list :type :file)))
  (define-make-option-using-type-method t
    (values #'net.didierverna.clon:make-lispobj
            (list :typespec (list* type inner-type)))))

(defmethod process ((source commandline-source)
                    (sink   t))
  "Obtain configuration options from environment variables."
  (let+ (((&accessors-r/o (prefix  source-prefix)
                          (context source-context)
                          (mapping source-%mapping)) source)
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
                  (notify name value option)))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip option ~A.~@:>"
                                option))
              (declare (ignore condition)))))

    ;; Analyze remainder to find commandline arguments corresponding
    ;; to wildcard commandline options.
    (let* ((prefix/dashes (concatenate 'string "--" prefix))
           (strip-start   (length prefix/dashes)))
      (iter (generate option in (net.didierverna.clon:remainder
                                 :context context))
            (let+ ((name-and-value (next option))
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
                (notify name/parsed value name :raw? t)))))))
