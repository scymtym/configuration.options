;;;; syntax-ini.lisp --- Interpret configuration information in "ini" syntax.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defclass ini-syntax ()
  ((source :initarg  :source
           :reader   syntax-source
           :documentation
           "Stores the source originally containing the configuration
            information (e.g. a `file-source'). The sole purpose of
            storing the source is the possibility to annotate provided
            options with the source from which they originated.")
   (syntax :initarg  :syntax
           :reader   syntax-syntax
           :type     list
           :initform '((parser.ini:*name-component-separator*                . #\.)
                       (parser.ini:*assignment-operator*                     . #\=)
                       (parser.ini:*value-terminating-whitespace-expression* . #\Newline))
           :documentation
           "Stores and alist of the form

              (VARIABLE-NAME . VALUE)

            where VARIABLE-NAME is a symbol naming one of the special
            variables provided by the parser.ini system for
            controlling syntax variants and VALUE is the value to
            which the designated variable should be bound while
            parsing."))
  (:default-initargs
   :source (missing-required-initarg 'ini-syntax :source))
  (:documentation
   "Parse textual configuration information in \"ini\" syntax."))

(service-provider:register-provider/class 'syntax :ini :class 'ini-syntax)

(declaim (special *sink*))

(defvar *sink* nil
  "Dynamically bound to a sink object to make it accessible in the
   method implementing the builder protocol of the parser.ini
   system.")

(defmethod architecture.builder-protocol:make-node
    ((builder ini-syntax)
     (kind    t)
     &rest args &key &allow-other-keys)
  args)

(defmethod architecture.builder-protocol:relate
    ((builder  ini-syntax)
     (relation (eql :section-option))
     (left     list)
     (right    list)
     &key)
  (with-simple-restart (continue "~@<Do not process ~S and ~
                                  continue.~@:>"
                                 right)
    (let+ (((&plist-r/o (section-name :name))
            left)
           ((&plist-r/o (option-name :name)
                        (value       :value)
                        (bounds      :bounds))
            right)
           (name (append section-name option-name))
           ((&flet notify (event &optional value)
              (notify *sink* event name value
                      :source (syntax-source builder)
                      :bounds bounds))))
      (notify :added)
      (notify :new-value value)))
  left)

(defmethod process-content ((syntax ini-syntax) (source stream) (sink t))
  (let ((ini-syntax (syntax-syntax syntax))
        (content    (read-stream-content-into-string source))
        (*sink*     sink))
    (progv (mapcar #'car ini-syntax) (mapcar #'cdr ini-syntax)
      (parser.ini:parse content syntax)))
  (values))
