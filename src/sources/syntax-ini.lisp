;;;; syntax-ini.lisp --- Interpret configuration information in "ini" syntax.
;;;;
;;;; Copyright (C) 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defclass ini-syntax ()
  ((source :initarg  :source
           :reader   syntax-source
           :documentation
           "Stores the source originally containing the configuration
            information (e.g. a `file-source'). The sole purpose of
            storing the source is the possibility to annotate provided
            options with the source from which they originated."))
  (:default-initargs
   :source (missing-required-initarg 'ini-syntax :source))
  (:documentation
   "Instances of this class parse textual configuration information
    according to the widely used \"ini\" syntax."))

(service-provider:register-provider/class 'syntax :ini :class 'ini-syntax)

(declaim (special *sink*))

(defvar *sink* nil
  "Dynamically bound to a sink object to make it accessible in the
   method implementing the builder protocol of the parser.ini
   system.")

(defmethod #+parser.ini.builder-protocol architecture.builder-protocol:make-node
           #-parser.ini.builder-protocol parser.ini:make-node
    ((builder ini-syntax)
     (kind    t)
     &rest args &key &allow-other-keys)
  args)

(defmethod #+parser.ini.builder-protocol architecture.builder-protocol:relate
           #-parser.ini.builder-protocol parser.ini:add-child
    (                              (builder  ini-syntax)
     #+parser.ini.builder-protocol (relation (eql :section-option))
                                   (left     list)
                                   (right    list)
     #+parser.ini.builder-protocol &key)
  (restart-case
      (let+ (((&plist-r/o (section-name :name)) left)
             ((&plist-r/o (option-name :name)
                          (value       :value)
                          (bounds      :bounds)) right)
             (name (append section-name option-name))
             ((&flet notify (event &optional value)
                (notify *sink* event name value
                        :source (syntax-source builder)
                        :bounds bounds))))
        (notify :added)
        (notify :new-value value))
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Do not process ~S and ~
                                continue.~@:>"
                        right))
      (declare (ignore condition))))
  left)

(defmethod process-content ((syntax ini-syntax)
                            (source stream)
                            (sink   t))
  (let ((content (read-stream-content-into-string source))
        (*sink*  sink))
    (parser.ini:parse content syntax))
  (values))
