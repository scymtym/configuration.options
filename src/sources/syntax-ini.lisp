;;;; syntax-ini.lisp --- Interpret configuration information in "ini" syntax.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
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

(service-provider::register-provider/class 'syntax :ini :class 'ini-syntax)

(declaim (special *sink*))

(defvar *sink* nil
  "Dynamically bound to a sink object to make it accessible in the
   method implementing the builder protocol of the parser.ini
   system.")

(defmethod parser.ini:make-node ((builder ini-syntax)
                                 (kind    t)
                                 &rest args &key &allow-other-keys)
  args)

(defmethod parser.ini:add-child ((builder ini-syntax)
                                 (parent  list)
                                 (child   list))
  ;; TODO record location
  ;; TODO continue restart?
  (let+ (((&plist-r/o (parent-name :name)) parent)
         ((&plist-r/o (child-name :name)
                      (value      :value)
                      (bounds     :bounds)) child)
         (name (append parent-name child-name)))
    (notify *sink* :added     name nil
            :source (cons (syntax-source builder) bounds))
    (notify *sink* :new-value name value
            :source (cons (syntax-source builder) bounds))
    parent))

(defmethod process-content ((syntax ini-syntax)
                            (source stream)
                            (sink   t))
  (let ((content (read-stream-content-into-string source))
        (*sink*  sink))
    (parser.ini:parse content syntax))
  (values))
