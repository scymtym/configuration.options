;;;; source-environment-variables.lisp --- Options from environment variables.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defclass environment-variables-source ()
  ((prefix :initarg  :prefix
           :type     (or null string)
           :accessor source-prefix
           :initform nil
           :documentation
           "Stores an optional prefix which environment variable names
            have to match in order to be considered for processing by
            the source."))
  (:documentation
   "Instances of this class provide values of environment variables to
    sinks."))

(service-provider::register-provider/class 'source :environment-variables
                                           :class 'environment-variables-source)

(defmethod process ((syntax environment-variables-source) (source t))
  "Obtain configuration options from environment variables."
  (let+ (((&accessors-r/o (prefix source-prefix)) syntax)
         ((&flet name->option-name (name)
            (when (or (null prefix) (starts-with-subseq prefix name))
              (split-sequence #\_ name :start (if prefix (length prefix) 0)))))
         ((&flet variable->option (string)
            (let+ ((index (position #\= string))
                   (name (name->option-name (subseq string 0 index)))
                   (value (subseq string (1+ index))))
              (when name
                (notify *sink* :added   name)
                (notify *sink* :changed name value))))))
    (mapc #'variable->option (sb-impl::posix-environ))))

(defmethod print-object ((object environment-variables-source) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:[~;~:*\"~A*\"~]" (source-prefix object))))
