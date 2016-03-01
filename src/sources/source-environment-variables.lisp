;;;; source-environment-variables.lisp --- Options from environment variables.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

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

(service-provider:register-provider/class
 'source :environment-variables :class 'environment-variables-source)

(defmethod process ((source environment-variables-source)
                    (sink   t))
  "Obtain configuration options from environment variables."
  (let+ (((&accessors-r/o (prefix source-prefix)) source)
         ((&flet name->option-name (name)
            (when (or (null prefix) (starts-with-subseq prefix name))
              (split-sequence #\_ (string-downcase name)
                              :start (if prefix (length prefix) 0)))))
         ((&flet variable->option (string)
            (let+ ((index (position #\= string))
                   (name (if index
                             (name->option-name (subseq string 0 index))
                             (warn "~@<Ignoring environment entry ~S.~@:>"
                                   string)))
                   (value (when index
                            (subseq string (1+ index)))))
              (when name
                (notify sink :added     name nil   :source source)
                (notify sink :new-value name value :source source))))))
    (iter (for entry in (sb-impl::posix-environ))
          (restart-case
              (variable->option entry)
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip entry ~S.~@:>" entry))
              (declare (ignore condition)))))))

(defmethod print-object ((object environment-variables-source) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:[~;~:*\"~A*\"~]" (source-prefix object))))
