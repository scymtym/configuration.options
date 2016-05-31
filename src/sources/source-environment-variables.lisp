;;;; source-environment-variables.lisp --- Options from environment variables.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defclass environment-variables-source (print-items:print-items-mixin)
  ((name-mapping :initarg  :name-mapping
                 :type     function
                 :reader   source-name-mapping
                 :accessor source-%name-mapping
                 :initform (make-environment-variable-name->option-name)
                 :documentation
                 "Stores a function that maps environment variable
                  names to option names."))
  (:documentation
   "Instances of this class provide values of environment variables to
    sinks."))

(service-provider:register-provider/class
 'source :environment-variables :class 'environment-variables-source)

(defmethod shared-initialize :before
    ((instance   environment-variables-source)
     (slot-names t)
     &key
     (prefix       nil prefix-supplied?)
     (name-mapping nil name-mapping-supplied?))
  (when (and prefix-supplied? name-mapping-supplied?)
    (incompatible-initargs 'environment-variables-source
                           :prefix       prefix
                           :name-mapping name-mapping)))

(defmethod shared-initialize :after
    ((instance   environment-variables-source)
     (slot-names t)
     &key
     (prefix       nil prefix-supplied?)
     (name-mapping nil name-mapping-supplied?))
  (cond
    (name-mapping-supplied?
     (setf (source-%name-mapping instance) name-mapping))
    (prefix-supplied?
     (setf (source-%name-mapping instance)
           (make-environment-variable-name->option-name :prefix prefix)))))

(defmethod process ((source environment-variables-source) (sink t))
  ;; Obtain configuration options from environment variables.
  (let+ (((&structure-r/o source- name-mapping) source)
         ((&flet variable->option (string)
            (log:trace "~@<~A is processing environment entry ~S~@:>"
                       source string)
            (let* ((index (position #\= string))
                   (name  (if index
                              (funcall name-mapping (subseq string 0 index))
                              (warn "~@<Ignoring environment entry ~S.~@:>"
                                    string)))
                   (value (when index
                            (subseq string (1+ index)))))
              (when name
                (log:debug "~@<~A mapped ~S to ~
                            ~/configuration.options:print-name/ ← ~S~@:>"
                           source string name value)
                (notify sink :added     name nil   :source source)
                (notify sink :new-value name value :source source))))))
    (iter (for entry in #+sbcl (sb-ext:posix-environ)
                        #-sbcl '())
          (restart-case
              (variable->option entry)
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Skip entry ~S.~@:>" entry))
              (declare (ignore condition)))))))

(defmethod print-items:print-items append ((object environment-variables-source))
  (let* ((mapping (source-name-mapping object))
         (in      "A_B")
         (out     (funcall mapping in))
         (example (when out
                    (list in out))))
    `((:mapping ,example "~:[with prefix mapping~;~:*~{~A → ~
                          ~/configuration.options:print-name/~}~]"))))

;;; Utilities

(defun make-environment-variable-name->option-name (&key prefix)
  (lambda (name)
    (when (or (null prefix) (starts-with-subseq prefix name))
      (split-sequence #\_ (string-downcase name)
                      :start (if prefix (length prefix) 0)))))
