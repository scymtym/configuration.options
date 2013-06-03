;;;; package.lisp --- Package definition for unit tests of the sources module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options.sources.test
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:eos

   #:options
   #:options.sources

   #:options.test)

  (:export
   #:options.sources.root)

  (:documentation
   "This package contains unit tests for the sources module"))

(cl:in-package #:options.sources.test)

;;; Test suite

(def-suite options.sources
  :in options
  :description
  "Root test suite for the sources module.")

;;; Mock syntax class

(defclass mock-syntax () ())

(service-provider:register-provider/class
 'options.sources::syntax :mock :class 'mock-syntax)

(defmethod shared-initialize :after ((instance   mock-syntax)
                                     (slot-names t)
                                     &key
                                     source)
  (declare (ignore source)))

(defmethod process-content ((syntax mock-syntax) (source stream) (sink t))
  (iter (for statement in (split-sequence
                           #\Space (read-stream-content-into-string source)
                           :remove-empty-subseqs t))
        (let+ (((name value) (split-sequence #\= statement))
               (name (parse-name (string-trim '(#\Space) name))))
          (notify sink :added     name nil)
          (notify sink :new-value name value :raw? t))))

;;; Utilities and macros

(defmacro with-environment-variable ((name value) &body body)
  "Execute BODY with the environment variable named NAME set to
   VALUE."
  (once-only (name)
    `(unwind-protect
          (progn
            (sb-posix:setenv ,name ,value 1)
            ,@body)
       (sb-posix:unsetenv ,name))))

(defmacro with-file ((name content) &body body)
  "Execute BODY with the file named NAME created and populated with
   CONTENT. The file is deleted when BODY finishes or performs a
   non-local exit."
  (once-only (name)
    `(unwind-protect
          (progn
            (ensure-directories-exist ,name)
            (write-string-into-file ,content ,name
                                    :if-does-not-exist :create
                                    :if-exists         :supersede)
            ,@body)
       (delete-file ,name))))

(defmacro with-files (bindings &body body)
  "Execute BODY with files created and populated according to BINDINGS
   entries of which are of the form

     (PATHNAME CONTENT)

   where PATHNAME and CONTENT are both evaluated. The files are
   deleted when BODY finishes or performs a non-local exit."
  (if bindings
      `(with-file ,(first bindings)
         (with-files ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-config-files
    ((prefix basename &optional (type "conf"))
     (&optional prefix-contents home-contents cwd-contents)
     &body body)
  "Execute BODY with config files created according to PREFIX,
   BASENAME and TYPE and populated according to PREFIX-CONTENTS,
   HOME-CONTENTS and CWD-CONTENTS."
  (once-only (prefix basename type)
    `(with-files (,@(when prefix-contents
                      `(((format nil "~A/etc/~A.~A" ,prefix ,basename ,type)
                         ,prefix-contents)))
                  ,@(when home-contents
                      `(((format nil "~~/.config/~A.~A" ,basename ,type)
                         ,home-contents)))
                  ,@(when cwd-contents
                      `(((format nil "~A.~A" ,basename ,type)
                         ,cwd-contents))))
       ,@body)))

(defmacro with-source-and-sink ((source-and-options
                                 &key
                                 (schema     '+simple-schema+)
                                 (source-var 'source)
                                 (sink-var   'sink))
                                &body body)
  "Execute BODY with SOURCE-VAR bound to a source object created
   according to SOURCE-AND-OPTIONS which is of the form

     SOURCE-NAME | (SOURCE-NAME INITARG+)

   and SINK-VAR bound to an `mock-sink' instance."
  (let+ (((source &rest initargs) (ensure-list source-and-options)))
    `(let ((,source-var (make-source ,source ,@initargs))
           (,sink-var   (make-instance 'mock-sink)))
       (initialize ,source-var ,schema)
       (process ,source-var ,sink-var)
       ,@body)))

(defmacro expecting-sink-calls ((&optional sink-var) &body expected)
  "Check the calls information stored in the `mock-sink' object which
   is the value of SINK-VAR against the elements of EXPECTED."
  `(let ((calls/expected (list ,@expected))
         (calls/actual   (sink-calls ,sink-var)))
     (and (is (= (length calls/expected) (length calls/actual)))
          (iter (for call/actual   in calls/actual)
                (for call/expected in calls/expected)
                (is (equal call/expected call/actual))))))
