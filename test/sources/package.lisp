;;;; package.lisp --- Package definition for unit tests of the sources module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.test
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions

   #:fiveam

   #:configuration.options
   #:configuration.options.sources

   #:configuration.options.test)

  ;; Mock syntax class
  (:export
   #:mock-syntax)

  ;; Source construction tests
  (:export
   #:call-as-source-construct-test
   #:source-construct-test)

  ;; Fixtures for external resources
  (:export
   #:with-environment-variable
   #:with-file
   #:with-files
   #:with-config-files)

  ;; Source test utilities
  (:export
   #:with-source-and-sink

   #:are-expected-sink-calls
   #:expecting-sink-calls
   #:expected-notify-calls-for-schema-items)

  (:documentation
   "This package contains unit tests for the sources module"))

(cl:in-package #:configuration.options.sources.test)

;;; Test suite

(def-suite options.sources
  :in options
  :description
  "Root test suite for the sources module.")

;;; Mock syntax class

(defclass mock-syntax () ())

(service-provider:register-provider/class
 'configuration.options.sources::syntax :mock :class 'mock-syntax)

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

;;; Test utilities and macros

(defun call-as-source-construct-test (cases thunk)
  (mapc (lambda+ ((initargs expected))
          (let+ (((&flet do-it () (funcall thunk initargs))))
            (case expected
              (incompatible-initargs
               (signals incompatible-initargs (do-it)))
              (missing-required-initarg
               (signals missing-required-argument (do-it)))
              (t (do-it)))))
        cases))

(defmacro source-construct-test ((test-case-name source-name) &body cases)
  "Define a test case named TEST-CASE-NAME for source SOURCE-NAME
   which tests constructing the source according to CASES. CASES is a
   list of evaluated specifications of the form

     (INITARGS EXPECTED)

   where EXPECTED is one of `incompatible-initargs',
   `missing-required-argument' or t."
  (let+ (((&values cases &ign documentation)
          (parse-body cases :documentation t)))
    `(test ,test-case-name
       ,@(when documentation `(,documentation))

       (call-as-source-construct-test
        (list ,@cases)
        (lambda (initargs)
          (apply #'make-source ',source-name initargs))))))

;;; Fixture-like utilities and macros

(defmacro with-environment-variable ((name value) &body body)
  "Execute BODY with the environment variable named NAME set to
   VALUE."
  (once-only (name)
    `(unwind-protect
          (progn
            (setf (uiop:getenv ,name) ,value)
            ,@body)
       #+sbcl (sb-posix:unsetenv ,name))))

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
                                 (schema     '*simple-schema*)
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

(defun are-expected-sink-calls (calls/expected calls/actual)
  (and (is (= (length calls/expected) (length calls/actual)))
       (iter (for call/actual   in calls/actual)
             (for call/expected in calls/expected)
             (is (equal call/expected call/actual)))))

(defmacro expecting-sink-calls ((&optional sink-var) &body expected)
  "Check the calls information stored in the `mock-sink' object which
   is the value of SINK-VAR against the elements of EXPECTED."
  `(are-expected-sink-calls (list ,@expected) (sink-calls ,sink-var)))

(defun expected-notify-calls-for-schema-items (schema &key index)
  (let ((expected '()))
    ;; We expect :added and optionally :new-value notifications for
    ;; the non-wild-named schema-items in SCHEMA.
    (map-options
     (lambda (item &key prefix &allow-other-keys)
       (let+ ((name (merge-names prefix (option-name item)))
              ((&values default default?)
               (option-default item :if-does-not-exist nil)))
         (unless (typep name 'configuration.options::wild-name)
           (push `(:added ,name nil
                          ,@(when index `(:index ,index)))
                 expected)
           (when default?
             (push `(:new-value ,name ,default
                                ,@(when index `(:index ,index)))
                   expected)))))
     schema)
    (nreverse expected)))
