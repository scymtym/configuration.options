;;;; mocks.lisp --- Mocks used in tests of the configuration.options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

;;; Simple schema and schema-item for tests

(define-schema *simple-sub-schema*
  "Simple schema for inclusion in a parent schema."
  ("whoop" :type 'string)
  (:wild :type 'integer))

(defun empty-schema ()
  "Empty schema for tests."
  (make-instance 'standard-schema))

(define-schema *simple-schema*
  "Simple configuration options for tests.

   Second paragraph."
  ("foo" :type 'integer :default 1
         :documentation
         "This option controls foo.")
  ("bar" :type 'boolean)
  ("foo" ("fez" :type 'integer))
  ("bar" ("fez" :type 'pathname))
  ("baz" ("foo" :type 'string))
  (:wild :type 'boolean)
  (("wild" :wild-inferiors) :type 'symbol)
  ("sub" *simple-sub-schema*)
  ("sub" ("sub" *simple-sub-schema*)))

(defun simple-schema-item (&key
                           (name    '("simple" "option"))
                           (default nil                  default-supplied?))
  (apply #'make-instance 'standard-schema-item
         :name name
         :type 'integer
         :documentation
         "A simple option."
         (when default-supplied?
           (list :default default))))

(defparameter *simple-schema-item* (simple-schema-item)
  "Simple schema-item for tests.")

;;; Simple configuration and option for tests

(defun empty-configuration ()
  "Empty configuration for tests."
  (make-configuration *simple-schema*))

(defun simple-configuration (&key (schema *simple-schema*))
  "Simple configuration for tests."
  (let* ((configuration (make-configuration schema))
         (synchronizer  (make-instance 'standard-synchronizer
                                       :target configuration))
         (source        (configuration.options.sources:make-source :defaults)))
    (configuration.options.sources:initialize source schema)
    (configuration.options.sources:process source synchronizer)
    configuration))

(defparameter *simple-configuration* (simple-configuration)
  "Simple configuration for tests.")

(defun simple-option (&key
                        (name '("simple" "option") name-supplied?)
                        (value nil                 value-supplied?))
  (let* ((schema-item (if name-supplied?
                          (simple-schema-item :name name)
                          *simple-schema-item*))
         (option      (make-option schema-item (option-name schema-item))))
    (when value-supplied?
      (setf (option-value option) value))
    option))

(defparameter *simple-option* (simple-option)
  "Simple option for tests.")

;;; Mock source and sink classes

(defclass mock-source ()
  ())

(defmethod initialize ((sink   mock-source)
                       (schema (eql :intentional-error)))
  (error "~@<Intentional error.~@:>"))

(defclass mock-sink ()
  ((calls :type     list
          :accessor sink-calls
          :initform '())))

(defmethod notify ((sink  mock-sink)
                   (event t)
                   (name  t)
                   (value t)
                   &rest args &key)
  (let ((args/clean (remove-from-plist
                     args :raw? :source :bounds :option)))
    (appendf (sink-calls sink)
             (list (list* event name value args/clean)))))

(defmethod notify ((sink  mock-sink)
                   (event (eql :intentional-error))
                   (name  t)
                   (value t)
                   &key)
  (error "~@<Intentional error.~@:>"))
