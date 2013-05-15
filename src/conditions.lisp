;;;; conditions.lisp --- Conditions used in the options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; Name-related conditions

(define-condition name-parse-error (parse-error)
  ((text :initarg  :text
         :type     string
         :reader   name-parse-error-text
         :documentation
         "Stores the string which could not be parsed as an option
          name."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to parse name ~S.~@:>"
             (name-parse-error-text condition))))
  (:documentation
   "This error is signaled when a given string cannot be parsed as an
    option name."))

;;; Option locating conditions

(define-condition no-such-option (error)
  ((name      :initarg  :name
              :reader   no-such-option-name
              :documentation
              "Stores the name of the option which could not be
               found.")
   (container :initarg  :container
              :reader   no-such-option-container
              :documentation
              "Stores the container object in which the requested
               option could not be found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No option named ~/options::print-name/ in ~
                     ~A.~@:>"
             (no-such-option-name      condition)
             (no-such-option-container condition))))
  (:documentation
   "This error is signaled when a requested option-like object cannot
    be found."))

;;;

(define-condition option-condition (condition)
  ((option :initarg  :option
           :reader   option-condition-option
           :documentation
           ""))
  (:default-initargs
   :option (missing-required-initarg 'option-condition :option))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition value-condition (condition)
  ((value :initarg  :value
          :reader   value-condition-value
          :documentation
          ""))
  (:default-initargs
   :value (missing-required-initarg 'value-condition :value))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition option-value-error (error
                                      option-condition
                                      value-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The value ~S is invalid for option ~A.~@:>"
             (option-condition-option condition)
             (value-condition-value   condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition option-value-type-error (option-value-error
                                           type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The value ~S is invalid for option ~A.~@:>"
             (option-condition-option condition)
             (value-condition-value   condition))))
  (:documentation
   "TODO(jmoringe): document"))

(define-condition option-syntax-error (option-value-error)
  ()
  (:documentation
   "TODO(jmoringe): document"))

;;; Sink-protocol-related conditions

(define-condition notification-error (error
                                      value-condition
                                      chainable-condition)
  ((sink   :initarg  :sink
           :reader   notification-error-sink
           :documentation
           "Stores the sink which was being notified when the error
            was encountered.")
   (event  :initarg  :event
           :reader   notification-error-event
           :documentation
           "Stores the event of which the was being notified when the
            error was encountered")
   (name   :initarg  :name
           :reader   notification-error-name
           :documentation
           "Stores the name of the option that was the subject of the
            notification during which the error was encountered.")
   (source :initarg  :source
           :reader   notification-error-source
           :documentation
           "Stores the source from which the notification
            originated."))
  (:report
   (lambda (condition stream)
     (format stream "~@<When notifying sink ~A of ~S option ~
                     ~/options::print-name/~@[ with value ~S~]~@[ by ~
                     source ~A~]~
                     ~/more-conditions::maybe-print-cause/~@:>"
             (notification-error-sink   condition)
             (notification-error-event  condition)
             (notification-error-name   condition)
             (value-condition-value     condition)
             (notification-error-source condition)
             condition)))
  (:documentation
   "This error is signaled when an error is encountered during
    notification of a sink of an event originating from a source."))
