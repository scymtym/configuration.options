;;;; conditions.lisp --- Conditions used in the cl-options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;;

(define-condition no-such-option (error)
  ((name      :initarg  :name
              :reader   no-such-option-name
              :documentation
              "")
   (container :initarg  :container
              :reader   no-such-option-container
              :documentation
              ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<No option named ~/options::print-name/ in ~A.~@:>"
             (no-such-option-name      condition)
             (no-such-option-container condition))))
  (:documentation
   "TODO(jmoringe): document"))

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
