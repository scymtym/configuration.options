;;;; conditions.lisp --- Conditions used by the sources module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(define-condition source-condition (condition)
  ((source :initarg  :source
           :reader   source-condition-source
           :documentation
           "Stores the source involved in the operation for which the
            condition is signaled."))
  (:default-initargs
   :source (missing-required-initarg 'source-condition :source))
  (:documentation
   "Instances of subclasses of this condition are signaled when an
    unexpected condition is encountered during an operation involving
    a source."))

(define-condition initialization-error (error
                                        chainable-condition
                                        source-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not initialize configuration source ~
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (source-condition-source condition)
             condition)))
  (:documentation
   "This error is signaled when initializing a configuration source
    fails."))

(define-condition sink-condition (condition)
  ((sink :initarg  :sink
         :reader   sink-condition-sink
         :documentation
         "Stores the sink involved in the operation for which the
          condition is signaled."))
  (:default-initargs
   :sink (missing-required-initarg 'sink-condition :sink))
  (:documentation
   "Instances of subclasses of this condition are signaled when an
    unexpected condition is encountered during an operation involving
    a sink."))

(define-condition processing-error (error
                                    chainable-condition
                                    source-condition
                                    sink-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not process configuration source ~A and ~
                     sink ~A~/more-conditions:maybe-print-cause/~@:>"
             (source-condition-source condition)
             (sink-condition-sink condition)
             condition)))
  (:documentation
   "This error is signaled when processing a source in order to
    provide options to a sink fails."))
