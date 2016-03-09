;;;; source-stream.lisp --- Source for options from streams.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defclass stream-source (print-items-mixin)
  ((stream :initarg  :stream
           :type     stream
           :reader   source-stream
           :accessor source-%stream
           :documentation
           "Stores the stream from which the source reads when
            processing.")
   (syntax :reader   source-syntax
           :accessor source-%syntax
           :documentation
           "Stores the syntax object which should be used to parse the
            contents of the processed stream."))
  (:default-initargs
   :stream (missing-required-initarg 'stream-source :stream)
   :syntax (missing-required-initarg 'stream-source :syntax))
  (:documentation
   "Instances of this class are options sources which read and parse
    stream containing configuration options in order to provide
    options to sinks.

    Supplied streams are not closed after processing."))

(service-provider:register-provider/class
 'source :stream :class 'stream-source)

(defmethod shared-initialize :after ((instance   stream-source)
                                     (slot-names t)
                                     &key
                                     syntax)
  (let+ (((syntax-class &rest syntax-args) (ensure-list syntax)))
    (setf (source-%syntax instance)
          (apply #'make-syntax syntax-class :source instance
                 syntax-args))))

(defmethod print-items append ((object stream-source))
  `((:syntax ,(class-name (class-of (source-syntax object))) " ~A")))

(defmethod process ((source stream-source) (sink t))
  (process-content (source-syntax source) (source-stream source) sink))
