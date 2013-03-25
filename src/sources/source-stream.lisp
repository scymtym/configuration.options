;;;; source-stream.lisp --- Source for options from streams.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defclass stream-source (print-items-mixin)
  ((syntax :initarg  :syntax
           :reader   source-syntax
           :documentation
           ""))
  (:default-initargs
   :syntax (missing-required-initarg 'stream-source :syntax))
  (:documentation
   "Instances of this class are options sources which read and parse
    stream containing configuration options in order to provide
    options to sinks."))

(service-provider::register-provider/class 'source 'stream :class 'stream-source)

(defmethod print-items append ((object stream-source))
  `((:syntax ,(class-name (class-of (source-syntax object))) " ~A")))

(defmethod process ((foo    stream-source)
                    (source stream))
  (process (source-syntax foo) source))

(defmethod process ((foo    stream-source)
                    (source string))
  (with-input-from-string (stream source)
    (process (source-syntax foo) stream)))
