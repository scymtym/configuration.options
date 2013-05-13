;;;; source-file.lisp --- Source for options from configuration files.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defclass file-source (stream-source)
  ((pathname          :type     pathname
                      :reader   source-pathname
                      :accessor source-%pathname
                      :documentation
                      "Stores the pathname of the file from which
                       configuration options should be read.")
   (element-type      :initarg  :element-type
                      :reader   source-element-type
                      :initform 'character
                      :documentation
                      "Stores the desired element-type of the stream
                       created for the source.")
   (if-does-not-exist :initarg  :if-does-not-exist
                      :reader   source-if-does-not-exist
                      :initform :error
                      :documentation
                      "Stores the behavior in case the specified file
                       does not exist."))
  (:default-initargs
   :stream   (load-time-value (make-string-input-stream "") t)
   :pathname (missing-required-initarg 'file-source :pathname))
  (:documentation
   "Instances of this class are option sources which read and parse
    configuration files in order to provide options to sinks."))

(service-provider:register-provider/class
 'source :file :class 'file-source)

(defmethod shared-initialize :after ((instance   file-source)
                                     (slot-names t)
                                     &key
                                     pathname)
  (setf (source-%pathname instance) (parse-namestring pathname)))

(defmethod print-items append ((object file-source))
  `((:pathname ,(source-pathname object) " ~A")))

(defmethod process ((source file-source)
                    (sink   t))
  (let+ (((&accessors-r/o
           (pathname          source-pathname)
           (element-type      source-element-type)
           (if-does-not-exist source-if-does-not-exist)) source))
    (with-input-from-file (stream pathname
                                  :element-type      element-type
                                  :if-does-not-exist if-does-not-exist)
      (when stream
        (setf (source-%stream source) stream)
        (call-next-method source sink)))))
