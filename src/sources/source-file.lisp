;;;; source-file.lisp --- Source for options from configuration files.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

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
                       does not exist.")
   (description       :initarg  :description
                      :type     (or null string)
                      :reader   source-description
                      :initform nil
                      :documentation
                      "A description of the role the file source plays
                       in the configuration scheme."))
  (:default-initargs
   :stream   (load-time-value (make-string-input-stream "") t)
   :pathname (missing-required-initarg 'file-source :pathname))
  (:documentation
   "This source reads configuration data from files."))

(service-provider:register-provider/class
 'source :file :class 'file-source)

(defmethod shared-initialize :after ((instance   file-source)
                                     (slot-names t)
                                     &key
                                     pathname)
  (setf (source-%pathname instance) (parse-namestring pathname)))

(defmethod print-items:print-items append ((object file-source))
  `((:pathname ,(source-pathname object) " ~A")))

(defmethod process ((source file-source) (sink t))
  (let+ (((&structure-r/o
           source- pathname element-type if-does-not-exist description)
          source))
    ;; TODO should use `with-input-from-file' but that behaves
    ;; strangely for :if-does-not-exist nil.
    (with-open-file (stream pathname
                            :direction         :input
                            :element-type      element-type
                            :if-does-not-exist if-does-not-exist)
      (with-source-debug ("~@[~A ~]\"~A\" ~:[does not exist~;exists~]"
                          description (namestring pathname) stream)
        (when stream
          (setf (source-%stream source) stream)
          (call-next-method source sink))))))
