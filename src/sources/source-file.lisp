;;;; file.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defclass file-source (stream-source)
  ((pathname          :type     pathname
                      :reader   source-pathname
                      :accessor %source-pathname
                      :documentation
                      "")
   (if-does-not-exist :initarg  :if-does-not-exist
                      :type     if-does-not-exist-policy
                      :reader   source-if-does-not-exist
                      :documentation
                      ""))
  (:default-initargs
   :pathname (missing-required-initarg 'file-source :pathname))
  (:documentation
   "TODO(jmoringe): document"))

(service-provider::register-provider/class 'source 'file :class 'file-source)

(defmethod shared-initialize :after ((instance   file-source)
                                     (slot-names t)
                                     &key
                                     pathname)
  (setf (%source-pathname instance) (parse-namestring pathname)))

(defmethod print-items append ((object file-source))
  `((:pathname ,(source-pathname object) "~A")))
