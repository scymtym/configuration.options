;;;; mixins.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

;;; `name-transform-mixin' mixin class

(defclass name-transform-mixin ()
  ((transform :initarg  :transform
              :type     (or null function)
              :reader   source-name-transform
              :initform nil
              :documentation
              ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod emit :around ((sink  name-transform-mixin)
                         (event t)
                         (name  t)
                         &optional datum)
  "TODO(jmoringe): document"
  (if-let ((transform (source-name-transform sink)))
    (call-next-method sink event (funcall transform name) datum)
    (call-next-method)))
