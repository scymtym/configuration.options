;;;; sinks.lisp --- Sinks for option updates via RSB.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.rsb)

(defclass change-publishing-mixin ()
  ((informer  :initarg  :informer
              :type     rsb:informer
              :reader   source-informer
              :writer   (setf source-%informer)
              :documentation
              "")
   (method    :initarg  :method
              :type     (or null keyword)
              :reader   sink-method
              :initform nil
              :documentation
              "")
   (inhibited :initarg  :inhibited
              :type     boolean
              :accessor sink-inhibited?
              :initform nil
              :documentation
              ""))
  (:documentation
   "TODO(jmoringe): document"))

(defclass rsb-sink (endpoint-name-mixin
                    configuration-introspection-mixin
                    change-publishing-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

#+later
(service-provider:register-provider/class 'options.sinks::sink :rsb
                                          :class 'rsb-sink)

#+no (defmethod shared-initialize :before ((instance rsb-sink) (slot-names t)
                                      &key
                                      informer
                                      server
                                      uri
                                      initargs)
  ;; URI default could be /configuration/(schema | options)
  (cond
    ((not (or informer server uri))
     (missing-required-initarg
      'rsb-sink '(xor (and :informer :server) :uri)))
    ((not (or informer server)))
    ((or uri initargs)
     (incompatible-initargs 'rsb-sink
                            :informer informer
                            :informer server
                            :uri      uri
                            :initargs initargs))))

(defmethod shared-initialize :after ((instance rsb-sink) (slot-names t)
                                     &key
                                     informer
                                     #+no server
                                     uri
                                     name
                                     initargs)
  (setf (source-%informer instance)
        (cond
          (informer)
          (uri      (apply #'rsb:make-informer uri t initargs))
          (t        (apply #'rsb:make-informer
                           (puri:uri (format nil "socket:~A" (rsb:scope-string (name->scope :changes :configuration name))))
                           t initargs)))))

;; TODO change-publishing-mixin?
(flet ((send-value (informer method name option)
         (handler-case
             (rsb:send informer
                       (apply #'make-instance
                              'rst.configuration:option
                              :name (print-name nil name)
                              (let+ (((&values value value?)
                                      (option-value option :if-does-not-exist nil)))
                                (when value? (list :value (value->rst value)))))
                       :method method)
           (error (condition) ;; TODO
             (warn (princ-to-string condition))))))

  (defmethod notify ((sink  rsb-sink)
                     (event (eql :added))
                     (name  t)
                     (value standard-option)
                     &key)
    (let+ (((&accessors-r/o (informer   source-informer)
                            (method     sink-method)
                            (inhibited? sink-inhibited?))
            sink))
      (hooks:add-to-hook (event-hook value)
                         (lambda (event name value) ; TODO use one lambda instead of making new ones
                           (notify sink event name value)))
      (unless inhibited?
        (send-value informer method name value))))

  ;; TODO removed?

  (defmethod notify ((sink  rsb-sink)
                     (event (eql :new-value))
                     (name  t)
                     (value standard-option)
                     &key)
    (let+ (((&accessors-r/o (informer   source-informer)
                            (method     sink-method)
                            (inhibited? sink-inhibited?))
            sink))
      (unless inhibited?
        (send-value informer method name value)))))

;;; Future: broadcasting schema changes

(defmethod notify ((sink  rsb-sink)
                   (event (eql :added))
                   (name  t)
                   (value standard-schema-item)
                   &key)
  (rsb:send (source-informer sink)
            (apply #'make-instance
                   'rst.configuration:schema-item
                   :name (print-name nil name)
                   :type (type->rst (option-type value))
                   (append
                    (let+ (((&values default default?)
                            (option-default value :if-does-not-exist nil)))
                      (when default? (list :default default)))
                    (when-let ((documentation (option-documentation value)))
                      (list :description documentation))))))
