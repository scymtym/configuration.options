;;;; mixins.lisp --- Mixin classes used in the RSB integration.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; TODO overview

(cl:in-package #:configuration.options.rsb)

;;; `endpoint-name-mixin'

(defclass endpoint-name-mixin ()
  ((name :initarg  :name
         :type     endpoint-name
         :reader   endpoint-name
         :documentation
         ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod print-items append ((object endpoint-name-mixin))
  `((:endpoint-name ,(endpoint-name object) "~/configuration.options:print-name")))

;;; `introspection' class

(defclass introspection ()
  ((items  :initarg  :items
           :reader   introspection-items
           :accessor introspection-%items
           :initform nil
           :documentation
           "")
   (server :reader   introspection-server
           :accessor introspection-%server
           :documentation
           ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   introspection)
                                     (slot-names t)
                                     &key
                                     name
                                     base-uri)
  (declare (type endpoint-name name))
  (let+ ((uri (puri:merge-uris (format nil "~{~A~^/~}" name) base-uri)) ; TODO format
         ((&structure introspection- (items %items) (server %server)) instance))
    (setf server
          (rsb.patterns:make-participant
           :local-server uri
           :converters `((nibbles:octet-vector . (:foo ,@(rsb:default-converter 'nibbles:octet-vector))))) ; TODO temp
          (rsb.patterns:server-method server "get")
          (lambda ()
            (log:info "get request received; items: ~A" items)
            (or items (error "~@<Items not available.~@:>")))))) ; TODO synchronization

;;; Mixin classes {configuration,schema}-introspection-mixin

(defclass introspection-mixin ()
  ()
  (:documentation
   "TODO"))

(defmethod make-introspection ((introspection introspection-mixin)
                               (scope         symbol)
                               (items         t))
  (make-instance 'introspection
                 :base-uri (puri:uri (format nil "socket:~A" (rsb:scope-string (base-scope :control scope)))) ; TODO
                 :name     (endpoint-name introspection)
                 :items    items))

(macrolet
    ((define-introspection-mixin (name)
       (let* ((class-name    (symbolicate name '#:-introspection-mixin))
              (slot-name     (symbolicate name '#:-introspection))
              (accessor-name (symbolicate name '#:-%introspection)))
         `(defclass ,class-name (introspection-mixin)
            ((,slot-name :reader   ,slot-name
                         :accessor ,accessor-name
                         :initform nil
                         :documentation
                         ""))
           (:documentation
            "TODO(jmoringe): document")))))

  (define-introspection-mixin schema)
  (define-introspection-mixin configuration))

(defmethod initialize :after ((source schema-introspection-mixin)
                              (schema t))
  (setf (schema-%introspection source)
        (make-introspection source :schema schema)))

;;; `configuration-event-processing-mixin' class

;; TODO rename to …-process*or*-mixin?

(defclass configuration-event-processing-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into classes which process
    configuration events containing either
    `rst.configuration:configuration' or `rst.configuration:option'
    payloads."))

(defmethod handle-event-using-data ((source configuration-event-processing-mixin)
                                    (sink   t)
                                    (event  rsb:event)
                                    (data   t))
  (warn "~@<Ignoring event ~A with payload ~S.~@:>" event data))

(defmethod handle-event-using-data ((source configuration-event-processing-mixin)
                                    (sink   t)
                                    (event  rsb:event)
                                    (data   rst.configuration:configuration))
  (iter (for option in-sequence (rst.configuration:configuration-option data))
        (handle-event-using-data source sink event option)))

(defmethod handle-event-using-data ((source configuration-event-processing-mixin)
                                    (sink   t)
                                    (event  rsb:event)
                                    (data   rst.configuration:option))
  (let+ (((&accessors-r/o (name  rst.configuration:option-name)
                          (value rst.configuration:option-value))
          data)
         (name (parse-name name))
         ((&values value value?)
          (unless (emptyp (rst.configuration:value-data value))
            (values (rst->value value) t)))
         ((&flet notify (event1 &optional (value nil value-supplied?))
            (apply #'notify sink event1 name value
                   :source      source
                   :origin      (rsb:event-origin event)
                   :create-time (rsb:timestamp event :create)
                   (when value-supplied?
                     (list :raw? nil))))))
    (notify :added) ; TODO not always true
    (when value?
      (notify :new-value value))))

;;; `configuration-event-listener-mixin' ; TODO …-*change*-listener-mixin ?

(defclass configuration-event-listener-mixin ()
  ((listener :initarg  :listener
             :type     rsb:listener
             :reader   source-listener
             :writer   (setf source-%listener)
             :documentation
             ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   configuration-event-listener-mixin)
                                     (slot-names t)
                                     &key
                                     name
                                     method)
  (declare (type endpoint-name name))
  (setf (source-%listener instance)
        (rsb:make-listener (puri:merge-uris
                            (rsb:scope-string
                             (name->scope :changes :configuration name))
                            (puri:uri "socket:")))) ; TODO
  (push (rsb.filter:filter :method :method method)
        (rsb:receiver-filters (source-listener instance))))
