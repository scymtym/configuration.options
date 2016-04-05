;;;; sources.lisp --- Sources for option updates via RSB.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; TODO overview
;;;; Class `rsb-source'
;;;;
;;;; Class `pull-source'

(cl:in-package #:configuration.options.rsb)

(defclass rsb-source (endpoint-name-mixin
                      schema-introspection-mixin
                      configuration-event-processing-mixin) ; TODO listener
  ((listener :initarg  :listener
             :type     rsb:listener
             :reader   source-listener
             :writer   (setf source-%listener)
             :documentation
             ""))
  (:documentation
   "TODO(jmoringe): document"))

(service-provider:register-provider/class
 'configuration.options.sources::source :rsb :class 'rsb-source)

(defmethod shared-initialize :before ((instance rsb-source) (slot-names t)
                                      &key
                                      listener
                                      uri
                                      initargs)
  ;; uri default could be /__rsb/configuration/(schema | options)
  (cond
    ((not (or listener uri))
     (missing-required-initarg 'rsb-source '(xor :listener :uri)))
    ((not listener))
    ((or uri initargs)
     (incompatible-initargs 'rsb-source
                            :listener listener
                            :uri      uri
                            :initargs initargs))))

(defmethod shared-initialize :after ((instance rsb-source) (slot-names t)
                                     &key
                                     listener
                                     uri
                                     initargs)
  (setf (source-%listener instance)
        (cond
          (listener)
          (uri      (apply #'rsb:make-listener uri initargs))))
  (push (rsb.filter:filter :method :method :put)
        (rsb:receiver-filters (source-listener instance))))

(defmethod request-configuration ((source rsb-source))
  (rsb:with-informer (informer (format nil "socket:~A" (rsb:scope-string (name->scope :control :configuration (endpoint-name source)))) t) ; TODO
    (log:info "~@<~A is sending configuration request to ~A~@:>"
              source (rsb:participant-scope informer))
    (rsb:send informer rsb.converter:+no-value+)))

(defmethod process ((source rsb-source)
                    (sink   t))
  (let+ (((&accessors-r/o (listener source-listener)) source) ; TODO put this part into a configuration event receiving mixin
         (result)
         ((&flet handle-configuration-event (event)
            (log:info "~@<~A received configuration event ~A~@:>"
                      source event)
            (handler-bind
                ((error (lambda (condition) ; TODO
                          (print (bt:current-thread))
                          (princ condition)
                          (sb-debug:print-backtrace)
                          (setf result condition)
                          (return-from handle-configuration-event))))
              (handle-event-using-data source sink event (rsb:event-data event))))))
    ;; Steps 1 and 2 two of "Initial Configuration Protocol".
    (iter
      (rsb:with-handler (source-listener source)
          ((event) ; TODO check cause
           (handle-configuration-event event)
           (unless result
             (setf result t)))
        (request-configuration source)
        (sleep 5)) ; Wait 5 seconds for replies. TODO configurable?

      (restart-case
          (etypecase result
            ((eql t) (return))
            (null    (error "~@<Did not receive a configuration ~
                             message.~@:>"))
            (error   (signal result)))
        (continue (&optional condition) ; TODO necessary? provided by protocol?
          :report (lambda (stream)
                    (format stream "~@<Continue without processing
                                    ~A.~:>"
                            source))
          (declare (ignore condition))
          (return))
        (retry ()
          :report (lambda (stream)
                    (format stream "~@<Retry processing ~A.~@:>"
                            source)))))
    ;; TODO racy

    ;; Accept configuration updates.
    (push #'handle-configuration-event
          (rsb.event-processing:handlers listener))))

;;;

;; TODO better name
(defclass pull-source (endpoint-name-mixin
                       configuration-event-listener-mixin
                       configuration-event-processing-mixin)
  ()
  (:documentation
   "This source performs an initial retrieval of the configuration of
    an endpoint and receives and processes configuration events after
    that."))

(service-provider:register-provider/class
 'configuration.options.sources::source :rsb/pull :class 'pull-source)

(defmethod shared-initialize :after ((instance pull-source) (slot-names t)
                                     &key
                                     name)
  (declare (type endpoint-name name))
  (setf (source-%listener instance)
        (rsb:make-listener (puri:merge-uris
                            (rsb:scope-string
                             (name->scope :changes :configuration name))
                            (puri:uri "socket:")))) ; TODO
  (push (rsb.filter:filter :method :method nil)
        (rsb:receiver-filters (source-listener instance))) ; TODO do this in a mixin
  )

;; TODO the following is missing here: when retrieving the initial
;; configuration, we should record some kind of version. When
;; configuration changes are received, they should be discarded when
;; their version is below that of already received snapshot.
;;
;; Receiving changes should start before retrieving the initial
;; snapshot to avoid the race condition of missing changes.
(defmethod process ((source pull-source)
                    (sink   t))
  (let+ (((&accessors-r/o (name     endpoint-name)
                          (listener source-listener)) source) ; TODO put this part into a configuration event receiving mixin
         (result)
         ((&flet handle-configuration-event (event)
            (log:info "~@<~A received configuration event ~A~@:>"
                      source event)
            (handler-bind
                ((error (lambda (condition) ; TODO
                          (print (bt:current-thread))
                          (princ condition)
                          (sb-debug:print-backtrace)
                          (setf result condition)
                          (return-from handle-configuration-event))))
              (handle-event-using-data source sink event (rsb:event-data event))))))
    ;; Steps 1 and 2 two of "Initial Configuration Protocol":
    ;; 1. TODO
    ;; 2. TODO
    (iter
      (restart-case
          (let ((event (rsb.patterns:with-remote-server
                           (server (puri:merge-uris
                                    (rsb:scope-string
                                     (name->scope :control :configuration name))
                                    (puri:uri "socket:"))) ; TODO
                         (rsb.patterns:call server "get" rsb.converter:+no-value+
                                            :return :event))))
            (handle-event-using-data source sink event (rsb:event-data event))
            (return))
        (continue (&optional condition) ; TODO necessary? provided by protocol?
          :report (lambda (stream)
                    (format stream "~@<Continue without processing
                                    ~A.~:>"
                            source))
          (declare (ignore condition))
          (return))
        (retry ()
          :report (lambda (stream)
                    (format stream "~@<Retry processing ~A.~@:>"
                            source)))))
    ;; TODO racy

    ;; Accept configuration updates.
    (push #'handle-configuration-event
          (rsb.event-processing:handlers listener))))
