;;;; service-provider.lisp --- Integration with the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider)

;;; Service and provider schema protocol

(defmethod service-schema
    ((service standard-service)
     &key
     (documentation (format nil "Configuration options of the ~A ~
                                 service."
                            (service-name service))))
  (let+ (((&structure-r/o service- name (providers providers/alist)) service)
         (schema (make-instance 'standard-schema
                                :documentation documentation))
         (names  (loop :for (name . provider) :in providers
                    :do (%add-or-update-provider-schema
                         schema name (provider-schema service provider))
                    :collect name)))
    (setf (find-option "provider" schema)
          (%make-service-provider-schema-item name names))
    schema))

(defmethod provider-schema ((service t) (provider class-provider)
                            &key
                            (documentation (documentation provider t)))
  (let+ (((&structure-r/o provider- name class) provider)
         (documentation (format nil "Configuration of the ~A provider.~
                                     ~@[~2%~A~]"
                                name documentation)))
    (class-schema class :documentation documentation)))

;;; Provider configuration protocol
;;;
;;; This extends the provider instantiation protocol of the
;;; service-provider systems by
;;;
;;; 1. Defining a new generic function
;;;    `make-provider-using-configuration' that accepts a service
;;;    (designator), a provider and a configuration for the provider.
;;;
;;; 2. Defining a method on `service-provider:make-provider' that
;;;    accepts a `standard-configuration' as a designator of the
;;;    provider to instantiate. This method calls
;;;    `make-provider-using-configuration'.

(defmethod make-provider ((service  t)
                          (provider standard-configuration)
                          &key)
  ;; 1. Retrieve the name of the provider to instantiate from the
  ;;    "provider" option.
  ;; 2. Compute a sub-configuration for that provider which consists
  ;;    of all options whose name has the provider name as the initial
  ;;    component, i.e. "PROVIDER-NAME.**".
  ;; 3. Let `make-provider-using-configuration' instantiate the
  ;;    provider using that sub-configuration.
  (let* ((provider-name     (option-value (find-option "provider" provider)))
         (child-name        (%provider-designator->child-name provider-name))
         (provider/resolved (find-provider service provider-name))
         (query             (merge-names (make-name (list child-name))
                                         (make-name '(:wild-inferiors))))
         (configuration (sub-configuration query provider)))
    (make-provider-using-configuration
     service provider/resolved configuration)))

(defmethod make-provider-using-configuration ((service       t)
                                              (provider      t)
                                              (configuration t))
  (let+ ((class    (provider-class provider))
         (initargs '())
         ((&flet initarg-for-option (option)
            (let+ (((&values value value?)
                    (option-value option :if-does-not-exist nil)))
              (when value?
                (appendf initargs
                         (list (option-initarg option class) value)))))))
    (map-options #'initarg-for-option configuration)
    (apply #'make-provider service provider initargs)))

;;; Utility functions

(defun %make-service-provider-schema-item (name names)
  (assert (every (of-type 'provider-designator) names))
  (make-instance 'standard-schema-item
                 :name          "provider"
                 :type          `(provider-designator-member ,@names)
                 :documentation
                 (format nil "Selects one of the providers of the ~
                              ~S service for instantiation."
                         name)))

(defun %provider-designator->child-name (name)
  (format nil "~(~{~A~^:~}~)" (ensure-list name)))

(defun %add-or-update-provider-schema
    (service-schema provider-name provider-schema)
  (let ((name (%provider-designator->child-name provider-name)))
    (setf (find-child name service-schema :if-exists :supersede)
          provider-schema)))

(defun %remove-provider-schema (service-schema provider-name)
  (let ((name (%provider-designator->child-name provider-name)))
    (setf (find-child name service-schema) nil)))
