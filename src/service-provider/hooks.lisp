;;;; hooks.lisp --- Integration with service-provider change hook.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider)

(defmethod service-schema :around ((service change-hook-mixin)
                                   &key documentation)
  (declare (ignore documentation))
  (let+ ((schema (call-next-method))
         ((&flet update-provider-item ()
            (let* ((item  (find-option "provider" schema))
                   (names (mapcar #'provider-name
                                  (service-providers service)))
                   (type  `(provider-designator-member ,@names)))
              (reinitialize-instance item :type type)))))
    (hooks:add-to-hook
     (service-change-hook service)
     (lambda (event name provider)
       (log:info "~@<Updating ~A for ~A ~A~@:>" schema event provider)
       (case event
         (:provider-added
          (%add-or-update-provider-schema
           schema name (provider-schema service provider))
          (update-provider-item))
         (:provider-removed
          (update-provider-item)
          (%remove-provider-schema schema name))
         (:provider-updated
          (%add-or-update-provider-schema
           schema name (provider-schema service provider))))))
    schema))
