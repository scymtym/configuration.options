;;;; hooks.lisp --- Tests for hooks-based updating of service schemas.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider.test)

(in-suite configuration.options.service-provider)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass service-with-change-hook (service-provider:standard-service
                                      service-provider::change-hook-mixin)
    ()))

(test hooks.schema-update
  "Test updating a service schema."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline service-provider:register-provider/class
                      (setf service-provider:find-provider)))

  (with-service (foo (:service-class service-with-change-hook))
    (let+ ((service 'foo)
           (schema  (service-schema service))
           ((&flet is-option (name type)
              (if-let ((option (find-option name schema
                                            :if-does-not-exist nil)))
                (is (type= type (option-type option)))
                (fail "~@<Option ~A does not exist in ~A.~@:>"
                      name schema))))
           ((&flet is-not-option (name)
              (is-false (find-option name schema :if-does-not-exist nil)))))

      ;; No providers are registered.
      (is-option     "provider" '(provider-designator-member))
      (is-not-option "bar.a")
      (is-not-option "fez.b")
      (is-not-option "fez.c")

      ;; Provider `bar' is registered.
      (service-provider:register-provider/class service :bar :class 'bar)
      (is-option     "provider" '(provider-designator-member :bar))
      (is-option     "bar.a"    'string)
      (is-not-option "fez.b")
      (is-not-option "fez.c")

      ;; Providers `bar' and `fez' are registered.
      (eval '(defclass fez () ((b :initarg :b :type (integer 0 10)))))
      (service-provider:register-provider/class service :fez :class 'fez)
      (is-option     "provider" '(provider-designator-member :bar :fez))
      (is-option     "bar.a"    'string)
      (is-option     "fez.b"    '(integer 0 10))
      (is-not-option "fez.c")

      ;; Provider `fez' has been redefined.
      (eval '(defclass fez () ((c :initarg :c :type boolean))))
      (service-provider:register-provider/class service :fez :class 'fez)
      (is-option     "provider" '(provider-designator-member :bar :fez))
      (is-option     "bar.a"    'string)
      (is-not-option "fez.b")
      (is-option     "fez.c"    'boolean)

      ;; Provider `fez' has been unregistered. Only provider `bar'
      ;; remains.
      (setf (service-provider:find-provider service :fez) nil)
      (is-option     "provider" '(provider-designator-member :bar))
      (is-option     "bar.a"    'string)
      (is-not-option "fez.b")
      (is-not-option "fez.c"))))

(test hooks.make-provider
  "Test constructing providers of an updated service schema."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline service-provider:register-provider/class
                      (setf service-provider:find-provider)
                      service-provider:make-provider))

  (with-service (foo (:service-class service-with-change-hook))
    (let+ ((service 'foo)
           (schema  (service-schema service))
           ((&flet make-provider (&rest option-values)
              (let+ ((configuration (make-configuration schema))
                     (synchronizer  (make-instance 'standard-synchronizer
                                                   :target configuration))
                     (defaults      (configuration.options.sources:make-source
                                     :defaults))
                     ((&flet set-value (name value)
                        (setf (option-value (find-option name configuration))
                              value))))
                (configuration.options.sources:initialize defaults schema)
                (configuration.options.sources:process defaults synchronizer)
                (mapc (lambda+ ((name value &optional expected))
                        (case expected
                          (option-missing-error
                           (signals option-missing-error
                             (set-value name value)))
                          (option-value-error
                           (signals option-value-error
                             (set-value name value)))
                          (t
                           (finishes (set-value name value)))))
                      option-values)
                (service-provider:make-provider service configuration)))))

      ;; No providers are registered.
      (signals value-missing-error
        (make-provider '("provider" :bar  option-value-error)
                       '("bar.a"    "bla" option-missing-error)
                       '("fez.b"    5     option-missing-error)
                       '("fez.c"    t     option-missing-error)))

      ;; Provider `bar' is registered.
      (service-provider:register-provider/class service :bar :class 'bar)
      (finishes
        (make-provider '("provider" :bar)
                       '("bar.a"    "bla")
                       '("fez.b"    5     option-missing-error)
                       '("fez.c"    t     option-missing-error)))

      ;; Providers `bar' and `fez' are registered.
      (eval '(defclass fez () ((b :initarg :b :type (integer 0 10)))))
      (service-provider:register-provider/class service :fez :class 'fez)
      (finishes
        (make-provider '("provider" :fez)
                       '("bar.a"    "bla")
                       '("fez.b"    5)
                       '("fez.c"    t     option-missing-error)))

      ;; Provider `fez' has been redefined.
      (eval '(defclass fez () ((c :initarg :c :type boolean))))
      (service-provider:register-provider/class service :fez :class 'fez)
      (finishes
        (make-provider '("provider" :fez)
                       '("bar.a"    "bla")
                       '("fez.b"    5     option-missing-error)
                       '("fez.c"    t)))

      ;; Provider `fez' has been unregistered. Only provider `bar'
      ;; remains.
      (setf (service-provider:find-provider service :fez) nil)
      (signals value-missing-error
        (make-provider '("provider" :fez  option-value-error)
                       '("bar.a"    "bla")
                       '("fez.b"    5     option-missing-error)
                       '("fez.c"    t     option-missing-error))))))
