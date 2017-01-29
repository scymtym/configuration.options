;;;; service-provider.lisp --- Tests for the architecture.service-provider integration.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider.test)

(in-suite configuration.options.service-provider)

(test service-schema.smoke
  "Smoke test for the `service-schema' function."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline service-provider:register-provider/class))

  (with-service (foo)
    (service-provider:register-provider/class 'foo :bar         :class 'bar)
    (service-provider:register-provider/class 'foo '(:baz :fez) :class 'baz)

    (let+ ((schema (service-schema 'foo))
           ((&flet is-option (name type)
              (if-let ((option (find-option name schema
                                            :if-does-not-exist nil)))
                (is (type= type (option-type option)))
                (fail "~@<Option ~A does not exist in ~A.~@:>"
                      name schema)))))
      (is (string= "Configuration options of the FOO service."
                   (documentation schema t)))
      (is-option "provider"  '(provider-designator-member :bar (:baz :fez)))
      (is-option "bar.a"     'string)
      (is-option "baz:fez.b" '(integer 0 10)))))

(test make-provider.smoke
  "Test using `make-provider' with a configuration instance."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline service-provider:register-provider/class))

  (with-service (foo)
    (service-provider:register-provider/class 'foo :bar         :class 'bar)
    (service-provider:register-provider/class 'foo '(:baz :fez) :class 'baz)

    (let ((instance (instantiate-provider 'foo :bar "a" "bla")))
      (is (typep instance 'bar))
      (is (equal "bla" (slot-value instance 'a))))

    (let ((instance (instantiate-provider 'foo '(:baz :fez) "b" 9)))
      (is (typep instance 'baz))
      (is (equal 9 (slot-value instance 'b))))))
