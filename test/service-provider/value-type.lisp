;;;; value-type.lisp --- Tests for provider-designator-member value type.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.service-provider.test)

(in-suite configuration.options.service-provider)

(test validate-value
  "Test for `validate-value' methods for `provider-designator-member'
   type."

  (mapc (lambda+ ((value type-args expected))
          (check-validate-value
           value `(provider-designator-member ,@type-args) expected))
        '((nil         ()           nil)
          (nil         (nil)        t)

          (nil         (:foo)        nil)
          (:bar        (:foo)        nil)
          (:foo        (:foo)        t)

          (:foo        ((:foo :bar)) nil)
          ((:foo :bar) ((:foo :bar)) t)

          (:bar        (:foo :bar)   t)
          (:foo        (:foo :bar)   t))))

(test string<->value
  "Test `value->string' and `raw->value' methods for
   `provider-designator-member' type."

  (mapc (lambda+ ((type-args string value))
          (check-value<->string
           `(provider-designator-member ,@type-args) string value))
        '(((:foo (:baz :fez)) "foo"     :foo)
          ((:foo (:baz :fez)) "baz/fez" (:baz :fez))
          ((:foo (:baz :fez)) "bar"     option-syntax-error)

          ((:foo :bar)        "foo"     :foo))))
