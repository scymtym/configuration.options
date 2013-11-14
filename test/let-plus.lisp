;;;; let-plus.lisp --- Unit tests for let-plus integration.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(def-suite let-plus-integration
  :in options
  :description
  "Test suite for the let-plus integration.")
(in-suite let-plus-integration)

(test let-plus.&options-r/o.smoke
  "Smoke test for the `&options-r/o' let-plus keyword."

  ;; Try binding to the value of an option which does not have a
  ;; value.
  (signals value-missing-error
    (let+ (((&options-r/o baz.foo) *simple-configuration*))
      baz.foo)) ; avoid unused variable warning

  ;; Bind some option values with explicit names, default values and
  ;; value presence variables.
  (let+ (((&options-r/o
           (foo)
           (bar             :default bar-value?) ; note: default not validated
           ((fez "foo.fez") nil      fez-value?))
          *simple-configuration*))
    ;; foo
    (is (eq 1 foo))
    ;; bar
    (is (eq :default bar)) (is-false bar-value?)
    ;; fez
    (is (eql nil fez) (is-false fez-value?))))

(test let-plus.&options.smoke
  "Smoke test for the `&options' let-plus keyword."

  ;; Try binding to the value of an option which does not have a
  ;; value.
  (signals value-missing-error
    (let+ (((&options baz.foo) *simple-configuration*))
      baz.foo)) ; avoid unused variable warning

  ;; Bind and mutate some option values with explicit names, default
  ;; values and value presence variables. We have to use the
  ;; `simple-configuration' function instead of
  ;; `*simple-configuration*' because we mutate the configuration.
  (let+ (((&options
           (foo)
           (bar             :default bar-value?) ; note: default not validated
           ((fez "foo.fez") nil      fez-value?))
          (simple-configuration)))
    ;; foo
    (is (eq 1 foo))

    (incf foo)
    (is (eql 2  foo))

    ;; bar
    (is (eq :default bar)) (is-false bar-value?)

    (signals option-value-error (setf bar 1))
    (is (eq :default bar)) (is-false bar-value?)

    (setf bar t)
    (is (eq t bar))        (is-true  bar-value?)

    ;; fez
    (is (eql nil fez)) (is-false fez-value?)

    (setf fez 1)
    (is (eql 1 fez)    (is-true  fez-value?))))

(test let-plus.&options/synchronizer.smoke
  "Smoke test for the `&options/synchronizer' let-plus keyword."

  (let* ((configuration (simple-configuration))
         (synchronizer  (make-instance 'standard-synchronizer
                                       :target configuration)))
    ;; Try binding to the value of an option which does not have a
    ;; value.
    (signals value-missing-error
      (let+ (((&options/synchronizer baz.foo) synchronizer))
        baz.foo))                      ; avoid unused variable warning

    ;; Bind and mutate some option values with explicit names, default
    ;; values and value presence variables. We have to use the
    ;; `simple-configuration' function instead of
    ;; `*simple-configuration*' because we mutate the configuration.
    (let+ (((&options/synchronizer
             (foo)
             (bar             :default bar-value?) ; note: default not validated
             ((fez "foo.fez") nil      fez-value?))
            synchronizer))
      ;; foo
      (is (eq 1 foo))

      (incf foo)
      (is (eql 2 foo))

      ;; bar
      (is (eq :default bar)) (is-false bar-value?)

      (signals notification-error (setf bar 1))
      (is (eq :default bar)) (is-false bar-value?)

      (setf bar t)
      (is (eq t bar))        (is-true bar-value?)

      ;; fez
      (is (eql nil fez)) (is-false fez-value?)

      (setf fez 1)
      (is (eql 1 fez)) (is-true  fez-value?))))
