;;;; protocol.lisp --- Test for the protocol functions of the options system.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(def-suite standard-configuration
  :in options
  :description
  "Test suite for the `standard-configuration' class.")
(in-suite standard-configuration)

(test standard-configuration.smoke
  "Smoke test for the `standard-configuration' class."

  (mapc
   (lambda+ ((schema-initargs expected-documentation))
     (let* ((schema        (apply #'make-instance 'standard-schema
                                  schema-initargs))
            (configuration (make-configuration schema)))
       (is (equal expected-documentation
                  (option-documentation configuration)))
       (is (equal expected-documentation
                  (documentation configuration t)))))
   ;; schema-initargs        expected-documentation
   '((()                     nil)
     ((:documentation "foo") "foo"))))

(test standard-configuration.find-option.create
  "Test :if-does-not-exist :create for `find-option'."

  (let ((configuration (make-configuration *simple-schema*)))
    ;; Option is not in schema
    (signals no-such-option
      (find-option "no.such.option" configuration
                   :if-does-not-exist :create))

    ;; Some valid cases
    (let+ (((&flet test-case (name)
              (let ((option/create (find-option name configuration
                                                :if-does-not-exist :create))
                    (option/find   (find-option name configuration)))
                (is (not (null option/create)))
                (is (eq option/create option/find))))))

      (mapc #'test-case '("foo" "bar" "foo.fez" "bar.fez" "baz.foo")))))

(def-suite standard-option
  :in options
  :description
  "Test suite for the `standard-option' class.")
(in-suite standard-option)

(test standard-option.smoke
  "Smoke test for the `standard-option' class."

  (mapc
   (lambda+ ((schema-item-initargs name
              expected-name
              expected-type expected-default
              expected-value
              expected-documentation))
     (let* ((schema-item (apply #'make-instance 'standard-schema-item
                                schema-item-initargs))
            (option      (make-option schema-item name)))
       (is (eq schema-item (option-schema-item option)))
       (is (name-equal expected-name (option-name option)))
       (is (equal expected-type (option-type option)))
       (is (equal expected-default
                  (option-default option :if-does-not-exist nil)))
       (is (equal expected-value
                  (option-value option :if-does-not-exist nil)))
       (is (equal expected-documentation (option-documentation option)))
       (is (equal expected-documentation (documentation option t)))))
   ;; schema-item-initargs                                name
   '(((:name ("a" "b") :type boolean)                     ("a" "b")
      ("a" "b") boolean nil nil nil)

     ((:name ("a" "b") :type string)                      ("a" "b")
      ("a" "b") string nil nil nil)

     ((:name ("a" "b") :type string :default "foo")       ("a" "b")
      ("a" "b") string "foo" nil nil)

     ((:name ("a" "b") :type string :documentation "foo") ("a" "b")
      ("a" "b") string nil nil "foo")

     ((:name ("a" :wild) :type string)                    ("a" "b")
      ("a" "b") string nil nil nil)

     ((:name ("a" :wild-inferiors) :type string)          ("a" "b")
      ("a" "b") string nil nil nil))))
