;;;; protocol.lisp --- Test for the protocol functions of the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(in-suite options)

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
