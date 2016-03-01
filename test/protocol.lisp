;;;; protocol.lisp --- Test for the protocol functions of the options system.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(in-suite options)

(macrolet
    ((define-value-function-test ((name &key (value-var 'value))
                                  construction-expression
                                  &body cases)
       `(test ,name
          ,(format nil "Test default behavior of `~(~A~)' function."
                   name)
          (mapc
           (lambda+ ((,value-var expected-value expected-value?))
             (let+ ((object ,construction-expression)
                    ((&flet do-it (&optional (if-does-not-exist
                                              nil
                                              if-does-not-exist-supplied?))
                       (apply #',name object
                              (when if-does-not-exist-supplied?
                                `(:if-does-not-exist ,if-does-not-exist))))))
               (let+ (((&values value value?) (do-it nil)))
                 (is (equal expected-value  value))
                 (is (eq    expected-value? value?)))
               (when (not expected-value?)
                 (signals no-such-value-error (do-it))
                 (is (equal :foo (handler-bind
                                     ((no-such-value-error (lambda (c)
                                                             (declare (ignore c))
                                                             (use-value :foo))))
                                   (do-it)))))))
           (list ,@cases)))))

  (define-value-function-test (option-default :value-var default)
      (apply #'make-instance 'standard-schema-item
             :name '("a" "b")
             :type t
             (when default `(:default ,(first default))))
    ;; default expected default expected default?
    '(()       nil              nil)
    '((1)      1                t)
    '((nil)    nil              t))

  (define-value-function-test (option-value :value-var value)
      (let* ((item   (make-instance 'standard-schema-item
                                    :name '("a" "b") :type t))
             (option (make-option item '("a" "b"))))
        (when value
          (setf (option-value option) (first value)))
        option)
    ;; value expected value expected value?
    '(()     nil            nil)
    '((1)    1              t)
    '((nil)  nil            t)))

(test setf-option-value.if-does-not-exist
  "Test that setf `option-value' accepts the :if-does-not-exist
   option (despite ignoring it)."

  (let* ((item   (make-instance 'standard-schema-item
                                :name '("a") :type t))
         (option (make-option item '("a"))))
    (setf (option-value option :if-does-not-exist #'error) 1)))

(test validate-value.default-behavior
  "Smoke test for the default behavior of the `validate-value'
   function."

  (let ((schema-item (make-instance 'standard-schema-item
                                    :name "a"
                                    :type 'boolean)))
    (is (eq :foo (validate-value schema-item 1 :if-invalid :foo)))

    (handler-bind ((option-value-error #'continue))
      (is (eq t (validate-value schema-item 1))))))
