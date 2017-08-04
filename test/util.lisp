;;;; util.lisp --- Utilities used in tests of the configuration.options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

;;; Custom predicates

(defun set-equal/equal (expected actual)
  (set-equal expected actual :test #'equal))

(defun set-equal/name-equal (expected actual)
  (set-equal expected actual :test #'name-equal :key #'make-name))

;;; Specialized testing utilities

(defun gen-ascii-name (&key (case :upper))
  (let* ((code         (gen-integer :min (char-code #\0)
                                    :max (ecase case
                                           (:upper (char-code #\Z))
                                           (:lower (char-code #\z)))))
         (alphanumeric (gen-character :code          code
                                      :alphanumericp t)))
    (gen-string :length   (gen-integer :min 1 :max 20)
                :elements (lambda ()
                            (case (random 100)
                              (0 #\_)
                              (1 #\!)
                              (t (funcall alphanumeric)))))))

(defun collect-map-options-calls (container)
  (let+ ((calls '())
         ((&flet collect-call (&rest args)
            (push args calls))))
    (map-options #'collect-call container)
    (nreverse calls)))

(defun check-describe-option-container (object expected-description)
  (let* ((description (with-output-to-string (stream)
                        (describe-object object stream)))
         (rest        (subseq description
                              (1+ (position #\Newline description))))
         (expected    (format nil "~%Tree:~%~{~2@T~A~^~%~}"
                              (split-sequence:split-sequence
                               #\Newline expected-description))))
    (is (string= expected rest))))

;;; Specialized testing utilities for value types

(defclass mock-type-based-validation-schema-item (mock-typed-schema-item
                                                  type-based-validation-mixin)
  ())

(defun check-validate-value (value type expected)
  (let+ ((schema-item (make-instance
                       'mock-type-based-validation-schema-item
                       :type type))
         ((&flet do-it (&key (if-invalid nil))
            (validate-value schema-item value :if-invalid if-invalid))))
    (is (eq expected (do-it))
        "~S is~:[ not~;~] supposed to be of type ~S"
        value expected type)
    (when (not expected)
      (signals option-value-error (do-it :if-invalid #'error)))))

(defclass mock-type-based-merging-schema-item (mock-typed-schema-item
                                               type-based-merging-mixin)
  ())

(defun check-merge-values (values type expected)
  (let+ ((schema-item (make-instance
                       'mock-type-based-merging-schema-item
                       :type type)))
    (is (equal expected
               (multiple-value-list
                (merge-values schema-item values))))))

(defclass mock-type-based-conversion-schema-item (mock-typed-schema-item
                                                  type-based-conversion-mixin)
  ())

(defun check-value<->string (type string value
                             &key (value-test #'equal))
  (let+ ((schema-item (make-instance
                       'mock-type-based-conversion-schema-item
                       :type type))
         ((&flet value= (expected actual)
            (funcall value-test expected actual))))
    (case value
      (option-syntax-error
       (signals option-syntax-error
         (raw->value schema-item string)))
      (t
       (is (equal  string (value->string schema-item value)))
       (is (value= value  (raw->value schema-item string)))))))
