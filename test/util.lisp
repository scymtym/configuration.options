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

(defun make-random-string (&key (case :upper) (length 20))
  "Return a random string of length LENGTH."
  (let ((base (case case
                (:upper (char-code #\A))
                (:lower (char-code #\a)))))
    (map-into (make-string length)
              (lambda () (code-char (+ base (random 26)))))))

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
