;;;; syntax-ini.lisp --- Unit tests for the ini syntax.
;;;;
;;;; Copyright (C) 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.syntax-ini.test
  (:use
   #:cl
   #:fiveam

   #:configuration.options.sources.test)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the syntax-ini module."))

(cl:in-package #:configuration.options.sources.syntax-ini.test)

;;; Test suite

(def-suite options.sources.syntax-ini
  :description
  "Root test suite for the syntax-ini module.")

(defun run-tests ()
  (let ((results (run 'options.sources.syntax-ini)))
    (explain! results)
    (results-status results)))

;;; Tests

(in-suite options.sources.syntax-ini)

(test ini-syntax.smoke
  "Smoke test for `ini-syntax' class."

  (with-input-from-string (stream "a.b=1
                                   a=2
                                   # comment
                                   [a]
                                   c = 3")
    (with-source-and-sink ((:stream :stream stream :syntax :ini)
                           :sink-var sink)
      (expecting-sink-calls (sink)
        '(:added     ("a" "b") nil)
        '(:new-value ("a" "b") "1")
        '(:added     ("a")     nil)
        '(:new-value ("a")     "2")
        '(:added     ("a" "c") nil)
        '(:new-value ("a" "c") "3")))))

(test ini-syntax.syntax-variation
  "Test activation of syntax variations in `ini-syntax' class."

  (flet ((test-case (bindings input)
           (with-input-from-string (stream input)
             (with-source-and-sink
                 ((:stream :stream stream
                           :syntax `(:ini :syntax ,bindings))
                  :sink-var sink)
               (expecting-sink-calls (sink)
                 '(:added     ("a" "b") nil)
                 '(:new-value ("a" "b") "1")
                 '(:added     ("a")     nil)
                 '(:new-value ("a")     "2"))))))

    (test-case
     '((parser.ini:*name-component-separator*                . #\.)
       (parser.ini:*assignment-operator*                     . #\=)
       (parser.ini:*value-terminating-whitespace-expression* . #\Newline))
     "a.b=1
      a=2")
    (test-case
     '((parser.ini:*name-component-separator*                . #\-)
       (parser.ini:*assignment-operator*                     . #\:)
       (parser.ini:*value-terminating-whitespace-expression* . #\Newline))
     "a-b:1
      a:2")))
