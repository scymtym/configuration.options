;;;; syntax-xml.lisp --- Unit tests for the xml syntax.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources.syntax-xml.test
  (:use
   #:cl
   #:fiveam

   #:configuration.options.sources.test)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the syntax-xml module."))

(cl:in-package #:configuration.options.sources.syntax-xml.test)

;;; Test suite

(def-suite options.sources.syntax-xml
  :description
  "Root test suite for the syntax-xml module.")

(defun run-tests ()
  (let ((results (run 'options.sources.syntax-xml)))
    (explain! results)
    (results-status results)))

;;; Tests

(in-suite options.sources.syntax-xml)

(test xml-syntax.smoke
  "Smoke test for the `xml-syntax' class."

  (with-input-from-string
      (stream "<group>
                 <value name=\"a.b\">1</value>
                 <value name=\"a\">2</value>
                 <!-- comment -->
                 <value name=\"a.c\">3</value>
               </group>")
    (with-source-and-sink
        ((:stream :stream stream
                  :syntax '(:xml :option-pattern "group/value"
                                 :name-pattern   "@name"
                                 :value-pattern  "text()"))
         :sink-var sink)
      (expecting-sink-calls (sink)
        '(:added     ("a" "b") nil)
        '(:new-value ("a" "b") "1")
        '(:added     ("a")     nil)
        '(:new-value ("a")     "2")
        '(:added     ("a" "c") nil)
        '(:new-value ("a" "c") "3")))))
