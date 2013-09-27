;;;; syntax-xml.lisp --- Unit tests for the ini syntax.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

(in-suite options.sources)

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
