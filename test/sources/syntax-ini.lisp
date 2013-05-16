;;;; syntax-ini.lisp --- Unit tests for the ini syntax.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

(in-suite options.sources)

(test ini-syntax.smoke
  "Smoke test for `ini-syntax' class."

  (with-input-from-string (stream "a.b=1
                                   a=2
                                   # comment
                                   [a]
                                   c = 3")
    (let ((parser.ini:*value-terminating-whitespace-expression* :newline))
      (with-source-and-sink ((:stream :stream stream :syntax :ini)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          '(:added     ("a" "b") nil)
          '(:new-value ("a" "b") "1")
          '(:added     ("a")     nil)
          '(:new-value ("a")     "2")
          '(:added     ("a" "c") nil)
          '(:new-value ("a" "c") "3"))))))
