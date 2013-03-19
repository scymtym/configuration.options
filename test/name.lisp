;;;; name.lisp --- Unit tests for name-related stuff.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(in-suite options)

(test name-matches/smoke

  (mapc
   (lambda+ ((query name expected))
     (is (eq (name-matches (make-instance 'wildcard-name :components query) name)
             expected)))

   `((("a" "b" :wild)                     ("a" "b" "c")                          t)
     (("a" "b" :wild "d")                 ("a" "b" "c" "d")                      t)
     (("a" "b" :wild-inferiors "e")       ("a" "b" "c" "d" "e")                  t)
     (("rsb" "transport" :wild "enabled") ("rsb" "transport" "spread" "enabled") t))))
