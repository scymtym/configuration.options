;;;; source.lisp --- Unit tests for the commandline source.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.commandline.test)

(in-suite options.sources.commandline)

(test commandline-source.smoke
  "Smoke test for the `commandline-source' class."

  (let* ((prefix    (concatenate
                     'string (make-random-string :case :lower) "-"))
         (arguments (mappend
                     (lambda+ ((name &optional value))
                       (list* (format nil "--~A~A" prefix name)
                              (when value (list value))))
                     '(("foo" "3") ("bar=true") ("foo-fez" "5")
                       ("bar-fez=/whoop") ("baz-foo=")))))
    (with-source-and-sink ((:commandline :prefix    prefix
                                         :arguments arguments)
                           :sink-var sink
                           :schema   *simple-schema*)
      (expecting-sink-calls (sink)
        '(:added     ("foo" "fez") nil)
        '(:new-value ("foo" "fez") 5)
        '(:added     ("foo")       nil)
        '(:new-value ("foo")       3)
        '(:added     ("baz" "foo") nil)
        '(:new-value ("baz" "foo") "")
        '(:added     ("bar" "fez") nil)
        `(:new-value ("bar" "fez") ,#P"/whoop")
        '(:added     ("bar")       nil)
        '(:new-value ("bar")       t)))))
