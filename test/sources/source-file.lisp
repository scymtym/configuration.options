;;;; source-file.lisp --- Unit tests for the file source.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

(in-suite options.sources)

(test file-source.smoke
  "Smoke test for `file-source' class."

  (let ((name (format nil "/tmp/~A/.conf" (make-random-string))))
    (with-file (name "a=1 b.c=2")
      (with-source-and-sink ((:file :syntax :mock :pathname name)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          '(:added     ("a")     nil)
          '(:new-value ("a")     "1")
          '(:added     ("b" "c") nil)
          '(:new-value ("b" "c") "2"))))))
