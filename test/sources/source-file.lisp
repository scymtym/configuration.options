;;;; source-file.lisp --- Unit tests for the file source.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test file-source.smoke
  "Smoke test for `file-source' class."

  (for-all ((name (gen-ascii-name)))
    (let ((path (format nil "/tmp/~A.conf" name)))
      (with-file (path "a=1 b.c=2")
        (with-source-and-sink ((:file :syntax :mock :pathname path)
                               :sink-var sink)
          (expecting-sink-calls (sink)
            '(:added     ("a")     nil)
            '(:new-value ("a")     "1")
            '(:added     ("b" "c") nil)
            '(:new-value ("b" "c") "2")))))))
