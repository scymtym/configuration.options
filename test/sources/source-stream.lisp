;;;; source-stream.lisp --- Unit tests for the stream source.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test stream-source.smoke
  "Smoke test for `stream-source' class."

  (with-input-from-string (stream "a.b=1 a=2")
    (with-source-and-sink ((:stream :stream stream :syntax :mock)
                           :sink-var sink)
      (expecting-sink-calls (sink)
        '(:added     ("a" "b") nil)
        '(:new-value ("a" "b") "1")
        '(:added     ("a")     nil)
        '(:new-value ("a")     "2")))))
