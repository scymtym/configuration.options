;;;; source-stream.lisp --- Unit tests for the stream source.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

(in-suite options.sources)

(test stream-source.smoke

  (with-input-from-string (stream "a.b=1 a=2")
    (with-source-and-sink ((:stream :stream stream :syntax :mock)
                           :sink-var sink)
      (expecting-sink-calls (sink)
        '(:new-value ("a" "b") "1")
        '(:new-value ("a")     "2")))))
