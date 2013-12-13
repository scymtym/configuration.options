;;;; source-defaults.lisp --- Unit tests for the cascade source.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

;;; Tests for `default-source' class

(test default-source.smoke
  "Smoke test for `default-source' class."

  (let ((schema *simple-schema*))
    (with-source-and-sink ((:defaults)
                           :schema   schema
                           :sink-var sink)
      (are-expected-sink-calls
       (expected-notify-calls-for-schema-items schema)
       (sink-calls sink)))))

