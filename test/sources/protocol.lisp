;;;; protocol.lisp --- Tests for protocol functions of the sources module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test initialize.default-behavior
  "Test for default behavior of `initialize' function."

  (let ((source (make-instance 'mock-source)))
    ;; An arbitrary error signaled by a sink during `initialize'
    ;; should be wrapped in a `initialization-error'.
    (signals initialization-error
      (initialize source :intentional-error))))

(test notify.default-behavior
  "Test for default behavior of `notify' function."

  (let ((sink (make-instance 'mock-sink)))
    (initialize sink :does-not-matter)
    ;; An arbitrary error signaled by a sink during `notify' should be
    ;; wrapped in a `notification-error'.
    (signals notification-error
      (notify sink :intentional-error nil nil))))
