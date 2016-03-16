;;;; package.lisp --- Package definition for sources module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.sources
  (:nicknames
   #:options.sources)

  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items

   #:configuration.options)

  (:import-from #:configuration.options
   #:wild-name)

  ;; Condition
  (:export
   #:schema-condition
   #:schema-condition-schema

   #:source-condition
   #:source-condition-source

   #:sink-condition
   #:sink-condition-sink

   #:initialization-error

   #:processing-error)

  ;; Source protocol and source creation
  (:export
   #:initialize
   #:process

   #:make-source)

  ;; Cascade source protocol
  (:export
   #:source-sources)

  ;; Syntax protocol and syntax creation
  (:export
   #:process-content

   #:make-syntax)

  (:documentation
   "This package contains configuration options sources.

    Sources such as configuration files, environment variables and
    commandline options provide option discovery and option value
    events to sinks (usually a synchronizer) which usually integrate
    the information into a configuration object."))

(cl:in-package #:configuration.options.sources)

;; TODO until alexandria provides these
#.(unless (find-symbol (string '#:read-stream-content-into-byte-vector)
                       '#:alexandria)
    '(progn
      (defun read-stream-content-into-byte-vector (stream &key length (initial-size 4096))
        "Return \"contents\" of STREAM as freshly allocated (unsigned-byte 8) vector."
        (check-type length (or null non-negative-integer))
        (do ((buffer (make-array (or length initial-size)
                                 :element-type '(unsigned-byte 8)))
             (offset 0)
             (offset-wanted 0))
            ((or (/= offset-wanted offset)
                 (and length (>= offset length)))
             (if (= offset (length buffer))
                 buffer
                 (subseq buffer 0 offset)))
          (unless (zerop offset)
            (let ((new-buffer (make-array (* 2 (length buffer))
                                          :element-type '(unsigned-byte 8))))
              (replace new-buffer buffer)
              (setf buffer new-buffer)))
          (setf offset-wanted (length buffer)
                offset (read-sequence buffer stream :start offset))))

      (defun read-stream-content-into-string (stream &key (buffer-size 4096))
        "Return the \"contents\" of STREAM as a fresh string."
        (let ((*print-pretty* nil))
          (with-output-to-string (datum)
            (let ((buffer (make-array buffer-size :element-type 'character)))
              (loop
                 :for bytes-read = (read-sequence buffer stream)
                 :do (write-sequence buffer datum :start 0 :end bytes-read)
                 :while (= bytes-read buffer-size))))))))
