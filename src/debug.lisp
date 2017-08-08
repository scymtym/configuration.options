;;;; debug.lisp --- Debugging of configuration processing.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options.debug
  (:use
   #:cl
   #:alexandria)

  ;; Activating debugging
  (:export
   #:+config-debug-variable-suffix+
   #:config-debug-variable-name
   #:enable-debugging
   #:maybe-enable-debugging)

  ;; Output
  (:export
   #:output)

  ;; Macros
  (:export
   #:call-with-level  #:with-level
   #:call-with-indent #:with-indent)

  (:documentation
   "Functions for debugging configuration processing."))

(cl:in-package #:configuration.options.debug)

;;; Debug state

(defvar *stream* nil)

(defvar *level* nil)

;;; Activating debugging

(define-constant +config-debug-variable-suffix+
    "CONFIG_DEBUG"
  :test #'string=
  :documentation
  "Suffix for the name of the environment variable controlling
   configuration debugging.")

(defun config-debug-variable-name (prefix)
  "Return environment variable name derived from PREFIX."
  (format nil "~A~A" prefix +config-debug-variable-suffix+))

(defun enable-debugging (stream)
  "Enable configuration debugging using stream."
  (setf *stream* stream))

(defun maybe-enable-debugging (prefix &key (stream *error-output*))
  "Enable configuration debugging according to environment variable.

   PREFIX is used with `config-debug-variable-name' to compute the
   name of the environment variable.

   STREAM is used for debug output if debugging is enabled."
  (when (uiop:getenvp (config-debug-variable-name prefix))
    (enable-debugging stream)
    t))

;; Output

(defun output (format-control &rest format-arguments)
  "Output FORMAT-CONTROL and FORMAT-ARGUMENTS if debugging."
  (when-let ((stream *stream*))
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (apply #'format stream format-control format-arguments)))))

;; Macros

(defun call-with-level (level thunk)
  "Call THUNK with debug level LEVEL.

   LEVEL can an integer or `:next' to increase the current level by
   one."
  (let ((*level* (case level
                   (:next (1+ (or *level* -1)))
                   (t     level))))
    (funcall thunk)))

(defmacro with-level ((level) &body body)
  "Execute BODY with debug level LEVEL.

   LEVEL can an integer or `:next' to increase the current level by
   one."
  `(call-with-level ,level (lambda () ,@body)))

(defun call-with-indent (amount thunk)
  "Call THUNK with debug indentation increased by AMOUNT."
  (if *stream*
      (let ((prefix (make-string amount :initial-element #\Space)))
        (pprint-logical-block (*stream* nil :per-line-prefix prefix)
          (funcall thunk))
        (when (zerop *level*)
          (terpri *stream*)))
      (funcall thunk)))

(defmacro with-indent ((amount) &body body)
  "Execute BODY with debug indentation increased by AMOUNT."
  `(call-with-indent ,amount (lambda () ,@body)))
