;;;; util.lisp --- Utilities used by the configuration.options system.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

(defun print-documentation (stream documentation &optional colon? at?)
  "`pprint-fill' the words in DOCUMENTATION onto STREAM."
  (declare (ignore colon? at?))
  (let ((*print-escape* nil))
    (pprint-fill stream (split-sequence #\Space documentation) nil)))

(defun typexpand-1 (type-specifier)
  #+sbcl          (sb-ext:typexpand-1 type-specifier)
  #+ccl           (let ((expanded (ccl::type-expand type-specifier)))
                    (values expanded (not (eq expanded type-specifier))))
  #-(or sbcl ccl) (error "not implemented"))

(defun typexpand-1-unless-builtin (type-specifier)
  (let ((symbol (typecase type-specifier
                  (symbol type-specifier)
                  (cons   (first type-specifier)))))
    (if (eq (symbol-package symbol)
            (load-time-value (find-package '#:common-lisp) t))
        type-specifier
        (typexpand-1 type-specifier))))
