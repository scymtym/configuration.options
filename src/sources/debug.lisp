;;;; debug.lisp --- Debugging of configuration sources.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defvar *debug-index* nil)

(defun call-with-source-debug (thunk format-control format-arguments)
  (configuration.options.debug:with-level (:next)
    (let ((indent        (cond
                           (*debug-index*
                            (output "~:[~;~@:_~]~D. ~?~@:_"
                                    (> configuration.options.debug::*level* 0)
                                    *debug-index*
                                    format-control format-arguments)
                            3)
                           (t
                            (output "~?~%" format-control format-arguments)
                            2)))
          (*debug-index* nil))
      (configuration.options.debug:call-with-indent
       indent thunk))))

(defmacro with-source-debug ((format-control &rest format-arguments)
                             &body forms)
  `(call-with-source-debug (lambda () ,@forms)
                           ,format-control (list ,@format-arguments)))
