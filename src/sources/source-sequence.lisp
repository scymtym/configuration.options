;;;; sequence.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

(defun options-from-default-sources ()
  "Combine options from the following configuration sources:
+ ~/.config/rsb.conf
+ $(PWD)/rsb.conf
+ Environment Variables"
  (merge-options
   (options-from-environment)
   (with-input-from-file (stream "rsb.conf"
				 :if-does-not-exist nil)
     (when stream (options-from-stream stream)))
   (with-input-from-file (stream "~/.config/rsb.conf"
				 :if-does-not-exist nil)
     (when stream (options-from-stream stream)))
   '(((:transport :spread :converter)
      . (:fundamental-utf-8-string :fundamental-bytes
	 :fundamental-double :fundamental-float
	 :fundamental-int32  :fundamental-int64
	 :fundamental-uint32 :fundamental-uint64)))))
