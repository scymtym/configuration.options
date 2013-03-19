;;;; types.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

(deftype name-component ()
  "TODO(jmoringe): document"
  '(or string (eql :wild) (eql :wild-inferiors)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun every-element-name-component? (thing)
    "TODO(jmoringe): document"
    (when (typep thing 'sequence)
      (every (of-type 'name-component) thing))))

(deftype name ()
  "TODO(jmoringe): document"
  '(and sequence (satisfies every-element-name-component?)))
