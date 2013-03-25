;;;; types.lisp --- Types used by the options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

(deftype wild-name-component ()
  "Wild components of an option name (like in pathnames)."
  '(or (eql :wild) (eql :wild-inferiors)))

(deftype name-component ()
  "Valid components of an option name."
  '(or string wild-name-component))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %every-element-name-component? (thing)
    (when (typep thing 'sequence)
      (every (of-type 'name-component) thing)))

  (defun %has-wild-component? (thing)
    (when (typep thing 'sequence)
      (some (of-type 'wild-name-component) thing))))

(deftype name ()
  "A sequence of name components some of which may be wild."
  '(and sequence (satisfies %every-element-name-component?)))

(deftype wild-name ()
  "A `name' which has at least one wild component."
  '(and name (satisfies %has-wild-component?)))
