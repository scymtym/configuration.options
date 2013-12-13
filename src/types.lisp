;;;; types.lisp --- Types used by the options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options)

;;; Name-related types

(deftype non-wild-name-component ()
  "Valid components of a non-wild option name."
  'string)

(deftype wild-name-component ()
  "Wild components of an option name (like in pathnames)."
  '(member :wild :wild-inferiors))

(deftype name-component ()
  "Valid components of an option name."
  '(or non-wild-name-component wild-name-component))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %every-element-name-component? (thing)
    (cond
      ((typep thing 'sequence)
       (every (of-type 'name-component) thing))
      #-sbcl
      ((typep thing 'wildcard-name)
       (every (of-type 'name-component) (name-components thing)))))

  (defun %has-wild-component? (thing)
    (cond
      ((typep thing 'sequence)
       (some (of-type 'wild-name-component) thing))
      #-sbcl
      ((typep thing 'wildcard-name)
       (some (of-type 'wild-name-component) (name-components thing))))))

(deftype wild-name ()
  "A `name' which has at least one wild component."
  '(and name (satisfies %has-wild-component?)))

(deftype name ()
  "A sequence of name components some of which may be wild."
  '(and #+sbcl sequence
        #-sbcl (or sequence wildcard-name)
        (satisfies %every-element-name-component?)))

;;; Matching-related types

(deftype wildcard-interpretation ()
  `(member nil :query :container))
