;;;; name-grammar.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

#+later (esrap:defgrammar #:options.option-name
  (:documentation
   ""))
#+later (esrap:in-grammar #:options.option-name)

(esrap:defrule wild-inferiors
    (and #\* #\*)
  (:constant :wild-inferiors))

(esrap:defrule wild
    #\*
  (:constant :wild))

(esrap:defrule escaped/character
    (and #\\ (or #\" #\\))
  (:function second))

(esrap:defrule component/quoted
    (and #\" (+ (or escaped/character (not (or #\\ #\")))) #\")
  (:function second))

(esrap:defrule component/stringish
    (or component/quoted (+ (not #\.)))
  (:text t))

(esrap:defrule component
    (or wild-inferiors wild component/stringish))

(esrap:defrule dot-and-component
    (and #\. component)
  (:function second))

(esrap:defrule name
    (and component (* dot-and-component))
  (:destructure (first rest)
    (let ((components (cons first rest)))
      (if (every #'stringp components)
          components
          (make-instance 'wildcard-name
                         :components components)))))
