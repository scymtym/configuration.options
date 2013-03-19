;;;; package.lisp --- Package definition for unit tests of the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:eos

   #:options)

  (:export
   #:options.root)

  (:documentation
   "This package contains unit tests for the options system"))

(cl:in-package #:options.test)

(def-suite options
  :description
  "Root unit test suite for the options system.")

(defun run-tests ()
  (run! 'options))

(defclass mock-source ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod push-options ((source mock-source)
                         (sink   t))
  "TODO(jmoringe): document"
  (mapc
   (lambda+ ((name value))
     (let ((option (or (find-option name sink :if-does-not-exist nil)
                       (setf (find-option name sink)
                             (make-option (find-option name (configuration-schema sink)
                                                       :if-does-not-exist #'error)
                                          name)))))
       (setf (option-value option)
             (string->value (option-schema-item option) value))))
   '((("a" "b")                              "5")
     (("a" "b" "c")                          "WILD")
     (("a" "b" "c" "d")                      "bli")
     (("child" "a")                          "CHILD")
     #+no (("child" "b")                          "bli")
     (("rsb" "transport" "spread" "enabled") "true"))))

(let* ((s  (make-instance 'standard-schema))
       (s2 (make-instance 'standard-schema))
       (c  (make-instance 'standard-configuration
                          :schema s)))
  (setf (options::find-child (esrap:parse 'options::name "child.**") s) s2)

  (setf (find-option '("a") s2)
        (make-instance 'standard-schema-item
                       :name '("a")
                       :type '(member :child)))

  (setf (find-option '("a" "b") s)
        (make-instance 'standard-schema-item
                       :name '("a" "b")
                       :type 'integer))
  (setf (find-option '("a" "b" :wild-inferiors) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("a" "b" :wild-inferiors))
                       :type 'string))

  (setf (find-option '("a" "b" :wild) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("a" "b" :wild))
                       :type '(member :wild)))

  (setf (find-option '("rsb" "transport" :wild "enabled") s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("rsb" "transport" :wild "enabled"))
                       :type 'boolean))

  (setf (find-option '(:wild-inferiors) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '(:wild-inferiors))
                       :type '(member :a :b)))

  (push-options (make-instance 'mock-source) c)

  #+no (setf (find-option '("a" "b") c)
             (make-instance 'standard-option
                            :name        '("a" "b")
                            :schema-item ))

  (print(list
    c
    (options c)
    (find-option '("a" "b") c :if-does-not-exist nil)
    (find-options (make-instance 'wildcard-name
                                 :components '("a" "b" :wild-inferiors)) c))))
