;;;; mixin.lisp --- Unit tests for mixins used by the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(defclass mock-typed-schema-item ()
  ((type :initarg  :type
         :reader    option-type)))

;;; `type-based-validation-mixin'

(def-suite options.type-based-validation-mixin
  :in options)
(in-suite options.type-based-validation-mixin)

(defclass mock-type-based-validation-schema-item (mock-typed-schema-item
                                                  type-based-validation-mixin)
  ())

(test smoke
  "Smoke test for methods on `validate-value' and
   `validate-value-using-type' for `type-based-validation-mixin'."
  (mapc
   (lambda+ ((value type expected))
     (let+ ((schema-item (make-instance
                          'mock-type-based-validation-schema-item
                          :type type))
            ((&flet do-it (&key (if-invalid nil))
               (validate-value schema-item value :if-invalid if-invalid))))
       (is (eq expected (do-it))
           "~S is~:[ not~:;~] supposed to be of type ~S"
           value expected type)
       (when (not expected)
         (signals option-value-error (do-it :if-invalid #'error)))))

   '((nil   integer              nil)
     (t     integer              nil)
     (1     integer              t)
     (0.5d0 integer              nil)
     ("foo" integer              nil)

     (nil   (integer 0 1)        nil)
     (t     (integer 0 1)        nil)
     (1     (integer 0 1)        t)
     (0.5d0 (integer 0 1)        nil)
     ("foo" (integer 0 1)        nil)

     (nil   (integer 0 (1))      nil)
     (t     (integer 0 (1))      nil)
     (1     (integer 0 (1))      nil)
     (0.5d0 (integer 0 (1))      nil)
     ("foo" (integer 0 (1))      nil)

     (nil   (real 0 (1))         nil)
     (t     (real 0 (1))         nil)
     (1     (real 0 (1))         nil)
     (0.5d0 (real 0 (1))         t)
     ("foo" (real 0 (1))         nil)

     (nil   (or integer boolean) t)
     (t     (or integer boolean) t)
     (1     (or integer boolean) t)
     (0.5d0 (or integer boolean) nil)
     ("foo" (or integer boolean) nil))))
