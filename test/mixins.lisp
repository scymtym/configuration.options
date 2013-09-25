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

;;; `type-based-merging-mixin'

(def-suite options.type-based-merging-mixin
  :in options)
(in-suite options.type-based-merging-mixin)

(defclass mock-type-based-merging-schema-item (mock-typed-schema-item
                                               type-based-merging-mixin)
  ())

(test smoke
  "Smoke test for methods on `merge-values' and
   `merge-values-using-type' for `type-based-merging-mixin'."
  (mapc
   (lambda+ ((values type expected))
     (let+ ((schema-item (make-instance
                          'mock-type-based-merging-schema-item
                          :type type)))
       (is (equal expected
                  (multiple-value-list
                   (merge-values schema-item values))))))

   '((()                           boolean                    (nil   nil))
     ((nil)                        boolean                    (nil   t))
     ((t)                          boolean                    (t     t))
     ((nil nil)                    boolean                    (nil   t))
     ((t   nil)                    boolean                    (t     t))

     (()                           integer                    (nil   nil))
     ((1)                          integer                    (1     t))
     ((2)                          integer                    (2     t))
     ((1 1)                        integer                    (1     t))
     ((2 1)                        ingeter                    (2     t))

     (()                           (list integer)             (nil   nil))
     (((1))                        (list integer)             ((1)   t))
     (((1) (2))                    (list integer)             ((1)   t))

     (()                           (list integer :inherit? t) (nil   nil))
     (((1))                        (list integer :inherit? t) ((1)   t))
     (((1 :inherit))               (list integer :inherit? t) ((1)   t))
     (((1) (2))                    (list integer :inherit? t) ((1)   t))
     (((1 :inherit) (2))           (list integer :inherit? t) ((1 2) t))
     (((1) (2  :inherit))          (list integer :inherit? t) ((1)   t))
     (((1 :inherit) (2  :inherit)) (list integer :inherit? t) ((1 2) t)))))

;;; `type-based-conversion-mixin'

(def-suite options.type-based-conversion-mixin
  :in options)
(in-suite options.type-based-conversion-mixin)

(defclass mock-type-based-conversion-schema-item (mock-typed-schema-item
                                                  type-based-conversion-mixin)
  ())

(test smoke
  "Smoke test for methods on `value->string', `string->value',
   `value->string-using-type' and `string->value-using-type' for
   `type-based-conversion-mixin'."
  (mapc
   (lambda+ ((type string value))
     (let+ ((schema-item (make-instance
                          'mock-type-based-conversion-schema-item
                          :type type)))
       (case value
         (option-syntax-error
          (signals option-syntax-error
            (string->value schema-item string)))
         (t
          (is (equal string (value->string schema-item value)))
          (is (equal value  (string->value schema-item string)))))))

   '((boolean                    ""      option-syntax-error)
     (boolean                    "1"     option-syntax-error)
     (boolean                    "false" nil)
     (boolean                    "true"  t)

     (integer                    ""      option-syntax-error)
     (integer                    "true"  option-syntax-error)
     (integer                    "1"     1)
     (integer                    "2"     2)

     ((member :foo :bar)         ""      option-syntax-error)
     ((member :foo :bar)         "1"     option-syntax-error)
     ((member :foo :bar)         "BAZ"   option-syntax-error)
     ((member :foo :bar)         "FOO"   :foo)
     ((member :foo :bar)         "BAR"   :bar)
     ((member :|foo| :|bar|)     "foo"   :|foo|)
     ((member :|foo| :|bar|)     "bar"   :|bar|)

     ((list integer)             ""      option-syntax-error)
     ((list integer)             "1"     (1))
     ((list integer)             "1:2"   (1 2))

     ((list integer :inherit? t) ""      option-syntax-error)
     ((list integer :inherit? t) ":"     (:inherit))
     ((list integer :inherit? t) "1"     (1))
     ((list integer :inherit? t) "1:"    (1 :inherit))
     ((list integer :inherit? t) "1:2"   (1 2))
     ((list integer :inherit? t) "1:2:"  (1 2 :inherit))

     ((or boolean integer)       ""      option-syntax-error)
     ((or boolean integer)       "FOO"   option-syntax-error)
     ((or boolean integer)       "false" nil)
     ((or boolean integer)       "true"  t)
     ((or boolean integer)       "1"     1)
     ((or boolean integer)       "2"     2)

     ((and real integer)         ""      option-syntax-error)
     ((and real integer)         "FOO"   option-syntax-error)
     ((and real integer)         "1"     1)
     ((and real integer)         "2"     2))))

;;; `list-container-mixin'

(def-suite options.list-container-mixin
  :in options)
(in-suite options.list-container-mixin)

(test find-option.smoke
  "Smoke test for the `find-option' and setf find-option functions."

  (macrolet
      ((test-case (&body body)
         `(let* ((container   (make-instance 'list-container-mixin))
                 (schema-item (make-instance 'standard-schema-item
                                             :name "foo"
                                             :type 'integer))
                 (option      (make-option schema-item (make-name "foo"))))
            (declare (ignorable option))
            ,@body)))

    ;; Reader
    (test-case
     (signals no-such-option
       (find-option "no.such.option" container)))
    (test-case
     (is (not (find-option "no.such.option" container
                           :if-does-not-exist nil))))
    (test-case
     (is (eq (handler-bind ((no-such-option
                              (lambda (condition)
                                (declare (ignore condition))
                                (invoke-restart 'use-value :foo))))
               (find-option "no.such.options" container))
             :foo)))

    ;; Writer
    (test-case
      (is (eq option (setf (find-option "foo" container) option)))
      (is (equal (list option) (options container)))
      (signals error
        (setf (find-option "foo" container) option))
      (setf (find-option "foo" container :if-exists :keep)
            option)
      (setf (find-option "foo" container :if-exists :supersede)
            option))))
