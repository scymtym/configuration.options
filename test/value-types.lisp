;;;; value-types.lisp --- Tests for validation and conversion of builtin types.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(def-suite options.value-types
  :in options
  :description
  "Test suite for validation and conversion methods for builtin
   types.")
(in-suite options.value-types)

(test builtin-types.validate-value.smoke
  "Smoke test for methods on `validate-value' for builtin types."

  (mapc
   (curry #'apply #'check-validate-value)
   '((nil            null                       t)
     (t              null                       nil)
     (1              null                       nil)
     (0.5d0          null                       nil)
     ("foo"          null                       nil)

     (nil            integer                    nil)
     (t              integer                    nil)
     (1              integer                    t)
     (0.5d0          integer                    nil)
     ("foo"          integer                    nil)

     (nil            (integer 0 1)              nil)
     (t              (integer 0 1)              nil)
     (1              (integer 0 1)              t)
     (0.5d0          (integer 0 1)              nil)
     ("foo"          (integer 0 1)              nil)

     (nil            (integer 0 (1))            nil)
     (t              (integer 0 (1))            nil)
     (1              (integer 0 (1))            nil)
     (0.5d0          (integer 0 (1))            nil)
     ("foo"          (integer 0 (1))            nil)

     (nil            (real 0 (1))               nil)
     (t              (real 0 (1))               nil)
     (1              (real 0 (1))               nil)
     (0.5d0          (real 0 (1))               t)
     ("foo"          (real 0 (1))               nil)

     (nil            string                     nil)
     (1              string                     nil)
     (""             string                     t)
     ("foo"          string                     t)

     (nil            (string 3)                 nil)
     (1              (string 3)                 nil)
     (""             (string 3)                 nil)
     ("foo"          (string 3)                 t)

     (:a             (member :a)                t)
     (:b             (member :a)                nil)
     (:a             (member :a :b)             t)
     (:b             (member :a :b)             t)

     (()             list                       t)
     ((1 2)          list                       t)
     ((1 "a")        list                       t)
     ("a"            list                       nil)

     (t              (list integer)             nil)
     (1              (list integer)             nil)
     (()             (list integer)             t)
     ((:inherit)     (list integer)             nil)
     ((1)            (list integer)             t)
     ((1 2)          (list integer)             t)
     ((1 2 :inherit) (list integer)             nil)

     (t              (list integer :inherit? t) nil)
     (1              (list integer :inherit? t) nil)
     (()             (list integer :inherit? t) t)
     ((:inherit)     (list integer :inherit? t) nil)
     ((1)            (list integer :inherit? t) t)
     ((1 2)          (list integer :inherit? t) t)
     ((1 2 :inherit) (list integer :inherit? t) nil)

     (nil            (or integer boolean)       t)
     (t              (or integer boolean)       t)
     (1              (or integer boolean)       t)
     (0.5d0          (or integer boolean)       nil)
     ("foo"          (or integer boolean)       nil)

     (nil            (and real integer)         nil)
     (1              (and real integer)         t)
     (0.5d0          (and real integer)         nil))))

(test builtin-types.merge-values.smoke
  "Smoke test for methods on `merge-values' and
   `merge-values-using-type' for `type-based-merging-mixin'."
  (mapc
   (curry #'apply #'check-merge-values)

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

(test builtin-types.value<->string.smoke
  "Smoke test for methods on `value->string', `raw->value',
   `value->string-using-type' and `raw->value-using-type' for
   `type-based-conversion-mixin'."
  (mapc
   (lambda+ ((type string value &optional (string2 string)))
     (check-value<->string type string value :string2 string2))

   `((null                       ""                 option-syntax-error)
     (null                       "1"                option-syntax-error)
     (null                       "nil"              nil)

     (boolean                    ""                 option-syntax-error)
     (boolean                    "1"                option-syntax-error)
     (boolean                    "false"            nil)
     (boolean                    "true"             t)

     (integer                    ""                 option-syntax-error)
     (integer                    "true"             option-syntax-error)
     (integer                    "1"                1)
     (integer                    "2"                2)

     (string                     "a"                "a")
     (string                     "a b"              "a b")

     ((member :foo :bar)         ""                 option-syntax-error)
     ((member :foo :bar)         "1"                option-syntax-error)
     ((member :foo :bar)         "BAZ"              option-syntax-error)
     ((member :foo :bar)         "foo"              :foo)
     ((member :foo :bar)         "bar"              :bar)
     ((member :|Foo| :|Bar|)     "Foo"              :|Foo|)
     ((member :|Foo| :|Bar|)     "Bar"              :|Bar|)
     ((member :|foo| :|bar|)     "FOO"              :|foo|)
     ((member :|foo| :|bar|)     "BAR"              :|bar|)

     (pathname                   "a"                ,#P"a")
     (pathname                   "a.b"              ,#P"a.b")
     (pathname                   "a/b"              ,#P"a/b")

     ((list integer)             ""                 option-syntax-error)
     ((list integer)             "1"                (1))
     ((list integer)             "1:2"              (1 2))

     ((list integer :inherit? t) ""                 option-syntax-error)
     ((list integer :inherit? t) ":"                (:inherit))
     ((list integer :inherit? t) "1"                (1))
     ((list integer :inherit? t) "1:"               (1 :inherit))
     ((list integer :inherit? t) "1:2"              (1 2))
     ((list integer :inherit? t) "1:2:"             (1 2 :inherit))

     ((list integer :inherit? t) ()                 ()             "")
     ((list integer :inherit? t) (:inherit)         (:inherit)     ":")
     ((list integer :inherit? t) ("1")              (1)            "1")
     ((list integer :inherit? t) ("1" :inherit)     (1 :inherit)   "1:")
     ((list integer :inherit? t) ("1" "2")          (1 2)          "1:2")
     ((list integer :inherit? t) ("1" "2" :inherit) (1 2 :inherit) "1:2:")

     ((or boolean integer)       ""                 option-syntax-error)
     ((or boolean integer)       "FOO"              option-syntax-error)
     ((or boolean integer)       "false"            nil)
     ((or boolean integer)       "true"             t)
     ((or boolean integer)       "1"                1)
     ((or boolean integer)       "2"                2)

     ((and real integer)         ""                 option-syntax-error)
     ((and real integer)         "FOO"              option-syntax-error)
     ((and real integer)         "1"                1)
     ((and real integer)         "2"                2))))
