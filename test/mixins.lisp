;;;; mixin.lisp --- Unit tests for mixins used by the options system.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

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

(test type-based-validation-mixin.smoke
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
           "~S is~:[ not~;~] supposed to be of type ~S"
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
     ("foo" (or integer boolean) nil)

     (nil   (and real integer)   nil)
     (1     (and real integer)   t)
     (0.5d0 (and real integer)   nil))))

;;; `type-based-merging-mixin'

(def-suite options.type-based-merging-mixin
  :in options)
(in-suite options.type-based-merging-mixin)

(defclass mock-type-based-merging-schema-item (mock-typed-schema-item
                                               type-based-merging-mixin)
  ())

(test type-based-merging-mixin.smoke
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

(test type-based-conversion-mixin.smoke
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

   `((boolean                    ""      option-syntax-error)
     (boolean                    "1"     option-syntax-error)
     (boolean                    "false" nil)
     (boolean                    "true"  t)

     (integer                    ""      option-syntax-error)
     (integer                    "true"  option-syntax-error)
     (integer                    "1"     1)
     (integer                    "2"     2)

     (string                     "a"     "a")
     (string                     "a b"   "a b")

     ((member :foo :bar)         ""      option-syntax-error)
     ((member :foo :bar)         "1"     option-syntax-error)
     ((member :foo :bar)         "BAZ"   option-syntax-error)
     ((member :foo :bar)         "foo"   :foo)
     ((member :foo :bar)         "bar"   :bar)
     ((member :|Foo| :|Bar|)     "Foo"   :|Foo|)
     ((member :|Foo| :|Bar|)     "Bar"   :|Bar|)
     ((member :|foo| :|bar|)     "FOO"   :|foo|)
     ((member :|foo| :|bar|)     "BAR"   :|bar|)

     (pathname                   "a"     ,#P"a")
     (pathname                   "a.b"   ,#P"a.b")
     (pathname                   "a/b"   ,#P"a/b")

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
     ((and real integer)         "2"     2)

     ;; These require type expansion
     (positive-integer           "1"     1)
     (positive-integer           "0"     option-syntax-error)
     ((array-index 10)           "1"     1)
     ((array-index 10)           "10"    option-syntax-error))))

;;; `list-container-mixin'

(def-suite options.list-container-mixin
  :in options)
(in-suite options.list-container-mixin)

(test list-container.map-options.smoke
  "Smoke test for the `map-options' method."

  (let+ (((&flet test-case (container expected-calls)
            (is (equal expected-calls
                       (collect-map-options-calls container))))))
    ;; Empty container.
    (test-case (make-instance 'list-container-mixin) '())
    ;; One option.
    (let ((container (make-instance 'list-container-mixin))
          (option    *simple-option*))
      (setf (find-option "simple.option" container) option)
      (test-case container `((,option))))))

(test list-container.find-options.smoke
  "Smoke test for the `find-options' method."

  (macrolet
      ((test-case (&body body)
         `(let+ ((container (make-instance 'list-container-mixin))
                 ((&flet check-query (expected query)
                    (let ((result (find-options query container)))
                      (is (equal expected (mapcar #'option-name result)))))))
            ,@body)))

    ;; Empty results.
    (test-case
     (check-query '() "no.such.option")
     (check-query '() '("no" "such" "option")))

    ;; [Wild-]inferiors queries.
    (test-case
     (setf (find-option "option" container) (simple-option :name "option"))
     (check-query '(("option")) '(:wild))
     (check-query '(("option")) "**"))

    (test-case
     (setf (find-option "simple.option" container) (simple-option))
     (check-query '()                    '(:wild))
     (check-query '(("simple" "option")) "**"))

    (test-case
     (setf (find-option "option" container)
           (simple-option :name "option")
           (find-option "simple.option" container)
           (simple-option :name "simple.option"))
     (check-query '(("option"))                     '(:wild))
     (check-query '(("simple" "option") ("option")) "**"))))

(test list-container.find-option.smoke
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
     (signals option-missing-error
       (find-option "no.such.option" container)))
    (test-case
     (is (not (find-option "no.such.option" container
                           :if-does-not-exist nil))))
    (test-case
     (is (eq (handler-bind ((option-missing-error
                              (lambda (condition)
                                (declare (ignore condition))
                                (let ((restart (find-restart 'retry)))
                                  (is-true restart)
                                  (is (not (emptyp (princ-to-string restart)))))
                                (let ((restart (find-restart 'use-value)))
                                  (is-true restart)
                                  (is (not (emptyp (princ-to-string restart))))
                                  (invoke-restart restart :foo)))))
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
