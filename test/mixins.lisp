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

(test type-based-validation-mixin.smoke
  "Smoke test for methods on `validate-value' and
   `validate-value-using-type' for `type-based-validation-mixin'."
  (mapc (curry #'apply #'check-validate-value)
        '(;; Atomic type specifier
          (nil   integer          nil)
          (t     integer          nil)
          (1     integer          t)
          (0.5d0 integer          nil)
          ("foo" integer          nil)

          ;; Compound type specifier
          (nil   (integer 0 1)    nil)
          (t     (integer 0 1)    nil)
          (1     (integer 0 1)    t)
          (0.5d0 (integer 0 1)    nil)
          ("foo" (integer 0 1)    nil)

          ;; These require type expansion
          (1     positive-integer t)
          (0     positive-integer nil)
          (1     (array-index 10) t)
          (10    (array-index 10) nil))))

;;; `type-based-merging-mixin'

(def-suite options.type-based-merging-mixin
  :in options)
(in-suite options.type-based-merging-mixin)

(test type-based-merging-mixin.smoke
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

;;; `type-based-conversion-mixin'

(def-suite options.type-based-conversion-mixin
  :in options)
(in-suite options.type-based-conversion-mixin)

(test type-based-conversion-mixin.smoke
  "Smoke test for methods on `value->string', `raw->value',
   `value->string-using-type' and `raw->value-using-type' for
   `type-based-conversion-mixin'."
  (mapc
   (curry #'apply #'check-value<->string)
   `(;; Atomic type specifier
     (integer          ""      option-syntax-error)
     (integer          "true"  option-syntax-error)
     (integer          "1"     1)
     (integer          "2"     2)

     ;; Compound type specifier
     ((integer 0 1)    ""      option-syntax-error)
     ((integer 0 1)    "true"  option-syntax-error)
     ((integer 0 1)    "1"     1)
     ((integer 0 1)    "2"     option-syntax-error)

     ;; These require type expansion
     (positive-integer "1"     1)
     (positive-integer "0"     option-syntax-error)
     ((array-index 10) "1"     1)
     ((array-index 10) "10"    option-syntax-error))))

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

(test sub-configuration.smoke
  "Smoke test for the `sub-configuration' function."

  :TODO)
