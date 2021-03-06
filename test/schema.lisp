;;;; schema.lisp --- Unit tests for schema-related stuff.
;;;;
;;;; Copyright (C) 2013, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

;;; `standard-schema' class

(def-suite standard-schema
  :in options
  :description
  "Test suite for the `standard-schema' class.")
(in-suite standard-schema)

(test standard-schema.map-options.smoke
  "Smoke test for the `map-options' method."

  (let+ (((&flet test-case (container expected-calls)
            (is (set-equal/equal expected-calls
                                 (collect-map-options-calls container))))))
    ;; Empty schema.
    (test-case (make-instance 'standard-schema) '())
    ;; A few schema items.
    (let* ((sub-schema *simple-sub-schema*)
           (schema     *simple-schema*)
           (expected   (append
                        (loop :for item :in (options schema)
                           :collect (list item
                                          :container schema
                                          :prefix    '()))
                        (loop :for item :in (options sub-schema)
                           :collect (list item
                                          :container sub-schema
                                          :prefix    '("sub"))
                           :collect (list item
                                          :container sub-schema
                                          :prefix    '("sub" "sub"))))))
      (test-case schema expected))))

(test standard-schema.find-options.smoke
  "Smoke test for the `find-options' method."
  ;; This test exercises finding options in sub-schemas (and is
  ;; therefore not redundant given the general `find-options' test in
  ;; protocol.lisp).

  (let+ (((&flet check-query (query-and-args expected container)
            (let+ (((query &rest args) (ensure-list query-and-args))
                   (result (apply #'find-options query container args)))
              (is (set-equal/name-equal
                   expected (mapcar #'option-name result)))))))
    (macrolet
        ((test-case ((container) &body body)
           `(let+ ((container ,container)
                   ((&flet check-query (query-and-args expected)
                      (check-query query-and-args expected container))))
              ,@body)))

      ;; Empty results.
      (test-case ((make-instance 'standard-schema))
        (check-query "no.such.option" '()))

      ;; Complex queries.
      (test-case (*simple-schema*)
        ;; Single component and wild[-inferiors] queries.
        (check-query "foo" '("foo"))
        (check-query "*"   '("*" "foo" "bar"))
        ;; "*" is returned twice (not checked by
        ;; `set-equal/name-equal') because the parent schema and the
        ;; sub-schema both have an option named (:wild)
        (check-query "**"  '("*" "whoop" "*" "wild.**" "foo.fez" "foo"
                             "baz.foo" "bar.fez" "bar"))

        (check-query '("foo" :interpret-wildcards? :container) '("*" "foo"))
        (check-query '("*"   :interpret-wildcards? :container) '("*"))
        (check-query '("**"  :interpret-wildcards? :container) '("*"))

        (check-query '("foo" :interpret-wildcards? nil)        '("foo"))
        (check-query '("*"   :interpret-wildcards? nil)        '("*"))
        (check-query '("**"  :interpret-wildcards? nil)        '())

        ;; Mixed queries.
        (check-query "**.bar"   '("bar"))
        (check-query "bar.**"   '("bar.fez" "bar"))
        (check-query "wild.foo" '())
        (check-query "sub.*"    '("*" "whoop"))

        (check-query '("**.bar"   :interpret-wildcards? :container) '())
        (check-query '("bar.**"   :interpret-wildcards? :container) '())
        (check-query '("wild.foo" :interpret-wildcards? :container) '("wild.**"))
        (check-query '("sub.*"    :interpret-wildcards? :container) '("*"))

        (check-query '("**.bar"   :interpret-wildcards? nil)        '())
        (check-query '("bar.**"   :interpret-wildcards? nil)        '())
        (check-query '("wild.foo" :interpret-wildcards? nil)        '())
        (check-query '("sub.*"    :interpret-wildcards? nil)        '("*"))))))

(test standard-schema.find-child.smoke
  "Smoke test for the `find-child' and (setf find-child) functions."

  (macrolet
      ((test-case (&body body)
         `(let ((schema (make-instance 'standard-schema))
                (child  (make-instance 'standard-schema)))
            (declare (ignorable child))
            ,@body)))

    ;; Reader
    (test-case
     (signals child-missing-error
       (find-child "no.such.child" schema)))
    (test-case
     (is (not (find-child "no.such.child" schema
                          :if-does-not-exist nil))))
    (test-case
     (is (eq (handler-bind ((child-missing-error
                              (lambda (condition)
                                (declare (ignore condition))
                                (let ((restart (find-restart 'retry)))
                                  (is-true restart)
                                  (is (not (emptyp (princ-to-string restart)))))
                                (let ((restart (find-restart 'use-value)))
                                  (is-true restart)
                                  (is (not (emptyp (princ-to-string restart))))
                                  (invoke-restart restart :foo)))))
               (find-child "no.such.child" schema))
             :foo)))

    ;; Writer
    (test-case
      (is (eq child (setf (find-child "child" schema) child)))
      (is (equal (list child) (schema-children schema)))
      (signals error
        (setf (find-child "child" schema) child))
      (setf (find-child "child" schema :if-exists :keep)
            child)
      (setf (find-child "child" schema :if-exists :supersede)
            child))))

(test standard-schema.make-option.smoke
  "Smoke test for the `make-option' function."

  ;; Attempt to make an option with an unrelated name.
  (let ((item (first (options *simple-schema*))))
    (signals error (make-option item "completely.unrelated")))

  ;; Make some options. Expect errors when attempting to make options
  ;; with wild names.
  (mapc (lambda (item)
          (let ((name (option-name item)))
            (if (typep name 'wild-name)
                (signals error (make-option item name))
                (let ((option (make-option item name)))
                  (is (eq item (option-schema-item option)))))))
        (options *simple-schema*)))

(test standard-schema.make-configuration.smoke
  "Smoke test for the `make-configuration' function."

  (let ((configuration (make-configuration *simple-schema*)))
    ;; SCHEMA should be the `configuration-schema' of the newly
    ;; created CONFIGURATION.
    (is (eq (configuration-schema configuration) *simple-schema*))
    ;; There should be no options in the new configuration.
    (is (emptyp (options configuration)))))

(test standard-schema.type-list
  "Kind-of integrationtest for schema item with list-of-something
   type."

  (mapc
   (lambda+ ((input expected-parsed expected-value))
     (let* ((item   (make-instance 'standard-schema-item
                                   :name    '("a" "b")
                                   :type    '(list integer :inherit? t)
                                   :default '(-1)))
            (parsed (mapcar (curry #'raw->value item) input))
            (value  (merge-values item parsed)))
       (is (equal parsed expected-parsed))
       (is (equal value expected-value))))

   '((()            ()                            ())
     (("1")         ((1))                         (1))
     (("1:2" "3")   ((1 2) (3))                   (1 2))
     ((":")         ((:inherit))                  ())
     (("1:")        ((1 :inherit))                (1))
     (("1:2" "3:")  ((1 2) (3 :inherit))          (1 2))
     (("1:2:" "3")  ((1 2 :inherit) (3))          (1 2 3))
     (("1:2:" "3:") ((1 2 :inherit) (3 :inherit)) (1 2 3)))))

(test standard-schema.describe-object
  "Smoke test for the `describe-object' method for the
   `standard-schema' class."

  (mapc (curry #'apply #'check-describe-option-container)
        `((,*simple-schema*
           ,(format nil "<root>~@
                         │ Simple configuration options for tests.~@
                         │ ~@
                         │~@
                         │   Second paragraph.~@
                         ├─*~@
                         │   Type    BOOLEAN~@
                         │   Default <no default>~@
                         ├─bar~@
                         │ │ Type    BOOLEAN~@
                         │ │ Default <no default>~@
                         │ └─fez~@
                         │     Type    PATHNAME~@
                         │     Default <no default>~@
                         ├─baz~@
                         │ └─foo~@
                         │     Type    STRING~@
                         │     Default <no default>~@
                         ├─foo~@
                         │ │ Type    INTEGER~@
                         │ │ Default 1~@
                         │ │ This option controls foo.~@
                         │ └─fez~@
                         │     Type    INTEGER~@
                         │     Default <no default>~@
                         ├─sub~@
                         │ │ Simple schema for inclusion in a parent schema.~@
                         │ ├─whoop~@
                         │ │   Type    STRING~@
                         │ │   Default <no default>~@
                         │ ├─*~@
                         │ │   Type    INTEGER~@
                         │ │   Default <no default>~@
                         │ └─sub~@
                         │   │ Simple schema for inclusion in a parent schema.~@
                         │   ├─whoop~@
                         │   │   Type    STRING~@
                         │   │   Default <no default>~@
                         │   └─*~@
                         │       Type    INTEGER~@
                         │       Default <no default>~@
                         └─wild~@
                         ~0@T  └─**~@
                         ~0@T      Type    SYMBOL~@
                         ~0@T      Default <no default>")))))

;;; `standard-schema-item' class

(def-suite standard-schema-item
  :in options
  :description
  "Test suite for the `standard-schema-item' class.")
(in-suite standard-schema-item)

(test standard-schema-item.option-default
  "Test handling of default value in the `standard-schema-item'
   class."

  (flet ((test-case (type default default? expected-default expected-default?)
           (let+ ((item (apply #'make-instance 'standard-schema-item
                               :name "test"
                               :type type
                               (when default?
                                 (list :default default))))
                  ((&values default default?)
                   (option-default item :if-does-not-exist nil)))
             (case type
               (function
                (is (eq expected-default? default?))
                (when default?
                  (is (functionp default))
                  (is (equal expected-default (funcall default)))))
               (t
                (is (eq expected-default? default?))
                (is (equal expected-default default)))))))
    (test-case 'integer  nil                       nil nil nil)
    (test-case 'integer  1                         t   1   t)
    (test-case 'integer  (lambda () 1)             t   1   t)
    (test-case 'function (lambda () (lambda () 1)) t   1   t)))

(test standard-schema-item.describe-object
  "Smoke test for the `describe-object' method for the
   `standard-schema-item' class."

  (mapc (lambda+ ((item expected))
          (is (string= expected
                       (with-output-to-string (stream)
                         (describe-object item stream)))))
        `((,(simple-schema-item)
           ,(format nil "Type    INTEGER~@
                         Default <no default>~@
                         A simple option."))
          (,(simple-schema-item :default 1)
           ,(format nil "Type    INTEGER~@
                         Default 1~@
                         A simple option.")))))
