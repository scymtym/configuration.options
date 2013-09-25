;;;; schema.lisp --- Unit tests for schema-related stuff.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(in-suite options)

(test find-child.smoke
  "Smoke test for the `find-child' and (setf find-child) functions."

  (macrolet
      ((test-case (&body body)
         `(let ((schema (make-instance 'standard-schema))
                (child  (make-instance 'standard-schema)))
            (declare (ignorable child))
            ,@body)))

    ;; Reader
    (test-case
     (signals no-such-option
       (find-child "no.such.child" schema)))
    (test-case
     (is (not (find-child "no.such.child" schema
                          :if-does-not-exist nil))))
    (test-case
     (is (eq (handler-bind ((no-such-option
                              (lambda (condition)
                                (declare (ignore condition))
                                (invoke-restart 'use-value :foo))))
               (find-option "no.such.child" schema))
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

(test make-option.smoke
  "Smoke test for the `make-option' function."

  (mapc (lambda (item) (make-option item (option-name item)))
        (options +simple-schema+)))

(test make-configuration.smoke
  "Smoke test for the `make-configuration' function."

  (let ((configuration (make-configuration +simple-schema+)))
    (is (eq (configuration-schema configuration) +simple-schema+))))

(test type-list
  "Kind-of integrationtest for schema item with list-of-something
   type."

  (mapc
   (lambda+ ((input expected-parsed expected-value))
     (let* ((item   (make-instance 'standard-schema-item
                                   :name    '("a" "b")
                                   :type    '(list integer :inherit? t)
                                   :default '(-1)))
            (parsed (mapcar (curry #'string->value item) input))
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
