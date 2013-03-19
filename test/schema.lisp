;;;; schema.lisp --- Unit tests for schema-related stuff.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(in-suite options)

(let ((i (make-instance 'standard-schema-item
                        :name   '("a" "b")
                        :type   'integer
                        :default 5)))
  (values
   (options::merge-values i '())
   (options::merge-values i '(1))
   (options::merge-values i '(1 2 3))))

(test type-list
 (mapc
  (lambda+ ((input expected-parsed expected-value))
    (let* ((item   (make-instance 'standard-schema-item
                                  :name    '("a" "b")
                                  :type    '(list integer :inherit? t)
                                  :default '(-1)))
           (parsed (mapcar (curry #'string->value item) input))
           (value  (options::merge-values item parsed)))
      (is (equal parsed expected-parsed))
      (is (equal value expected-value))))

  '((()            ()                            (-1))
    (("1")         ((1))                         (1))
    (("1:2" "3")   ((1 2) (3))                   (1 2))
    ((":")         ((:inherit))                  (-1))
    (("1:")        ((1 :inherit))                (1 -1))
    (("1:2" "3:")  ((1 2) (3 :inherit))          (1 2))
    (("1:2:" "3")  ((1 2 :inherit) (3))          (1 2 3))
    (("1:2:" "3:") ((1 2 :inherit) (3 :inherit)) (1 2 3 -1)))))
