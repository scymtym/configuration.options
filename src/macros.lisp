;;;; macros.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

(defmacro define-schema (() &body specs)
  "TODO(jmoringe): document"
  (let+ (((&with-gensyms schema))
         ((&labels process-spec (spec &optional prefix)
            (let+ (((name &rest rest) spec)
                   (name (etypecase name
                           ((eql :wild)
                            (make-instance 'wildcard-name :components '(:wild)))
                           ((eql :wild-inferiors)
                            (make-instance 'wildcard-name :components '(:wild-inferiors)))
                           (string
                            (esrap:parse 'options::name name))))
                   ((&flet add-option (class)
                      `((setf (find-option ',(merge-names prefix name) ,schema)
                              (make-instance
                               ',(getf rest :schema-item-class class)
                               :name ',(merge-names prefix name)
                               ,@(remove-from-plist rest :schema-item-class)))))))
              (cond
                ;; TODO(jmoringe, 2013-03-12): wild-name?
                ((and (keywordp (first rest)) (typep name 'wildcard-name))
                 (add-option 'wildcard-schema-item))

                ((keywordp (first rest))
                 (add-option 'standard-schema-item))

                ;; TODO(jmoringe, 2013-03-12): wild-name?
                ((and (consp (first rest)) (typep name 'wildcard-name))
                 `((setf (find-child ,(merge-names prefix name) ,schema
                                     :if-exists #'error)
                         (define-schema () ,@rest))))

                ((consp (first rest))
                 (let ((prefix (merge-names prefix name)))
                   (mapcan (rcurry #'process-spec prefix) rest)))

                (t
                 (error "~@<Syntax error: ~S ~S.~@:>" name rest)))))))
    `(let ((,schema (make-instance 'standard-schema)))
       ,@(mapcan #'process-spec specs)
       ,schema)))
