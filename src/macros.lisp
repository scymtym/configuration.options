;;;; macros.lisp --- Macros provided by the options system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

(defun map-schema-spec (function spec)
  "Call FUNCTION for each specification item in SPEC and return
   FUNCTION's return value for the toplevel schema item in SPEC.

   FUNCTION should have a lambda-list compatible to the following one

     kind name rest &key prefix schema self

   where KIND is one of :schema, :wild-schema and :item, NAME is the
   name of the currently processed item and REST is the
   remainder (following the name) of the specification item when KIND
   is :item."
  (let+ (((&labels+ recur ((&whole spec name &rest rest) prefix schema)
            (let* ((name        (make-name (ensure-list name)))
                   (prefix/next (merge-names prefix name)))
             (etypecase spec
               ((cons t (cons keyword)) ; item
                (funcall function :item name rest :prefix prefix :schema schema))
               ((cons t list)           ; sub-schema
                (let* ((kind        (etypecase name
                                      (wild-name      :wild-schema)
                                      ((or null name) :schema)))
                       (schema/next (funcall function kind name nil
                                             :prefix prefix
                                             :schema schema)))
                  (mapc (rcurry #'recur prefix/next schema/next) rest)
                  schema/next)))))))
    (recur spec '() nil)))

(defun eval-schema-spec (spec &key documentation)
  "Evaluate SPEC as a schema specification and return the resulting
   schema object.

   If DOCUMENTATION is supplied, it is used as the documentation
   string of the toplevel schema item in SPEC."
  (let+ (((&flet make-initarg-when-supplied (initarg form supplied?)
            (when supplied?
              (list initarg (eval form)))))
         ((&flet do-spec (kind name rest &key prefix schema &allow-other-keys)
            (etypecase kind
              ((member :schema :wild-schema)
               (or schema (make-instance 'standard-schema
                                         :documentation documentation)))
              #+TODO-no? ((eql :wild-schema)
                          (unless schema
                            (error "~@<Toplevel schema cannot have wildcard name.~@:>"))
                          (setf (find-child (merge-names prefix name) schema)
                                (make-instance 'standard-schema)))
              ((eql :item)
               (let+ (((&key
                        (type    nil type-supplied?)
                        (default nil default-supplied?)
                        &allow-other-keys) rest)
                      (name/merged       (merge-names prefix name))
                      (schema-item-class 'standard-schema-item))
                 (setf (find-option name/merged schema :if-exists #'error)
                       (apply #'make-instance schema-item-class
                              :name name/merged
                              (append
                               (make-initarg-when-supplied
                                :type type type-supplied?)
                               (make-initarg-when-supplied
                                :default default default-supplied?)
                               (remove-from-plist
                                rest
                                :prefix :schema :schema-item-class
                                :type :default))))))))))
    (map-schema-spec #'do-spec spec)))

(defmacro define-schema (name-and-args &body docstring-and-specs)
  "Define a parameter (like `cl:defparameter') named according to
   NAME-AND-ARGS the initial value of which is a schema as specified
   by DOCSTRING-AND-SPECS.

   NAME-AND-ARGS can either be a symbol which will be treated as a
   name or a list starting with a symbol followed by keyword
   arguments.

   DOCSTRING-AND-SPECS is a list of schema and option
   specifications (optionally preceded by a documentation string)
   where each specification is of the form

     SPEC        ::= SCHEMA-SPEC | OPTION-SPEC
     SCHEMA-SPEC ::= (NAME SPEC*)
     OPTION-SPEC ::= (NAME &key type default documentation)

   The arguments of the type and default keyword parameters are
   evaluated.

   When DOCSTRING-AND-SPECS starts with a documentation string, it is
   used as the documentation string of the toplevel schema object.

   Example:

     (define-schema *my-schema*
       \"Schema for my configuration\"

       (\"section\"
         (\"option\" :type 'integer))) ; Note: :type argument is evaluated "
  (let+ (((name) (ensure-list name-and-args))
         ((&values specs &ign documentation)
          (parse-body docstring-and-specs :documentation t)))
    `(defparameter ,name
       (eval-schema-spec '(() ,@specs) :documentation ,documentation)
       ,@(when documentation `(,documentation)))))
