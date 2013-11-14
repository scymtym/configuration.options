;;;; let-plus.lisp --- let-plus integration.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This file provides three kinds of let-plus bindings:
;;;;
;;;;   (let+ (((&options-r/o          BINDING*) CONFIGURATION))
;;;;
;;;;     read-only bindings of variables to option values
;;;;
;;;;   (let+ (((&options              BINDING*) CONFIGURATION))
;;;;
;;;;     read/write bindings of symbols to option values
;;;;
;;;;   (let+ (((&options/synchronizer BINDING*) SYNCHRONIZER))
;;;;
;;;;     read/write bindings of symbols to option values with
;;;;     modifications going through SYNCHRONIZER

(cl:in-package #:configuration.options)

(define-let+-expansion (&options-r/o arguments
                        :value-var value
                        :body-var  body)
  "Execute BODY with established bindings of option values from the
   given `configuration' object.

   Bindings are specifications of the form

     BINDING      ::= VAR-AND-NAME | FULL-BINDING
     VAR-AND-NAME ::= VAR | (VAR NAME)
     FULL-BINDING ::= (VAR-AND-NAME [DEFAULT-VALUE [VALUE?-VAR]])

   VAR is a symbol naming the variable to which the option value
   should be bound.

   NAME is a string naming an option or an option name object. When
   NAME is not supplied, it is derived from VAR by downcasing the name
   of the symbol.

   When the option named by NAME does not have a value and
   DEFAULT-VALUE is supplied, VAR is bound to DEFAULT-VALUE.

   When VALUE?-VAR is supplied, it is bound to true when the option
   named by NAME has value and to false otherwise."
  (let+ (((&flet+ one-binding ((var name default default-supplied? value?-var))
            `(,(if value?-var
                   `(&values ,var ,value?-var)
                   var)
              ,(%make-option-value
                `(find-option ,name ,value :if-does-not-exist :create)
                (if default-supplied? default '#'error))))))
    `(let+ ,(mapcar (compose #'one-binding #'%parse-value-binding) arguments)
       ,@body)))

(define-let+-expansion (&options arguments
                        :value-var value
                        :body-var  body)
  "Like `&options-r/o' but the bound variables are `setf' able
   places. Setting the value of such a place sets the value of the
   corresponding option."
  (let+ (((&flet+ one-binding ((var name default default-supplied? value?-var))
            (let ((option (gensym)))
              (list
               `(,option (find-option ,name ,value :if-does-not-exist :create))
               (when value?-var
                 `(,value?-var (nth-value 1 ,(%make-option-value option))))
               `(,var ,(%make-option-value
                        option (if default-supplied? default '#'error)))))))
         (bindings (mapcar (compose #'one-binding #'%parse-value-binding) arguments)))
    `(let ,(mapcar #'first bindings)                               ; option instances
       (symbol-macrolet (,@(remove nil (mapcar #'second bindings)) ; value? bindings
                         ,@(mapcar #'third bindings))              ; symbol macros
         ,@body))))

(define-let+-expansion (&options/synchronizer arguments
                        :value-var       value
                        :body-var        body
                        ; TODO maybe later :environment-var env
                        )
  "Similar to `&options' but the value has to be a synchronizer
   instead of a `configuration'. When `setf' is used on a place bound
   to an option, the synchronizer is used to set the new value."
  `(let+ (((&flet (setf option-value) (new-value option &rest args)
             (declare (ignore args))
             (let ((name (option-name option)))
               (notify ,value :added     name nil)
               (notify ,value :new-value name new-value
                       :raw?   nil
                       :source `(:code :file ,(or *compile-file-pathname*
                                                  *load-pathname*))))))
          ((&options ,@arguments) (synchronizer-target ,value)))
     ,@body))

;;; Utility functions

;; Parse SPEC which is of the form
;;
;;   spec         ::= (var-and-name [DEFAULT-VALUE [VALUE?-VAR]])
;;   var-and-name ::= VAR | (VAR NAME)
;;
;; and return a list of the form
;;
;;   (VAR NAME DEFAULT-VALUE DEFAULT-SUPPLIED? VALUE?-VAR)
(defun %parse-value-binding (spec)
  (let+ (((var-and-name &optional (default nil default-supplied?) value?-var)
          (ensure-list spec))
         ((var &optional (name (string-downcase var)))
          (ensure-list var-and-name)))
    (list var name default default-supplied? value?-var)))

(defun %make-option-value (option &optional if-does-not-exist)
  `(option-value ,option :if-does-not-exist ,if-does-not-exist))
