;;;; protocol.lisp --- Test for the protocol functions of the options system.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(in-suite options)

;;; Helper for name coercion tests

(defvar *mock-container/name-coercion-testing-find-option* nil)

(defvar *mock-container/name-coercion-testing-find-child* nil)

(defclass mock-container/name-coercion () ())

(defmethod find-options ((query t) (container mock-container/name-coercion))
  (list query))

(defmethod find-option ((name t) (container mock-container/name-coercion)
                        &key &allow-other-keys)
  (when *mock-container/name-coercion-testing-find-option*
    (list name)))

(defmethod (setf find-option) ((new-value t)
                               (name      t)
                               (container mock-container/name-coercion)
                               &key &allow-other-keys)
  (list name))

(defmethod find-child ((name t) (container mock-container/name-coercion)
                       &key &allow-other-keys)
  (when *mock-container/name-coercion-testing-find-child*
    (list name)))

(defmethod (setf find-child) ((new-value t)
                              (name      t)
                              (container mock-container/name-coercion)
                              &key &allow-other-keys)
  (list name))

(defclass mock-schema-item/name-coercion () ())

(defmethod make-option ((schema-item mock-schema-item/name-coercion)
                        (name        t))
  (list name))

(defun call-with-name-coercion-cases (container thunk)
  (let+ (((&flet+ test-case ((name expected-wild?))
            (let+ (((result) (funcall thunk name container)))
              (if expected-wild?
                  (is (typep result 'wild-name))
                  (is (typep result '(and name (not wild-name)))))))))
    (mapc #'test-case '((()        nil)
                        (#()       nil)
                        (("a")     nil)
                        (("a" "b") nil)
                        (("*")     nil)
                        (("**")    nil)
                        ("a"       nil)
                        ("a.b"     nil)
                        ("*"       t)
                        ("**"      t)))))

;;; Option container protocol

;; Name coercion

(test protocol.find-options.name-coercion
  "Test name coercion performed by the `find-options' generic
   function."
  (call-with-name-coercion-cases
   (make-instance 'mock-container/name-coercion) #'find-options))

(test protocol.find-option.name-coercion
  "Test name coercion performed by the `find-option' generic
   function."
  (let ((*mock-container/name-coercion-testing-find-option* t))
    (call-with-name-coercion-cases
     (make-instance 'mock-container/name-coercion) #'find-option)))

(test protocol.setf-find-options.name-coercion
  "Test name coercion performed by the setf `find-option' generic
   function."
  (call-with-name-coercion-cases
   (make-instance 'mock-container/name-coercion)
   (lambda (name container)
     (setf (find-option name container) t))))

;; Default behavior

(defun map-options.ensure-function (&rest args)
  args)

(defclass map-options.ensure-function () ())

(defmethod map-options ((function  function)
                        (container map-options.ensure-function))
  function)

(test protocol.map-options.ensure-function
  "Test function coercion performed by `map-options'."
  (is (eq #'map-options.ensure-function
          (map-options 'map-options.ensure-function
                       (make-instance 'map-options.ensure-function)))))

(test protocol.find-option.remove
  "Test removing options via setf `find-option'."

  (mapc
   (lambda+ ((exists? if-does-not-exist if-exists expected))
     (let+ ((container (empty-configuration))
            (option    (simple-option))
            (name      (option-name option))
            (existing  (when exists?
                         (setf (find-option name container) option)))
            ((&flet do-it ()
               (setf (find-option name container
                                  :if-does-not-exist if-does-not-exist
                                  :if-exists         if-exists)
                     nil))))
       (ecase expected
         ((nil)                  (is (eq nil (do-it))))
         (:existing              (is (eq existing (do-it))))
         (option-missing-warning (signals option-missing-warning (do-it)))
         (option-missing-error   (signals option-missing-error (do-it)))
         (option-exists-warning  (signals option-exists-warning (do-it)))
         (option-exists-error    (signals option-exists-error (do-it))))))
   '((nil nil   :supersede nil)
     (nil nil   :keep      nil)
     (nil nil   error      nil)
     (nil nil   warn       nil)

     (nil error :supersede option-missing-error)
     (nil error :keep      option-missing-error)
     (nil error error      option-missing-error)
     (nil error warn       option-missing-error)

     (nil warn  :supersede option-missing-warning)
     (nil warn  :keep      option-missing-warning)
     (nil warn  error      option-missing-warning)
     (nil warn  warn       option-missing-warning)

     (t   nil   :supersede nil)
     (t   nil   :keep      :existing)
     (t   nil   error      option-exists-error)
     (t   nil   warn       option-exists-warning)

     (t   error :supersede nil)
     (t   error :keep      :existing)
     (t   error error      option-exists-error)
     (t   error warn       option-exists-warning)

     (t   warn  :supersede nil)
     (t   warn  :keep      :existing)
     (t   warn  error      option-exists-error)
     (t   warn  warn       option-exists-warning))))

;;; Schema protocol

;; Name coercion

(test protocol.find-child.name-coercion
  "Test name coercion performed by the `find-child' generic
   function."
  (let ((*mock-container/name-coercion-testing-find-child* t))
    (call-with-name-coercion-cases
     (make-instance 'mock-container/name-coercion)
     #'find-child)))

(test protocol.setf-find-child.name-coercion
  "Test name coercion performed by the setf `find-child' generic
   function."
  (call-with-name-coercion-cases
   (make-instance 'mock-container/name-coercion)
   (lambda (name container)
     (setf (find-child name container) t))))

;; Default behavior

(test protocol.find-child.remove
  "Test removing options via setf `find-option'."

  (mapc
   (lambda+ ((exists? if-does-not-exist if-exists expected))
     (let+ ((container (empty-schema))
            (name      "child")
            (existing  (when exists?
                         (setf (find-child name container) (empty-schema))))
            ((&flet do-it ()
               (setf (find-child name container
                                 :if-does-not-exist if-does-not-exist
                                 :if-exists         if-exists)
                     nil))))
       (ecase expected
         ((nil)                 (is (eq nil (do-it))))
         (:existing             (is (eq existing (do-it))))
         (child-missing-warning (signals child-missing-warning (do-it)))
         (child-missing-error   (signals child-missing-error (do-it)))
         (child-exists-warning  (signals child-exists-warning (do-it)))
         (child-exists-error    (signals child-exists-error (do-it))))))
   '((nil nil   :supersede nil)
     (nil nil   :keep      nil)
     (nil nil   error      nil)
     (nil nil   warn       nil)

     (nil error :supersede child-missing-error)
     (nil error :keep      child-missing-error)
     (nil error error      child-missing-error)
     (nil error warn       child-missing-error)

     (nil warn  :supersede child-missing-warning)
     (nil warn  :keep      child-missing-warning)
     (nil warn  error      child-missing-warning)
     (nil warn  warn       child-missing-warning)

     (t   nil   :supersede nil)
     (t   nil   :keep      :existing)
     (t   nil   error      child-exists-error)
     (t   nil   warn       child-exists-warning)

     (t   error :supersede nil)
     (t   error :keep      :existing)
     (t   error error      child-exists-error)
     (t   error warn       child-exists-warning)

     (t   warn  :supersede nil)
     (t   warn  :keep      :existing)
     (t   warn  error      child-exists-error)
     (t   warn  warn       child-exists-warning))))

;;; Option-like protocol

(macrolet
    ((define-value-function-test ((name &key (value-var 'value))
                                  construction-expression
                                  &body cases)
       `(test ,(symbolicate '#:protocol. name)
          ,(format nil "Test default behavior of `~(~A~)' function."
                   name)
          (mapc
           (lambda+ ((,value-var expected-value expected-value?))
             (let+ ((object ,construction-expression)
                    ((&flet do-it (&optional (if-does-not-exist
                                              nil
                                              if-does-not-exist-supplied?))
                       (apply #',name object
                              (when if-does-not-exist-supplied?
                                `(:if-does-not-exist ,if-does-not-exist))))))
               (let+ (((&values value value?) (do-it nil)))
                 (is (equal expected-value  value))
                 (is (eq    expected-value? value?)))
               (when (not expected-value?)
                 (signals value-missing-error (do-it))
                 (is (equal :foo (handler-bind
                                     ((value-missing-error
                                       (lambda (condition)
                                         (declare (ignore condition))
                                         (let ((restart (find-restart 'retry)))
                                           (is-true restart)
                                           (is (not (emptyp (princ-to-string restart)))))
                                         (let ((restart (find-restart 'use-value)))
                                           (is-true restart)
                                           (is (not (emptyp (princ-to-string restart))))
                                           (invoke-restart restart :foo)))))
                                   (do-it)))))))
           (list ,@cases)))))

  (define-value-function-test (option-default :value-var default)
      (apply #'make-instance 'standard-schema-item
             :name '("a" "b")
             :type t
             (when default `(:default ,(first default))))
    ;; default expected default expected default?
    '(()       nil              nil)
    '((1)      1                t)
    '((nil)    nil              t))

  (define-value-function-test (option-value :value-var value)
      (let* ((item   (make-instance 'standard-schema-item
                                    :name '("a" "b") :type t))
             (option (make-option item '("a" "b"))))
        (when value
          (setf (option-value option) (first value)))
        option)
    ;; value expected value expected value?
    '(()     nil            nil)
    '((1)    1              t)
    '((nil)  nil            t)))

(test protocol.setf-option-value.if-does-not-exist
  "Test that setf `option-value' accepts the :if-does-not-exist
   option (despite ignoring it)."

  (let* ((item   (make-instance 'standard-schema-item
                                :name '("a") :type t))
         (option (make-option item '("a"))))
    (setf (option-value option :if-does-not-exist #'error) 1)))

(test protocol.validate-value.default-behavior
  "Smoke test for the default behavior of the `validate-value'
   function."

  (let ((schema-item (make-instance 'standard-schema-item
                                    :name "a"
                                    :type 'boolean)))
    (is (eq :foo (validate-value schema-item 1 :if-invalid :foo)))

    (handler-bind ((option-value-error #'continue))
      (is (eq t (validate-value schema-item 1))))))

;;; Schema-item protocol

;; Name coercion

(test protocol.make-option.name-coercion
  "Test name coercion performed by the `make-option' generic
   function."

  (call-with-name-coercion-cases
   (make-instance 'mock-schema-item/name-coercion)
   (lambda (name container)
     (make-option container name))))
