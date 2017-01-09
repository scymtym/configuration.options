;;;; protocol.lisp --- Test for the protocol functions of the options system.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(def-suite protocol
  :in options
  :description
  "Test suite for protocol functions.")
(in-suite protocol)

;;; Helper for name coercion tests

(defvar *mock-container/name-coercion-testing-find-option* nil)

(defvar *mock-container/name-coercion-testing-find-child* nil)

(defclass mock-container/name-coercion () ())

(defmethod map-matching-options ((function function)
                                 (query    t)
                                 (continer mock-container/name-coercion)
                                 &key interpret-wildcards?)
  (declare (ignore interpret-wildcards?))
  (list query))

(defmethod find-options ((query t) (container mock-container/name-coercion)
                         &key interpret-wildcards?)
  (declare (ignore interpret-wildcards?))
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

;;; Value protocol

;; Name coercion

(defun value-name-coercion-test-cases (thunk)
  (let ((configuration *simple-configuration*))
    ;; :configuration is not supplied and `*configuration*' is not
    ;; bound to a configuration.
    (signals error (funcall thunk "foo"))
    (signals error (funcall thunk "does-not-exist"))
    (signals error (funcall thunk "does-not-exist" :if-does-not-exist nil))

    ;; :configuration is supplied.
    (is-true (nth-value
              1 (funcall thunk "foo" :configuration configuration)))
    (signals option-missing-error
      (funcall thunk "does-not-exist" :configuration configuration))
    (is-false (nth-value
               1 (funcall thunk "does-not-exist"
                          :configuration     configuration
                          :if-does-not-exist nil)))

    ;; `*configuration*' is bound.
    (let ((*configuration* configuration))
      (is-true (nth-value 1 (funcall thunk "foo")))
      (signals option-missing-error
        (funcall thunk "does-not-exist"))
      (is-false (nth-value
                 1 (funcall thunk "does-not-exist"
                            :if-does-not-exist nil))))))

(test protocol.value.name-coercion
  "Test name coercion performed by the `value' generic function."

  (value-name-coercion-test-cases
   (lambda (name &rest args)
     (apply #'value name args))))

(test protocol.setf-value.name-coercion
  "Test name coercion performed by the `value' generic function."

  (value-name-coercion-test-cases
   (lambda (name &rest args)
     (apply #'(setf value) 1 name args))))

;; Default behavior

(test protocol.value.smoke
  "Smoke test for the `value' generic function."

  ;; Missing value.
  (let ((option (simple-option)))
    (signals value-missing-error (value option))
    (is-false (nth-value 1 (value option :if-no-value nil)))
    (is-false (nth-value 1 (value option :if-no-value :foo))))

  ;; Value.
  (let ((option (simple-option :value 1)))
    (is (equal '(1 t nil) (multiple-value-list (value option))))))

(test protocol.setf-value.smoke
  "Smoke test for the setf `value' generic function."

  (let ((option (simple-option)))
    (setf (value option) 5)
    (is (equal '(5 t nil) (multiple-value-list (value option))))))

;;; Option container protocol

;; Name coercion

(test protocol.map-matching-options.name-coercion
  "Test name coercion performed by the `map-matching-options' generic
   function."
  (call-with-name-coercion-cases
   (make-instance 'mock-container/name-coercion)
   (lambda (name container)
     (map-matching-options #'identity name container))))

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

(test protocol.setf-find-option.name-coercion
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

(defmethod map-matching-options ((function  function)
                                 (query     t)
                                 (container map-options.ensure-function)
                                 &key
                                 interpret-wildcards?)
  (declare (ignore interpret-wildcards?))
  function)

(defmethod map-options ((function  function)
                        (container map-options.ensure-function))
  function)

(test protocol.map-options.ensure-function
  "Test function coercion performed by `map-options'."
  (is (eq #'map-options.ensure-function
          (map-options 'map-options.ensure-function
                       (make-instance 'map-options.ensure-function)))))

(test protocol.map-matching-options.ensure-function
  "Test function coercion performed by `map-matching-options'."
  (is (eq #'map-options.ensure-function
          (map-matching-options
           'map-options.ensure-function
           "a.b"
           (make-instance 'map-options.ensure-function)))))

(defclass mock-option/matching ()
  ((name :initarg  :name
         :reader   option-name)))

(defclass mock-container/matching ()
  ((options :initarg  :options
            :accessor options
            :initform '())))

(defmethod shared-initialize :after ((instance   mock-container/matching)
                                     (slot-names t)
                                     &key
                                     options)
  (setf (options instance)
        (mapcar (compose (curry #'make-instance 'mock-option/matching :name)
                         #'make-name)
                options)))

(defmethod map-options ((function  function)
                        (container mock-container/matching))
  (mapcar function (options container)))

(macrolet
    ((test-case ((container) &body body)
       `(let+ ((container ,container)
               ((&flet check-query (query-and-args expected)
                  (check-query query-and-args expected container))))
          ,@body))
     (test-cases (&key interpret-wildcards?-parameter?)
       `(progn
          ;; Empty results.
          (test-case ((make-instance 'mock-container/matching))
            (check-query "no.such.option" '()))

          ;; Complex queries.
          (test-case ((make-instance 'mock-container/matching
                                     :options '("*" "wild.**" "foo" "foo.fez"
                                                "bar" "bar.fez" "baz.foo")))
            ;; Single component and wild[-inferiors] queries.
            (check-query "foo" '("foo"))
            (check-query "*"   '("*" "foo" "bar"))
            (check-query "**"  '("*" "wild.**" "foo.fez" "foo" "baz.foo"
                                 "bar.fez" "bar"))
            ,@(when interpret-wildcards?-parameter?
                `((check-query '("foo" :interpret-wildcards? :container) '("*" "foo"))
                  (check-query '("*"   :interpret-wildcards? :container) '("*"))
                  (check-query '("**"  :interpret-wildcards? :container) '("*"))
                  (check-query '("foo" :interpret-wildcards? nil)        '("foo"))
                  (check-query '("*"   :interpret-wildcards? nil)        '("*"))
                  (check-query '("**"  :interpret-wildcards? nil)        '())))

            ;; Mixed queries.
            (check-query "**.bar" '("bar"))
            (check-query "bar.**" '("bar.fez" "bar"))
            ,@(when interpret-wildcards?-parameter?
                `((check-query '("**.bar"   :interpret-wildcards? :container) '())
                  (check-query '("bar.**"   :interpret-wildcards? :container) '())
                  (check-query '("wild.foo" :interpret-wildcards? :container) '("wild.**"))
                  (check-query '("**.bar"   :interpret-wildcards? nil)        '())
                  (check-query '("bar.**"   :interpret-wildcards? nil)        '())
                  (check-query '("wild.foo" :interpret-wildcards? nil)        '())))))))

  ;;; This hack is needed because fiveam evaluates test bodies in the
  ;;; null lexical environment.
  (defun protocol.map-matching-options.smoke-body ()
    (let+ (((&flet check-query (query-and-args expected container)
              (let+ (((query &rest args) (ensure-list query-and-args))
                     (result '()))
                (apply #'map-matching-options
                       (lambda (option &rest args)
                         (declare (ignore args))
                         (push (option-name option) result))
                       query container args)
                (is (set-equal/name-equal expected result))))))
      (test-cases :interpret-wildcards?-parameter? t)))

  (defun protocol.find-options.smoke-body ()
    (let+ (((&flet check-query (query expected container)
              (let ((result (find-options query container)))
                (is (set-equal/name-equal
                     expected (mapcar #'option-name result)))))))
      (test-cases))))

(test protocol.map-matching-options.smoke
  "Smoke test for the `map-matching-options' function."

  (protocol.map-matching-options.smoke-body))

(test protocol.find-options.smoke
  "Smoke test for the `find-options' generic function."

  (protocol.find-options.smoke-body))

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
  "Test removing children via setf `find-child'."

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
    ((define-value-function-test ((name
                                   parameters
                                   (value-var value?-var &rest values))
                                  construction-expression
                                  &body cases)
       (let+ (((&flet expected-name (name)
                 (symbolicate '#:expected- name)))
              (expected-values (mapcar #'expected-name values)))
         `(test ,(symbolicate '#:protocol. name)
            ,(format nil "Test default behavior of `~(~A~)' function."
                     name)
            (mapc
             (lambda+ ((,@parameters
                        expected-value expected-value? ,@expected-values))
               (let+ ((object ,construction-expression)
                      ((&flet do-it (&optional (if-does-not-exist
                                                nil
                                                if-does-not-exist-supplied?))
                         (apply #',name object
                                (when if-does-not-exist-supplied?
                                  `(:if-does-not-exist ,if-does-not-exist))))))
                 (let+ (((&values ,value-var ,value?-var ,@values) (do-it nil)))
                   ,@(mapcar
                      (lambda (expected value)
                        `(is (equal ,expected ,value)))
                      (list* 'expected-value 'expected-value? expected-values)
                      (list* value-var       value?-var       values)))
                 (when (not expected-value?)
                   (signals value-missing-error (do-it))
                   (is (equal
                        :foo
                        (handler-bind
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
             (list ,@cases))))))

  (define-value-function-test (option-default
                               (default)
                               (default default?))
      (apply #'make-instance 'standard-schema-item
             :name '("a" "b")
             :type t
             (when default `(:default ,(first default))))
    ;; default expected default expected default?
    '(()       nil              nil)
    '((1)      1                t)
    '((nil)    nil              t))

  (define-value-function-test (option-value
                               (default values value)
                               (value value? source))
      (let* ((item   (apply #'make-instance 'standard-schema-item
                            :name    '("a" "b")
                            :type    t
                            (when default `(:default ,(first default)))))
             (option (make-option item '("a" "b"))))
        (when values
          (setf (option-values option) values))
        (when value
          (setf (option-value option) (first value)))
        option)
    ;; default values                  value  ex. val ex val? ex source
    `(nil      #()                     nil    nil     nil     nil)
    `(nil      #((nil :source :foo))   (nil)  nil     t       :foo)
    `((1)      #()                     nil    nil     nil     nil)
    `((1)      #((1 :source :default)) (1)    1       t       :default)
    `((1)      #((1) (2))              (1)    1       t       t)))

(test protocol.setf-option-value.if-does-not-exist
  "Test that setf `option-value' accepts the :if-does-not-exist
   option (despite ignoring it)."

  (let* ((item   (make-instance 'standard-schema-item
                                :name '("a") :type t))
         (option (make-option item '("a"))))
    (finishes
      (setf (option-value option :if-does-not-exist #'error) 1))))

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
