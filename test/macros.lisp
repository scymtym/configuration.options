;;;; macros.lisp --- Test for the macros provided by the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(define-schema *child-schema* ; used in next test
  ("b" :type 'string))

(test eval-schema-spec.smoke
  "Smoke test for the `eval-schema-spec' function."

  (mapc (lambda+ ((input expected))
          (let+ (((&flet do-it () (apply #'eval-schema-spec input))))
            (case expected
              (error (signals error (do-it)))
              (schema-syntax-error
               (signals schema-syntax-error (do-it)))
              (option-value-error
               (signals option-value-error (do-it)))
              (t
               (let+ (((expected-documentation queries) expected)
                      (schema (do-it)))
                 (is (equal expected-documentation
                            (documentation schema t)))
                 (is (equal expected-documentation
                            (option-documentation schema)))
                 (mapc (lambda+ ((query expected-type
                                  &optional
                                  (expected-default nil expected-default-supplied?)
                                  expected-documentation))
                         (let ((item (find-option query schema :match-wildcards? t)))
                           (is (equal expected-type (option-type item)))
                           (when expected-default-supplied?
                             (is (equal expected-default (option-default item))))
                           (is (equal expected-documentation
                                      (documentation item t)))
                           (is (equal expected-documentation
                                      (option-documentation item)))))
                       queries))))))

        '(;; Some invalid cases.
          (((() ("a")))          schema-syntax-error)     ; incomplete specification
          (((() ("a" ())))       schema-syntax-error)     ; likewise
          (((() ("a "("b"))))    schema-syntax-error)     ; likewise
          (((() ("a" ("b" ())))) schema-syntax-error)     ; likewise
          (((() ("a" :type string)))                      ; invalid type (evaluated)
           error)
          (((() ("a" :type 'string) ("a" :type 'string))) ; duplicate name
           error)
          (((() ("a" :type 'boolean :default unbound)))   ; invalid default (evaluated)
           error)
          (((() ("a" :type 'boolean :default 1)))         ; invalid default
           option-value-error)

          ;; Some valid cases.
          (((()))
           (nil ()))

          (((()) :documentation "foo")
           ("foo" ()))

          (((() ("a" :type 'string)))
           (nil (("a" string))))

          (((() ("a" :type 'integer :default 5)))
           (nil (("a" integer 5))))

          (((() ("a" :type 'boolean :default t :documentation "bar")))
           (nil (("a" boolean t "bar"))))

          (((() ("a" ("b" :type 'boolean))))
           (nil (("a.b" boolean))))

          (((() ("a" *child-schema*)))
           (nil (("a.b" string)))))))

(test define-schema.smoke
  "Smoke test for the `define-schema' macro."

  ;; Incomplete specification
  (signals schema-syntax-error
    (define-schema *schema*
      ("a")))

  ;; Invalid type (when evaluated).
  (signals error
    (define-schema *schema*
      ("a" :type string)))

  ;; Duplicate name.
  (signals error
    (define-schema *schema*
      ("a" :type 'string) ("a" :type 'string)))

  ;; Invalid default (when evaluated).
  (signals error
    (define-schema *schema*
      ("a" :type 'boolean :default unbound)))

  ;; Invalid default.
  (signals option-value-error
    (define-schema *schema*
        ("a" :type 'boolean :default 1)))

  ;; A real-world schema.
  (define-schema *schema*
    "Test schema."
    ("rsc"
     ("logging"
      (:wild-inferiors
       ("LEVEL" :type '(member :debug :info :warning :error :fatal)))))

    ("qualityofservice"
     ("reliability"
      :type '(member :unreliable :reliable))
     ("ordering"
      :type '(member :unordered :ordered)))

    ("errorhandling"
     ("onhandlererror" :type '(member :log :print :exit)))

    ("plugins"
     (:wild
      ("path" :type '(list pathname :inherit? t))
      ("load" :type '(list string :inherit? t))))

    ("transport"
     (:wild
      ("enabled"
       :schema-item-class standard-schema-item
       :option-class foo
       :type 'boolean
       :default nil)
      (:wild
       :type 'integer))
     ("socket"
      ("host"   :type 'string)
      ("port"   :type '(integer 0 65535))
      ("server" :type '(or boolean (member :auto))
                :documentation
                "Act as server or client or determine role automatically.")))

    (:wild-inferiors
     ("config-file" :type 'pathname)
     (:wild :type 'string))))
