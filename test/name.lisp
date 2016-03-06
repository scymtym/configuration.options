;;;; name.lisp --- Unit tests for name-related stuff.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(in-suite options)

(test wildcard-name.construction
  "Test construction of `wildcard-name' instances."

  (mapc
   (lambda+ ((input expected))
     (let+ (((&flet do-it ()
               (make-instance 'wildcard-name :components input))))
       (case expected
         (type-error (signals type-error (do-it)))
         (t          (let ((name (do-it)))
                       (is (typep name 'wild-name))
                       (is (name-equal (make-name expected) name)))))))

   '(;; Some invalid cases
     (()                type-error) ; non-wild
     (("a")             type-error) ; likewise
     (("a" "b")         type-error) ; likewise

     ;; These are valid
     ((:wild)           (:wild))
     ((:wild-inferiors) (:wild-inferiors)))))

(test make-name.smoke
  "Smoke test for `make-name' function."

  (mapc
   (lambda+ ((input expected-name?
              &optional
              expected-wild-name? expected-components))
     (let+ (((&flet do-it () (make-name input))))
       (case expected-name?
         (type-error       (signals type-error (do-it)))
         (name-parse-error (signals name-parse-error (do-it)))
         (t                (let ((name (do-it)))
                             (is (typep name 'name))
                             (when expected-wild-name?
                               (is (typep name 'wild-name)))
                             (is (equal expected-components
                                        (name-components name))))))))

   `(;; Some invalid cases
     (1                  type-error)        ; completely wrong type
     (:foo               type-error)        ; likewise

     (""                 name-parse-error)  ; invalid string
     ("."                name-parse-error)  ; likewise
     ("***"              name-parse-error)  ; likewise

     ;; No-op
     (,(make-name "a.b") t nil ("a" "b"))
     (,(make-name "a.*") t t   ("a" :wild))

     ;; Some non-wild cases.
     ("a"                t nil ("a"))
     (("a")              t nil ("a"))
     (,#("a")            t nil ("a"))
     ("a.b"              t nil ("a" "b"))
     (("a" "b")          t nil ("a" "b"))
     (,#("a" "b")        t nil ("a" "b"))
     (("*")              t nil ("*"))       ; non-wild with no string
     (("**")             t nil ("**"))      ; likewise

     ;; Some wild cases.
     ("*"                t t   (:wild))
     ((:wild)            t t   (:wild))
     ("**"               t t   (:wild-inferiors))
     ((:wild-inferiors)  t t   (:wild-inferiors)))))

(test name-matches.smoke
  "Smoke test for `name-matches' function."

  (mapc
   (lambda+ ((query name expected))
     (is (eq expected
             (name-matches (make-name query) (make-name name)))))

   `((()           ()          t)
     ("a"          "a"         t) ("a"          "b"         nil)
     ("a.b"        "a.b"       t) ("a.b"        "a.c"       nil)
     ("a.b.c"      "a.b.c"     t)

     ("*"          "a"         t) ("*"          "a.b"       nil)
     ("a.b.*"      "a.b.c"     t) ("a.b.*"      "a.d.c"     nil)
     ("*.b.c"      "a.b.c"     t) ("*.b.c"      "a.b.d"     nil)

     ("**"         ()          t)
     ("**"         "a"         t)
     ("**.a"       "a"         t) ("**.a"       "b"         nil)
     ("a.b.**.e"   "a.b.c.d.e" t) ("a.b.**.e"   "a.b.c.d.f" nil)

     ("*.b.**.e"   "a.b.c.d.e" t) ("*.b.**.e"   "a.c.b.d.e" nil))))

(test name<.smoke
  "Smoke test for `name<' function."

  (mapc
   (lambda+ ((left right expected))
     (is (eq expected (name< (make-name left) (make-name right)))
         "expected ~S to~:[ not~;~] be ~S ~S."
         left expected 'name< right))

   `((()   ()   nil) (()   "a"  t)   (()   "b"  t)
     (()   "*"  t)   (()   "**" t)

     ("a"  ()   nil) ("a"  "a"  nil) ("a"  "b"  t)
     ("a"  "*"  t)   ("a"  "**" t)

     ("b"  ()   nil) ("b"  "a"  nil) ("b"  "b"  nil)
     ("b"  "*"  t)   ("b"  "**" t)

     ("*"  ()   nil) ("*"  "a"  nil) ("*"  "b"  nil)
     ("*"  "*"  nil) ("*"  "**" t)

     ("**" ()   nil) ("**" "a"  nil) ("**" "b"  nil)
     ("**" "*"  nil) ("**" "**" nil))))

(test merge-names.smoke
  "Smoke test for `merge-names' function."

  (mapc
   (lambda+ ((left right expected))
     (is (name-equal (make-name expected)
                     (merge-names (make-name left) (make-name right)))))

   `((()   ()   ())
     (()   "a"  "a")    (()   "b"  "b")
     (()   "*"  "*")    (()   "**" "**")

     (()   "a"  "a")    ("a"  ()   "a")
     ("a"  "a"  "a.a")  ("a"  "b"  "a.b")
     ("a"  "*"  "a.*")  ("a"  "**" "a.**")

     (()  "*"   "*")    ("*"  ()   "*")
     ("*"  "a"  "*.a")  ("*"  "b"  "*.b")
     ("*"  "*"  "*.*")  ("*"  "**" "*.**")

     (()   "**" "**")   ("**" ()   "**")
     ("**" "a"  "**.a") ("**" "b"  "**.b")
     ("**" "*"  "**.*") ("**" "**" "**.**"))))

(test parse-name.smoke
  "Smoke test for `parse-name' function."

  (mapc
   (lambda+ ((input wild-allowed wild-inferiors-allowed expected))
     (case expected
       (name-parse-error
        (signals name-parse-error
          (parse-name input
                      :wild-allowed           wild-allowed
                      :wild-inferiors-allowed wild-inferiors-allowed)))
       (t
        (is (name-equal (make-name expected) (parse-name input))))))

   `(;; Some invalid cases
     (""      t   t   name-parse-error)
     ("."     t   t   name-parse-error)
     (".."    t   t   name-parse-error)
     ("a."    t   t   name-parse-error)
     (".a"    t   t   name-parse-error)
     ("a..b"  t   t   name-parse-error)

     ("a*"    t   t   name-parse-error)
     ("*a"    t   t   name-parse-error)

     ("a**"   t   t   name-parse-error)
     ("**a"   t   t   name-parse-error)

     ("***"   t   t   name-parse-error)

     ("*"     nil t   name-parse-error)
     ("a.*"   nil t   name-parse-error)
     ("*.a"   nil t   name-parse-error)

     ("**"    t   nil name-parse-error)
     ("a.**"  t   nil name-parse-error)
     ("**.a"  t   nil name-parse-error)

     ;; These are valid
     ("a"     t   t   ("a"))
     ("a.b"   t   t   ("a" "b"))
     ("a.b.c" t   t   ("a" "b" "c"))

     ("*"     t   t   (:wild))
     ("a.*"   t   t   ("a" :wild))
     ("*.a"   t   t   (:wild "a"))

     ("**"    t   t   (:wild-inferiors))
     ("a.**"  t   t   ("a" :wild-inferiors))
     ("**.a"  t   t   (:wild-inferiors "a")))))

(test print-name.smoke
  "Smoke test for `print-name' function."

  (mapc
   (lambda+ ((input colon? at? width expected))
     (let* ((name   (make-name input))
            (output (with-output-to-string (stream)
                      (print-name stream name colon? at? width))))
       (is (string= expected output))))

   `((()      nil nil nil "")
     (()      t   nil nil "<root>")

     ("a"     nil nil nil "a")
     ("a"     t   nil nil "a")
     ("a"     nil nil 4   "a   ")

     ("a.b"   nil nil nil "a.b")
     ("a.b"   t   nil nil "a.b")
     ("a.b"   nil nil 4   "a.b ")

     ("*"     nil nil nil "*")
     ("*"     t   nil nil "*")
     ("*"     nil nil 4   "*   ")

     ("*.a"   nil nil nil "*.a")
     ("*.a"   t   nil nil "*.a")
     ("*.a"   nil nil 4   "*.a ")

     ("**"    nil nil nil "**")
     ("**"    t   nil nil "**")
     ("**"    nil nil 4   "**  ")

     ("a.**" nil nil nil "a.**")
     ("a.**" t   nil nil "a.**")
     ("a.**" nil nil 4   "a.**"))))
