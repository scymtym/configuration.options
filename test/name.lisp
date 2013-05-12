;;;; name.lisp --- Unit tests for name-related stuff.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(in-suite options)

(defun %make-name (components)
  (if (intersection '(:wild :wild-inferiors) components)
      (make-instance 'wildcard-name :components components)
      components))

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
                       (name-equal (%make-name expected) name))))))

   '(;; Some invalid cases
     (()                type-error)
     (("a")             type-error)
     (("a" "b")         type-error)

     ;; These are valid
     ((:wild)           (:wild))
     ((:wild-inferiors) (:wild)))))

(test name-matches.smoke
  "Smoke test for `name-matches' function."

  (mapc
   (lambda+ ((query name expected))
     (is (eq expected
             (name-matches (%make-name query) (%make-name name)))))

   `((("a" "b" :wild)                     ("a" "b" "c")                          t)
     (("a" "b" :wild "d")                 ("a" "b" "c" "d")                      t)
     (("a" "b" :wild-inferiors "e")       ("a" "b" "c" "d" "e")                  t)
     (("rsb" "transport" :wild "enabled") ("rsb" "transport" "spread" "enabled") t))))

(test name-<.smoke
  "Smoke test for `name-<' function."

  (mapc
   (lambda+ ((left right expected))
     (is (eq expected (name-< (%make-name left) (%make-name right)))))

   `((()                ()                nil)
     (()                ("a")             t)
     (()                ("b")             t)
     (()                (:wild)           t)
     (()                (:wild-inferiors) t)

     (("a")             ()                nil)
     (("a")             ("a")             nil)
     (("a")             ("b")             nil)
     (("a")             (:wild)           t)
     (("a")             (:wild-inferiors) t)

     ((:wild)           ()                nil)
     ((:wild)           ("a")             nil)
     ((:wild)           ("b")             nil)
     ((:wild)           (:wild)           t)
     ((:wild)           (:wild-inferiors) t)

     ((:wild-inferiors) ()                nil)
     ((:wild-inferiors) ("a")             nil)
     ((:wild-inferiors) ("b")             nil)
     ((:wild-inferiors) (:wild)           nil)
     ((:wild-inferiors) (:wild-inferiors) t))))

(test merge-names.smoke
  "Smoke test for `merge-names' function."

  (mapc
   (lambda+ ((left right expected))
     (is (name-matches (%make-name expected)
                       (merge-names (%make-name left) (%make-name right)))))

   `((()                ()                ())
     (()                ("a")             ("a"))
     (()                ("b")             ("b"))
     (()                (:wild)           (:wild))
     (()                (:wild-inferiors) (:wild-inferiors))

     (("a")             ()                ("a"))
     (("a")             ("a")             ("a" "a"))
     (("a")             ("b")             ("a" "b"))
     (("a")             (:wild)           ("a" :wild))
     (("a")             (:wild-inferiors) ("a" :wild-inferiors))

     ((:wild)           ()                (:wild))
     ((:wild)           ("a")             (:wild "a"))
     ((:wild)           ("b")             (:wild "b"))
     ((:wild)           (:wild)           (:wild :wild))
     ((:wild)           (:wild-inferiors) (:wild :wild-inferiors))

     ((:wild-inferiors) ()                (:wild-inferiors))
     ((:wild-inferiors) ("a")             (:wild-inferiors "a"))
     ((:wild-inferiors) ("b")             (:wild-inferiors "b"))
     ((:wild-inferiors) (:wild)           (:wild-inferiors :wild))
     ((:wild-inferiors) (:wild-inferiors) (:wild-inferiors :wild-inferiors)))))

(test parse-name.smoke
  "Smoke test for `parse-name' function."

  (mapc
   (lambda+ ((input expected))
     (case expected
       (name-parse-error
        (signals name-parse-error (parse-name input)))
       (t
        (is (name-equal (%make-name expected) (parse-name input))))))

   `(;; Some invalid cases
     (""      name-parse-error)
     ("."     name-parse-error)
     (".."    name-parse-error)
     ("a."    name-parse-error)
     (".a"    name-parse-error)
     ("a..b"  name-parse-error)

     ("a*"    name-parse-error)
     ("*a"    name-parse-error)

     ("a**"   name-parse-error)
     ("**a"   name-parse-error)

     ;; These are valid
     ("a"     ("a"))
     ("a.b"   ("a" "b"))
     ("a.b.c" ("a" "b" "c"))

     ("*"     (:wild))
     ("a.*"   ("a" :wild))
     ("*.a"   (:wild "a"))

     ("**"    (:wild-inferiors))
     ("a.**"  ("a" :wild-inferiors))
     ("**.a"  (:wild-inferiors "a")))))

(test print-name.smoke
  "Smoke test for `print-name' function."

  (mapc
   (lambda+ ((input colon? at? width expected))
     (let* ((name   (if (emptyp input) '() (parse-name input)))
            (output (with-output-to-string (stream)
                      (print-name stream name colon? at? width))))
       (is (string= expected output))))

   `((""      nil nil nil "")
     (""      t   nil nil "<root>")

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
