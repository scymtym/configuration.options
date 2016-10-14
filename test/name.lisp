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

(test wildcard-name.print-object
  "Test `print-object' method specialized on `wildcard-name'."

  (mapc (lambda (name)
          (is (search name (princ-to-string (make-name name)))))
        `("*" "*.a" "**" "a.**")))

(test make-name.smoke
  "Smoke test for `make-name' function."

  (mapc
   (lambda+ ((input expected-name?
              &optional
              expected-wild-name? expected-components expected-converted?))
     (let+ (((&flet do-it () (make-name input))))
       (case expected-name?
         (type-error       (signals type-error (do-it)))
         (name-parse-error (signals name-parse-error (do-it)))
         (t                (let+ (((&values name converted?) (do-it)))
                             (is (typep name 'name))
                             (when expected-wild-name?
                               (is (typep name 'wild-name)))
                             (is (equal expected-components
                                        (name-components name)))
                             (is (eq expected-converted? converted?)
                                 "~@<Converted for ~S was ~S expected ~S.~@:>"
                                 input converted? expected-converted?))))))

   `(;; Some invalid cases
     (1                  type-error)        ; completely wrong type
     (:foo               type-error)        ; likewise

     (""                 name-parse-error)  ; invalid string
     ("."                name-parse-error)  ; likewise
     ("***"              name-parse-error)  ; likewise

     ;; No-op
     (,(make-name "a.b") t nil ("a" "b")         nil)
     (,(make-name "a.*") t t   ("a" :wild)       nil)

     ;; Some non-wild cases.
     ("a"                t nil ("a")             t)
     (("a")              t nil ("a")             nil)
     (,#("a")            t nil ("a")             t)
     ("a.b"              t nil ("a" "b")         t)
     (("a" "b")          t nil ("a" "b")         nil)
     (,#("a" "b")        t nil ("a" "b")         t)
     (("*")              t nil ("*")             nil) ; non-wild with no string
     (("**")             t nil ("**")            nil) ; likewise

     ;; Some wild cases.
     ("*"                t t   (:wild)           t)
     ((:wild)            t t   (:wild)           t)
     ("**"               t t   (:wild-inferiors) t)
     ((:wild-inferiors)  t t   (:wild-inferiors) t))))

(test name-equal.smoke
  "Smoke test for `name-equal' function."

  (mapc
   (lambda+ ((left right args expected))
     (let ((expected (ensure-list expected)))
       (is (equal expected (multiple-value-list
                            (apply #'name-equal left right args))))))

   '((()        ()        ()          t)
     (()        ("a")     ()          (nil 0))
     (()        (:wild)   ()          (nil 0))
     (("a")     ()        ()          (nil 0))
     (("a")     ("a")     ()          t)
     (("a")     (:wild)   ()          (nil 0))
     ((:wild)   ()        ()          (nil 0))
     ((:wild)   ("a")     ()          (nil 0))
     ((:wild)   (:wild)   ()          t)

     (("a")     ("a" "b") ()          (nil 1))
     (("a" "b") ("a")     ()          (nil 1))
     (("a" "b") ("a" "b") ()          t)

     (("a" "b") ("b")     (:start1 1) t)
     (("a" "b") ("a" "b") (:start1 1) (nil 1))
     (("a" "b") ("a")     (:end1 1)   t)
     (("a" "b") ("a" "b") (:end1 1)   (nil 1))

     (("b")     ("a" "b") (:start2 1) t)
     (("a" "b") ("a" "b") (:start2 1) (nil 0))
     (("a")     ("a" "b") (:end2 1)   t)
     (("a" "b") ("a" "b") (:end2 1)   (nil 1)))))

(test map-query-alignments.smoke
  "Smoke test for the `map-query-alignments' function."

  (mapc
   (lambda+ ((query-spec name-spec expected))
     (let+ (((&flet+ expand-spec ((name &key (start 0) end))
               (let ((name (name-components (make-name name))))
                 (values name start (or end (length name))))))
            ((&values query query-start query-end)
             (expand-spec (ensure-list query-spec)))
            ((&values name  name-start  name-end)
             (expand-spec (ensure-list name-spec)))
            (calls '())
            ((&flet set-equal/equal (left right)
               (set-equal left right :test #'equal))))
       (map-query-alignments (lambda (&rest args)
                               (push args calls))
                             query query-start query-end
                             name  name-start  name-end)
       (is (set-equal/equal expected calls))))
   '((#()              #()   ((t   0 0)))
     (#()              "a"   ((nil 0 0)))
     (#()              "*"   ((nil 0 0)))
     (#()              "**"  ((nil 0 0)))
     (#()              "a.b" ((nil 0 0)))
     (#()              "a.c" ((nil 0 0)))

     ("a"              #()   ((nil 0 0)))
     ("a"              "a"   ((t   1 1)))
     ("a"              "*"   ((nil 0 0)))
     ("a"              "**"  ((nil 0 0)))
     ("a"              "a.b" ((nil 1 1)))
     ("a"              "a.c" ((nil 1 1 )))

     ("*"              #()   ((nil 0 0)))
     ("*"              "a"   ((t   1 1)))
     ("*"              "*"   ((t   1 1)))
     ("*"              "**"  ((t   1 1)))
     ("*"              "a.b" ((nil 1 1)))
     ("*"              "a.c" ((nil 1 1 )))

     ("**"             #()   ((t   0 0)))
     ("**"             "a"   ((t   0 1) (nil 1 0)))
     ("**"             "*"   ((t   0 1) (nil 1 0)))
     ("**"             "**"  ((t   0 1) (nil 1 0)))
     ("**"             "a.b" ((t   0 2) (nil 1 1) (nil 1 0)))
     ("**"             "a.c" ((t   0 2) (nil 1 1) (nil 1 0)))

     ("a.b"            #()   ((nil 0 0)))
     ("a.b"            "a"   ((nil 1 1)))
     ("a.b"            "*"   ((nil 0 0)))
     ("a.b"            "**"  ((nil 0 0)))
     ("a.b"            "a.b" ((t   2 2)))
     ("a.b"            "a.c" ((nil 1 1)))

     ("a.c"            #()   ((nil 0 0)))
     ("a.c"            "a"   ((nil 1 1)))
     ("a.c"            "*"   ((nil 0 0)))
     ("a.c"            "**"  ((nil 0 0)))
     ("a.c"            "a.b" ((nil 1 1)))
     ("a.c"            "a.c" ((t   2 2)))

     (("a.c" :start 1) #()   ((nil 1 0)))
     (("a.c" :start 1) "a"   ((nil 1 0)))
     (("a.c" :start 1) "*"   ((nil 1 0)))
     (("a.c" :start 1) "**"  ((nil 1 0)))
     (("a.c" :start 1) "a.b" ((nil 1 0)))
     (("a.c" :start 1) "a.c" ((nil 1 0)))

     (("a.c" :end 1)   #()   ((nil 0 0)))
     (("a.c" :end 1)   "a"   ((t   1 1)))
     (("a.c" :end 1)   "*"   ((nil 0 0)))
     (("a.c" :end 1)   "**"  ((nil 0 0)))
     (("a.c" :end 1)   "a.b" ((nil 1 1)))
     (("a.c" :end 1)   "a.c" ((nil 1 1))))))

(test name-matches.smoke
  "Smoke test for `name-matches' function."

  (mapc
   (lambda+ ((query name extra-args expected))
     (let+ (((&flet do-it ()
               (apply #'name-matches (make-name query) (make-name name)
                      extra-args))))
      (case expected
        (type-error
         (signals type-error (do-it)))
        (t
         (is (eq expected (do-it))
             "~A did ~:[~;not ~]match ~A" query expected name)))))

   `((()           ()          ()                                    t)
     ("a"          "a"         ()                                    t)
     ("a"          "b"         ()                                    nil)
     ("a.b"        "a.b"       ()                                    t)
     ("a.b"        "a.c"       ()                                    nil)
     ("a.b.c"      "a.b.c"     ()                                    t)

     ("*"          "a"         ()                                    t)
     ("*"          "a.b"       ()                                    nil)
     ("a.b.*"      "a.b.c"     ()                                    t)
     ("a.b.*"      "a.d.c"     ()                                    nil)
     ("*.b.c"      "a.b.c"     ()                                    t)
     ("*.b.c"      "a.b.d"     ()                                    nil)

     ("**"         ()          ()                                    t)
     ("**"         "a"         ()                                    t)
     ("**.a"       "a"         ()                                    t)
     ("**.a"       "b"         ()                                    nil)
     ("a.b.**.e"   "a.b.c.d.e" ()                                    t)
     ("a.b.**.e"   "a.b.c.d.f" ()                                    nil)

     ("*.b.**.e"   "a.b.c.d.e" ()                                    t)
     ("*.b.**.e"   "a.c.b.d.e" ()                                    nil)

     ;; Some invalid bounds
     ("a.b.c"      "a.b.c"     (:start1 4)                           type-error)
     ("a.b.c"      "a.b.c"     (          :end1 4)                   type-error)
     ("a.b.c"      "a.b.c"     (:start1 2 :end1 1)                   type-error)
     ("a.b.c"      "a.b.c"     (                  :start2 4)         type-error)
     ("a.b.c"      "a.b.c"     (                            :end2 4) type-error)
     ("a.b.c"      "a.b.c"     (                  :start2 2 :end2 1) type-error)

     ("a.**.c"     "a.b.c"     (:start1 4)                           type-error)
     ("a.**.c"     "a.b.c"     (          :end1 4)                   type-error)
     ("a.**.c"     "a.b.c"     (:start1 2 :end1 1)                   type-error)
     ("a.**.c"     "a.b.c"     (                  :start2 4)         type-error)
     ("a.**.c"     "a.b.c"     (                            :end2 4) type-error)
     ("a.**.c"     "a.b.c"     (                  :start2 2 :end2 1) type-error)

     ;; Some valid bounds
     ("a.b.c"      "a.b.c"     (:start1 1)                           nil)
     ("a.b.c"      "a.b.c"     (          :end1 2)                   nil)
     ("a.b.c"      "a.b.c"     (:start1 1 :end1 2)                   nil)
     ("a.b.c"      "a.b.c"     (                  :start2 1)         nil)
     ("a.b.c"      "a.b.c"     (                            :end2 1) nil)
     ("a.b.c"      "a.b.c"     (                            :end2 2) nil)
     ("a.b.c"      "a.b.c"     (                  :start2 1 :end2 2) nil)

     ("a.*"        "a.b.c"     (:start1 1)                           nil)
     ("a.*"        "a.b.c"     (          :end1 1)                   nil)
     ("a.*"        "a.b.c"     (:start1 1 :end1 1)                   nil)
     ("a.*"        "a.b.c"     (                  :start2 1)         nil)
     ("a.*"        "a.b.c"     (                            :end2 1) nil)
     ("a.*"        "a.b.c"     (                            :end2 2) t)
     ("a.*"        "a.b.c"     (                  :start2 1 :end2 2) nil)

     ("a.**.c"     "a.b.c"     (:start1 1)                           t)
     ("a.**.c"     "a.b.c"     (          :end1 2)                   t)
     ("a.**.c"     "a.b.c"     (:start1 1 :end1 2)                   t)
     ("a.**.c"     "a.b.c"     (                  :start2 1)         nil)
     ("a.**.c"     "a.b.c"     (                            :end2 1) nil)
     ("a.**.c"     "a.b.c"     (                            :end2 2) nil)
     ("a.**.c"     "a.b.c"     (                  :start2 1 :end2 2) nil))))

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
     (flet ((test-one (name)
              (let ((output (with-output-to-string (stream)
                              (print-name stream name colon? at? width))))
                (is (string= expected output)))))
       (test-one input)
       (test-one (make-name input))))

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
