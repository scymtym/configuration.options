;;;; source-cascade.lisp --- Unit tests for the cascade source.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

(in-suite options.sources)

;;; Tests for `cascade-source' class

(test cascade-source.smoke
  "Smoke test for `cascade-source' class."

  (let ((config-file-1 (format nil "/tmp/~A.conf" (make-random-string)))
        (config-file-2 (format nil "/tmp/~A.conf" (make-random-string))))
    (with-environment-variable ("FOO_A_B" "1")
      (with-files ((config-file-1 "a.b=2 b.c=3 d=4"))
        (with-source-and-sink
            ((:cascade
              :sources `((:environment-variables :prefix "FOO_")
                         (:file :pathname ,config-file-1
                                :syntax   :mock)
                         (:file :pathname          ,config-file-2
                                :syntax            :mock
                                :if-does-not-exist nil)))
             :sink-var sink)
          (expecting-sink-calls (sink)
            '(:added     ("a" "b") nil :index 0)
            '(:new-value ("a" "b") "1" :index 0)
            '(:added     ("a" "b") nil :index 1)
            '(:new-value ("a" "b") "2" :index 1)
            '(:added     ("b" "c") nil :index 1)
            '(:new-value ("b" "c") "3" :index 1)
            '(:added     ("d")     nil :index 1)
            '(:new-value ("d")     "4" :index 1)))))))

;;; Tests for `config-file-cascade-source' class

(test config-file-cascade-source.construct
  "Test constructing `config-file-cascade-source' instances."

  (mapc (lambda+ ((initargs expected))
          (let+ (((&flet do-it ()
                    (apply #'make-source :config-file-cascade initargs))))
            (case expected
              (incompatible-initargs
               (signals incompatible-initargs (do-it)))
              (missing-required-initarg
               (signals missing-required-argument (do-it)))
              (t (do-it)))))
        '(;; Incompatible initargs.
          ((:sources () :prefix "/")             incompatible-initargs)
          ((:sources () :paths ("/" "/" "/"))    incompatible-initargs)
          ((:sources () :config-file "foo.conf") incompatible-initargs)

          ((:paths ("/" "/" "/") :prefix "/")    incompatible-initargs)

          ;; :config-file is missing.
          ((:paths ("/" "/" "/"))                missing-required-initarg)
          ((:prefix "/")                         missing-required-initarg)

          ;; :syntax is missing.
          ((:config-file "foo.conf")             missing-required-initarg)

          ;; These are valid.
          (()                                    t)
          ((:config-file "foo.conf"
            :syntax      :mock)                  t))))

(test config-file-cascade-source.smoke
  "Smoke test for `config-file-cascade-source' class."

  (let ((prefix   (format nil "/tmp/~A/" (make-random-string)))
        (basename (make-random-string))
        (type     "conf"))
    (with-config-files (prefix basename type)
        ("a=1 b.c=2 d=3"
         "a=4"
         "b.c=5")
      (with-source-and-sink ((:config-file-cascade
                              :prefix      prefix
                              :config-file (format nil "~A.~A" basename type)
                              :syntax      :mock)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          '(:added     ("b" "c") nil :index 0)
          '(:new-value ("b" "c") "5" :index 0)
          '(:added     ("a")     nil :index 1)
          '(:new-value ("a")     "4" :index 1)
          '(:added     ("a")     nil :index 2)
          '(:new-value ("a")     "1" :index 2)
          '(:added     ("b" "c") nil :index 2)
          '(:new-value ("b" "c") "2" :index 2)
          '(:added     ("d")     nil :index 2)
          '(:new-value ("d")     "3" :index 2))))))

;;; Tests for `directory-source' class

(test directory-source.construct
  "Test constructing `directory-source' instances."

  (mapc (lambda+ ((initargs expected))
          (let+ (((&flet do-it ()
                    (apply #'make-source :directory initargs))))
            (case expected
              (missing-required-initarg
               (signals missing-required-argument (do-it)))
              (t (do-it)))))
        '(;; :pattern and :syntax are missing.
          (()                                missing-required-initarg)
          ((:pattern "/etc/sysctl.d/*.conf") missing-required-initarg)
          ((:syntax  :mock)                  missing-required-initarg)

          ;; These are valid.
          ((:pattern "/etc/sysctl.d/*.conf"
            :syntax  :mock)                  t))))

(test directory-source.smoke
  "Smoke test for `directory-source' class."

  (let ((directory (format nil "/tmp/~A/" (make-random-string)))
        (type      "conf"))
    (with-files (((format nil "~A/00-foo.~A" directory type)
                  "b.c=5")
                 ((format nil "~A/01-baz.~A" directory type)
                  "a=4")
                 ((format nil "~A/02-bar.~A" directory type)
                  "a=1 b.c=2 d=3"))
      (with-source-and-sink ((:directory
                              :pattern (merge-pathnames
                                        (make-pathname :name :wild :type type)
                                        directory)
                              :syntax  :mock)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          '(:added     ("b" "c") nil :index 0)
          '(:new-value ("b" "c") "5" :index 0)
          '(:added     ("a")     nil :index 1)
          '(:new-value ("a")     "4" :index 1)
          '(:added     ("a")     nil :index 2)
          '(:new-value ("a")     "1" :index 2)
          '(:added     ("b" "c") nil :index 2)
          '(:new-value ("b" "c") "2" :index 2)
          '(:added     ("d")     nil :index 2)
          '(:new-value ("d")     "3" :index 2))))))

;;; Tests for `common-cascade-source' class

(test common-cascade-source.construct
  "Test constructing `common-cascade-source' instances."

  (mapc (lambda+ ((initargs expected))
          (let+ (((&flet do-it ()
                    (apply #'make-source :common-cascade initargs))))
            (case expected
              (incompatible-initargs
               (signals incompatible-initargs (do-it)))
              (missing-required-initarg
               (signals missing-required-argument (do-it)))
              (t (do-it)))))
        '(;; :basename and :syntax are missing.
          (()                     missing-required-initarg)
          ((:paths ("/" "/" "/")) missing-required-initarg)
          ((:prefix "/")          missing-required-initarg)

          ;; :syntax is missing
          ((:basename "foo")      missing-required-initarg)

          ;; These are valid.
          ((:basename "foo"
            :syntax    :mock)     t)
          ((:basename "foo"
            :type      "ini"
            :syntax    :mock)     t))))

(test common-cascade-source.smoke
  "Smoke test for `common-cascade-source' class."

  (let ((prefix   (format nil "/tmp/~A/" (make-random-string)))
        (basename (make-random-string))
        (type     "conf")
        (offset   (if (service-provider:find-provider
                       'options.sources::source :commandline
                       :if-does-not-exist nil)
                      1 0)))
    (with-environment-variable ((format nil "~A_B_C" basename) "1")
      (with-config-files (prefix basename type)
          ("a=2 b.c=3 d=4"
           "a=5"
           "b.c=6")
        (with-source-and-sink ((:common-cascade
                                :prefix   prefix
                                :basename basename
                                :type     type
                                :syntax   :mock)
                               :sink-var sink)
          (expecting-sink-calls (sink)
            `(:added     ("b" "c") nil :index ,(+ 0 offset))
            `(:new-value ("b" "c") "1" :index ,(+ 0 offset))

            `(:added     ("b" "c") nil :index ,(+ 1 offset))
            `(:new-value ("b" "c") "6" :index ,(+ 1 offset))

            `(:added     ("a")     nil :index ,(+ 2 offset))
            `(:new-value ("a")     "5" :index ,(+ 2 offset))

            `(:added     ("a")     nil :index ,(+ 3 offset))
            `(:new-value ("a")     "2" :index ,(+ 3 offset))
            `(:added     ("b" "c") nil :index ,(+ 3 offset))
            `(:new-value ("b" "c") "3" :index ,(+ 3 offset))
            `(:added     ("d")     nil :index ,(+ 3 offset))
            `(:new-value ("d")     "4" :index ,(+ 3 offset))))))))
