;;;; source-cascade.lisp --- Unit tests for the cascade source.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

;;; Tests for `cascade-source' class

(test cascade-source.smoke
  "Smoke test for `cascade-source' class."

  (for-all ((file-name-1        (gen-ascii-name))
            (file-name-2        (gen-ascii-name) (string/= file-name-1
                                                           file-name-2))
            (environment-prefix (gen-ascii-name)))
    (let* ((config-file-1      (format nil "/tmp/~A.conf" file-name-1))
           (config-file-2      (format nil "/tmp/~A.conf" file-name-2))
           (environment-prefix (concatenate 'string environment-prefix "_"))
           (environment-name   (format nil "~AA_B" environment-prefix))
           (environment-entry  (format nil "~AA_B=1" environment-prefix)))
      (with-environment-variable (environment-name "1")
        (with-files ((config-file-1 "a.b=2 b.c=3 d=4"))
          (with-source-and-sink
              ((:cascade
                :sources `((:environment-variables :prefix ,environment-prefix)
                           (:file :pathname ,config-file-1
                                  :syntax   :mock)
                           (:file :pathname          ,config-file-2
                                  :syntax            :mock
                                  :if-does-not-exist nil)))
               :sink-var sink)
            (expecting-sink-calls (sink)
              #+sbcl `(:added     ("a" "b") nil :index 0 :entry ,environment-entry)
              #+sbcl `(:new-value ("a" "b") "1" :index 0 :entry ,environment-entry)
              '(:added     ("a" "b") nil :index 1)
              '(:new-value ("a" "b") "2" :index 1)
              '(:added     ("b" "c") nil :index 1)
              '(:new-value ("b" "c") "3" :index 1)
              '(:added     ("d")     nil :index 1)
              '(:new-value ("d")     "4" :index 1))))))))

;;; Tests for `config-file-cascade-source' class

(source-construct-test (config-file-cascade-source.construct
                        :config-file-cascade)
  "Test constructing `config-file-cascade-source' instances."

  ;; Incompatible initargs.
  '((:sources () :prefix "/")             incompatible-initargs)
  '((:sources () :paths ("/" "/" "/"))    incompatible-initargs)
  '((:sources () :config-file "foo.conf") incompatible-initargs)

  '((:paths ("/" "/" "/") :prefix "/")    incompatible-initargs)

  ;; :config-file is missing.
  '((:paths ("/" "/" "/"))                missing-required-initarg)
  '((:prefix "/")                         missing-required-initarg)

  ;; :syntax is missing.
  '((:config-file "foo.conf")             missing-required-initarg)

  ;; These are valid.
  '(()                                    t)
  '((:config-file "foo.conf"
     :syntax      :mock)                  t))

(test config-file-cascade-source.smoke
  "Smoke test for `config-file-cascade-source' class."

  (for-all ((directory (gen-ascii-name))
            (basename  (gen-ascii-name)))
    (let ((prefix (format nil "/tmp/~A/" directory))
          (type   "conf"))
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
            '(:new-value ("d")     "3" :index 2)))))))

;;; Tests for `directory-source' class

(source-construct-test (directory-source.construct :directory)
  "Test constructing `directory-source' instances."

  ;; :pattern and :syntax are missing.
  '(()                                missing-required-initarg)
  '((:pattern "/etc/sysctl.d/*.conf") missing-required-initarg)
  '((:syntax  :mock)                  missing-required-initarg)

  ;; These are valid.
  '((:pattern "/etc/sysctl.d/*.conf"
     :syntax  :mock)                  t))

(test directory-source.smoke
  "Smoke test for `directory-source' class."

  (for-all ((name (gen-ascii-name)))
    (let ((directory (format nil "/tmp/~A/" name))
          (type      "conf"))
      (with-files (((format nil "~A/00-foo.~A" directory type)
                    "b.c=5")
                   ((format nil "~A/01-baz.~A" directory type)
                    "a=4")
                   ((format nil "~A/02-bar.~A" directory type)
                    "a=1 b.c=2 d=3")
                   ((format nil "~A/directory.~A/dummy" directory type)
                    ""))
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
            '(:new-value ("d")     "3" :index 2)))))))

;;; Tests for `common-cascade-source' class

(source-construct-test (common-cascade-source.construct :common-cascade)
  "Test constructing `common-cascade-source' instances."

  ;; :basename and :syntax are missing.
  '(()                     missing-required-initarg)
  '((:paths ("/" "/" "/")) missing-required-initarg)
  '((:prefix "/")          missing-required-initarg)

  ;; :syntax is missing
  '((:basename "foo")      missing-required-initarg)

  ;; These are valid.
  '((:basename                     "foo"
     :syntax                       :mock)
    t)
  '((:basename                     "foo"
     :type                         "ini"
     :syntax                       :mock)
    t)
  '((:basename                     "foo"
     :syntax                       :mock
     :prefix/commandline           "bar-")
    t)
  '((:basename                     "foo"
     :syntax                       :mock
     :prefix/environment-variables "BAR_")
    t))

(test common-cascade-source.smoke
  "Smoke test for `common-cascade-source' class."

  (for-all ((prefix   (gen-ascii-name))
            (basename (gen-ascii-name)))
    (let* ((schema             *simple-schema*)
           (prefix             (format nil "/tmp/~A/" prefix))
           (type               "conf")
           (basename/safe      (substitute #\_ #\! basename))
           (environment-prefix (concatenate 'string basename/safe "_"))
           (environment-name   (format nil "~AB_C" environment-prefix))
           (environment-entry  (format nil "~A=1" environment-name))
           (offset             (if (service-provider:find-provider
                                    'configuration.options.sources::source :commandline
                                    :if-does-not-exist nil)
                                   1 0)))
      (with-environment-variable (environment-name "1")
        (with-config-files (prefix basename type)
            ("a=2 b.c=3 d=4"
             "a=5"
             "b.c=6")
          (with-source-and-sink ((:common-cascade
                                  :prefix   prefix
                                  :basename basename
                                  :type     type
                                  :syntax   :mock)
                                 :schema   schema
                                 :sink-var sink)
            (are-expected-sink-calls
             `(#+sbcl (:added     ("b" "c") nil :index ,(+ 0 offset)
                                  :entry ,environment-entry)
               #+sbcl (:new-value ("b" "c") "1" :index ,(+ 0 offset)
                                  :entry ,environment-entry)

               (:added     ("b" "c") nil :index ,(+ 1 offset))
               (:new-value ("b" "c") "6" :index ,(+ 1 offset))

               (:added     ("a")     nil :index ,(+ 2 offset))
               (:new-value ("a")     "5" :index ,(+ 2 offset))

               (:added     ("a")     nil :index ,(+ 3 offset))
               (:new-value ("a")     "2" :index ,(+ 3 offset))
               (:added     ("b" "c") nil :index ,(+ 3 offset))
               (:new-value ("b" "c") "3" :index ,(+ 3 offset))
               (:added     ("d")     nil :index ,(+ 3 offset))
               (:new-value ("d")     "4" :index ,(+ 3 offset))

               ,@(expected-notify-calls-for-schema-items
                  schema :index (+ 4 offset)))
             (sink-calls sink))))))))
