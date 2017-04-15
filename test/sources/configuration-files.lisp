;;;; configuration-files.lisp --- Unit tests for the configuration file functions.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test configuration-files.smoke
  "Smoke test for the `configuration-files' function."

  (mapc (lambda+ ((arguments expected))
          (let ((files (apply #'configuration-files arguments)))
            (is (equal expected files))))

        `((("application.conf"
            :prefix       "/prefix/"
            :user-homedir "/home/")

           ((,#P"application.conf"
             "Current directory file")
            (#+unix  ,#P"/home/.config/application.conf"
             #+win32 ,#P"/home/application.config"
             "User config file")
            (,#P"/prefix/etc/application.conf"
             "System-wide config file")))

          (("application.conf"
            :prefix       "/prefix/"
            :user-homedir "/home/"
            :file-specs   ("/foo/bar/" "%system" "%user" "%pwd"))

           ((,#P"/foo/bar/application.conf"
             "User specified config file")
            (,#P"/prefix/etc/application.conf"
             "System-wide config file")
            (#+unix  ,#P"/home/.config/application.conf"
             #+win32 ,#P"/home/application.config"
             "User config file")
            (,#P"application.conf"
             "Current directory file"))))))

(test configuration-file-specs.smoke
  "Smoke test for the `configuration-file-specs' function."

  (for-all ((prefix (gen-ascii-name)))
    (let ((name (format nil "~ACONFIG_FILES" prefix)))
      (is (equal '("%pwd" "%user" "%system")
                 (configuration-file-specs prefix)))
      (with-environment-variable (name "%system:foo")
        (is (equal '("%system" "foo")
                   (configuration-file-specs prefix)))))))
