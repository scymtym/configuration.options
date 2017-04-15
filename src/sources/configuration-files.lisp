;;;; configuration-files.lisp --- Computing configuration file paths.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

;;; User- and system-wide configuration directories

(defun user-configuration-directory
    (&key (user-homedir (user-homedir-pathname)))
  #+unix  (merge-pathnames #P".config/" user-homedir)
  #+win32 user-homedir-pathname)

(defun system-configuration-directory (&key (prefix #P"/"))
  #+unix  (merge-pathnames #P"etc/" prefix)
  #+win32 prefix)

;;; Configuration directory placeholders

(define-constant +config-file-system-placeholder+ "%system" :test #'string=)
(define-constant +config-file-user-placeholder+   "%user"   :test #'string=)
(define-constant +config-file-pwd-placeholder+    "%pwd"    :test #'string=)

(declaim (type list *default-configuration-files*))

(defvar *default-configuration-files*
  `(,+config-file-pwd-placeholder+
    ,+config-file-user-placeholder+
    ,+config-file-system-placeholder+)
  "List of configuration file names in order of decreasing priority.")

;;; Computing configuration file paths

(defun configuration-files (filename
                            &key
                            user-homedir
                            prefix
                            (file-specs *default-configuration-files*))
  "Return a list of configuration file descriptions based on FILENAME.

   Each of the returned descriptions is a list of the form

     (PATHNAME DESCRIPTION)

   To produce the list of descriptions, FILENAME is combined with each
   of the specifications in FILE-SPECS. Such a specification can be

   * file or directory pathnames which are merged with FILENAME

   * the value of `+config-file-pwd-placeholder+' (i.e. the string
     \"%pwd\") which represents a file named FILENAME in the current
     directory.

   * the value of `+config-file-user-placeholder+' (i.e. the string
     \"%user\") which represents a file named FILENAME in the user
     configuration directory as computed by
     `user-configuration-directory' applied to USER-HOMEDIR.

   * the value of `+config-file-system-placeholder+' (i.e. the string
     \"%system\") which represents a file named FILENAME in the system
     configuration directory as computed by
     `system-configuration-directory' applied to PREFIX."
  (let+ ((filename (pathname filename))
         ((&flet process-spec (spec)
            (switch (spec :test #'equal)
              (+config-file-pwd-placeholder+
               (list filename
                     "Current directory file"))

              (+config-file-user-placeholder+
               (list (merge-pathnames
                      filename (apply #'user-configuration-directory
                                      (when user-homedir
                                        (list :user-homedir user-homedir))))
                     "User config file"))

              (+config-file-system-placeholder+
               (list (merge-pathnames
                      filename (apply #'system-configuration-directory
                                      (when prefix
                                        (list :prefix prefix))))
                     "System-wide config file"))

              (t
               (list (merge-pathnames spec filename)
                     "User specified config file"))))))
    (mapcar #'process-spec file-specs)))

;;; From environment variable

(define-constant +config-files-variable-suffix+
    "CONFIG_FILES"
  :test #'string=)

(defun config-files-variable-name (prefix)
  (format nil "~A~A" prefix +config-files-variable-suffix+))

(defun config-files-variable-value (prefix)
  (uiop:getenv (config-files-variable-name prefix)))

(defun configuration-file-specs (prefix)
  "Return a list of configuration file specifications using PREFIX.

   Elements of the returned list are either namestrings or the
   placeholders `+config-file-pwd-placeholder+',
   `+config-file-user-placeholder+' and
   `+config-file-system-placeholder+'.

   PREFIX is used to compute the name of an environment variable
   PREFIX_CONFIG_FILES the value of which is split at \":\" to produce
   the return value, if the variable is defined.

   Otherwise the default list of configuration file specifications,
   which is the value of `*default-configuration-files*', is
   returned."
  (if-let ((value (config-files-variable-value prefix)))
    (split-sequence:split-sequence #\: value)
    *default-configuration-files*))
