;;;; source-cascade.lisp --- Cascades of sources.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

;;; `cascade-source'

(defstruct (indexed-sink (:constructor make-indexed-sink (index sink)))
  (index (missing-required-initarg 'indexed-sink :index)
         :read-only t
         :type      non-negative-integer)
  (sink  (missing-required-initarg 'indexed-sink :sink)
         :read-only t))

(defmethod notify ((sink  indexed-sink)
                   (event t)
                   (name  t)
                   (value t)
                   &rest args
                   &key
                   (index 0)
                   &allow-other-keys)
  (apply #'notify (indexed-sink-sink sink) event name value
         :index (+ (indexed-sink-index sink) index)
         (remove-from-plist args :index)))

(defclass cascade-source (print-items:print-items-mixin)
  ((sources :initarg  :sources
            :type     list
            :reader   source-sources
            :accessor source-%sources
            :initform '()
            :documentation
            "Stores a list of sources in order of priority."))
  (:documentation
   "This source organizes a set of sources into a prioritized cascade.

    Option values from sources with higher priority are shadow or are
    combined with option values from sources with lower priority.

    When subordinate sources are themselves cascades, their priorities
    are integrated into the priorities of the containing cascade."))

(service-provider:register-provider/class
 'source :cascade :class 'cascade-source)

(defmethod shared-initialize :after ((instance   cascade-source)
                                     (slot-names t)
                                     &key
                                     (sources '() sources-supplied?))
  (when sources-supplied?
    (setf (source-%sources instance)
          (map 'list (curry #'apply #'make-source) sources))))

(defmethod print-items:print-items append ((object cascade-source))
  `((:num-sources ,(length (source-sources object)) " (~D)")))

(defmethod initialize ((source cascade-source)
                       (schema t))
  (mapc (rcurry #'initialize schema) (source-%sources source)))

(defmethod process ((source cascade-source) (sink t))
  (let+ (((&labels descendent-count (source)
            (if-let ((sources (source-sources source)))
              (reduce #'+ sources :key (compose #'descendent-count
                                                #'source-sources))
              1))))
    (iter (for child in (source-sources source))
          (for i initially 0 then (+ i (descendent-count child)))
          (with-simple-restart
              (continue "~@<Ignore source ~A and continue with the ~
                         next source.~@:>"
                        child)
            (process child (make-indexed-sink i sink))))))

;;; `config-file-cascade-source'

(defclass config-file-cascade-source (cascade-source)
  ()
  (:documentation
   "This source implements a cascade of file-based sources.

    Names of configuration files are derived from a given base name.

    The following initargs are accepted:

    :prefix PREFIX

      \"/etc/\" and CONFIG-FILE are appended to this prefix to form
      the name of the system-wide (actually prefix-wide) configuration
      file. Defaults to \"/\". Incompatible with :paths.

    :paths PATHS

      A list of directories containing configuration files, each of
      which is merged with the value of CONFIG-FILE to produce a
      configuration file name. Incompatible with :prefix.

    :config-file CONFIG-FILE

      A configuration file basename (without directory) with is merged
      with each element of PATHS to produce configuration file names.

    :sources SOURCES

      This initarg is accepted to maintain the protocol of the
      `cascade-source' superclass, but is incompatible with all other
      initargs."))

(service-provider:register-provider/class
 'source :config-file-cascade :class 'config-file-cascade-source)

(defmethod shared-initialize :around
    ((instance   config-file-cascade-source)
     (slot-names t)
     &rest args
     &key
     (config-file nil                           config-file-supplied?)
     (prefix      "/"                           prefix-supplied?)
     (paths       *default-configuration-files* paths-supplied?)
     (sources     '()                           sources-supplied?)
     &allow-other-keys)
  (cond
    ;; No CONFIG-FILE => SOURCES can be supplied, call next method.
    ((not (or prefix-supplied? paths-supplied? config-file-supplied?))
     (call-next-method))
    ;; CONFIG-FILE and SOURCES have been supplied.
    (sources-supplied?
     (incompatible-initargs 'config-file-cascade-source
                            :prefix      prefix
                            :paths       paths
                            :config-file config-file
                            :sources     sources))
    ;; SOURCES has not been supplied => CONFIG-FILE is required to
    ;; compute sources.
    ((not config-file-supplied?)
     (missing-required-initarg 'config-file-cascade-source :config-file))
    ;; CONFIG-FILE has been supplied, SOURCES has not been supplied =>
    ;; compute sources and call next method.
    (t
     (let ((other-args (remove-from-plist args :prefix :paths :config-file))
           (files      (configuration-files config-file
                                            :prefix     prefix
                                            :file-specs paths)))
       (call-next-method
        instance slot-names
        :sources (mapcar (lambda (file)
                           (let+ (((file &ign) (ensure-list file)))
                             (list* :file :pathname file other-args)))
                         files))))))

(defmethod print-items:print-items append ((object config-file-cascade-source))
  ;; This is only a best-effort approach and may fail.
  (ignore-errors
   (let* ((config-file (make-pathname
                        :directory nil
                        :defaults  (source-pathname
                                    (first (source-sources object)))))
          (name        (namestring config-file)))
     `((:config-file ,name " ~S" ((:before :num-sources)))))))

;;; `directory-source'

(defclass directory-source (cascade-source)
  ((pattern :initarg  :pattern
            :type     (or string pathname)
            :reader   source-pattern
            :documentation
            "A (wild) pathname or pathname designator which will be
             used to collect files for the source."))
  (:default-initargs
   :pattern (missing-required-initarg 'directory-source :pattern)
   :ignore  (load-time-value (curry #'starts-with #\.) t)
   :compare (load-time-value
             (lambda (x y)
               (string< (pathname-name x) (pathname-name y)))
             t))
  (:documentation
   "Collects config files and creates corresponding subordinate sources.

    The following initargs are accepted:

    :pattern PATTERN

      A (wild) pathname or pathname designator which will be used to
      collect files for the source.

    :ignore FUNCTION-OF-ONE-ARGUMENT

      A function or function designator that should be called on
      collected candidate files to decide whether they should be used
      or ignored.

    :compare FUNCTION-OF-TWO-ARGUMENTS

      A function or function designator that should be used to sort
      collected files and thereby determine a processing order."))

(service-provider:register-provider/class
 'source :directory :class 'directory-source)

(defmethod shared-initialize :around ((instance   directory-source)
                                      (slot-names t)
                                      &rest args
                                      &key
                                      pattern
                                      ignore
                                      compare
                                      &allow-other-keys)
  (let+ ((other-args     (remove-from-plist args :pattern :ignore :compare))
         (matching-files (remove-if (disjoin #'uiop:directory-exists-p ignore)
                                    (directory pattern)))
         (files          (sort matching-files compare)))
    (apply #'call-next-method
           instance slot-names
           :sources (mapcar (lambda (file)
                              (list* :file :pathname file other-args))
                            files)
           args)))

(defmethod print-items:print-items append ((object directory-source))
  `((:pattern ,(source-pattern object) " ~S" ((:before :num-sources)))))

;;; `common-cascade-source'

(defclass common-cascade-source (cascade-source)
  ()
  (:default-initargs
   :basename (missing-required-initarg
               'common-cascade-source :basename))
  (:documentation
   "This source implements a typical cascade for commandline programs.

    The cascade consists of the following sources:
    1. Commandline options
    2. Environment variables
    3. Configuration files
       1. ./BASENAME.TYPE
       2. ~/.config/BASENAME.TYPE
       3. PREFIX/etc/BASENAME.TYPE
    4. Default values

    where TYPE defaults to \"conf\" and PREFIX defaults to \"/\"."))

(service-provider:register-provider/class
 'source :common-cascade :class 'common-cascade-source)

(defmethod shared-initialize :around
    ((instance   common-cascade-source)
     (slot-names t)
     &rest args
     &key
     (sources                      '()                               sources-supplied?)
     basename
     (type                         "conf")
     (paths                        (configuration-file-specs
                                    (environment-variable-namify basename)))
     (prefix/commandline           (format nil "~(~A~)-" basename))
     (prefix/environment-variables (environment-variable-namify basename))
     &allow-other-keys)
  (cond
    (sources-supplied?
     (incompatible-initargs 'common-cascade-source
                            :sources  sources
                            :basename basename))
    (t
     (let ((pathname     (make-pathname :type type :defaults basename))
           (name-mapping (when prefix/environment-variables
                           (ignore-meta-configuration-variables
                            (make-environment-variable-name->option-name
                             :prefix prefix/environment-variables)))))
       (call-next-method
        instance slot-names
        :sources `(;; Commandline
                   ,@(when (service-provider:find-provider
                            'source :commandline
                            :if-does-not-exist nil)
                       `((:commandline
                          ,@(when prefix/commandline
                              `(:prefix ,prefix/commandline)))))
                   ;; Environment variables
                   (:environment-variables
                    ,@(when name-mapping
                        `(:name-mapping ,name-mapping)))
                   ;; Configuration files
                   (:config-file-cascade
                    :config-file       ,pathname
                    :paths             ,paths
                    :if-does-not-exist nil
                    ,@(remove-from-plist
                       args :basename :type :paths
                       :prefix/commandline
                       :prefix/environment-variables))
                   ;; Default values
                   (:defaults)))))))

;;; Utilities

(defun ignore-meta-configuration-variables (function)
  (lambda (name)
    (unless (ends-with-subseq +config-files-variable-suffix+ name)
      (funcall function name))))

(defun environment-variable-namify (string)
  (format nil "~:@(~A~)_" (substitute-if-not
                           #\_ (lambda (char)
                                 (or (char<= #\a char #\z)
                                     (char<= #\A char #\Z)
                                     (char<= #\0 char #\9)))
                           string)))
