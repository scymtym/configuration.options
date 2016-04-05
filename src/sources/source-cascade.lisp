;;;; source-cascade.lisp --- Cascades of sources.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

(defclass cascade-source ()
  ((sources :initarg  :sources
            :type     list
            :reader   source-sources
            :accessor source-%sources
            :initform '()
            :documentation
            "Stores a list of sources in order of priority."))
  (:documentation
   "Instances of this class organize a set of sources into a
    prioritized cascade. Option values from sources with higher
    priority are shadow or are combined with option values from
    sources with lower priority.

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

(defmethod print-items append ((object cascade-source))
  `((:num-sources ,(length (source-sources object)) " (~D)")))

(defmethod initialize ((source cascade-source)
                       (schema t))
  (mapc (rcurry #'initialize schema) (source-%sources source)))

(defmethod process ((source cascade-source) (sink t))
  (let+ (((&labels count1 (source)
            (if (source-sources source)
                (reduce #'+ (source-sources source)
                        :key (compose #'count1 #'source-sources))
                1))))
    (iter (for child in (source-sources source))
          (for i initially 0 then (+ i (count1 child)))
          (restart-case
              (process child (make-indexed-sink i sink))
            (continue (&optional condition)
              :report (lambda (stream)
                        (format stream "~@<Ignore source ~A and ~
                                        continue with the next ~
                                        source.~@:>"
                                child))
              (declare (ignore condition)))))))

;;; `config-file-cascade-source'

(defclass config-file-cascade-source (cascade-source
                                      print-items-mixin)
  ()
  (:documentation
   "Instances of this class construct a cascade of
    configuration-file-based sources, deriving names of configuration
    files from a given base name.

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
     (prefix      "/"  prefix-supplied?)
     (paths       `("./"
                    ,(merge-pathnames
                      ".config/" (user-homedir-pathname))
                    ,(merge-pathnames "etc/" prefix))
                      paths-supplied?)
     (config-file nil config-file-supplied?)
     (sources     ()  sources-supplied?)
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
    ;; PREFIX's sole purpose is being used in the default computation
    ;; of PATHS. Thus supplying both does not make sense.
    ((and prefix-supplied? paths-supplied?)
     (incompatible-initargs 'config-file-cascade-source
                            :prefix prefix
                            :paths  paths))
    ;; SOURCES has not been supplied => CONFIG-FILE is required to
    ;; compute sources.
    ((not config-file-supplied?)
     (missing-required-initarg 'config-file-cascade-source :config-file))
    ;; CONFIG-FILE has been supplied, SOURCES has not been supplied =>
    ;; compute sources and call next method.
    (t
     (let ((other-args (remove-from-plist args :prefix :paths :config-file)))
       (call-next-method
        instance slot-names
        :sources (mapcar (lambda (path)
                           (list* :file
                                  :pathname (merge-pathnames config-file path)
                                  other-args))
                         paths))))))

(defmethod print-items append ((object config-file-cascade-source))
  ;; This best-effort only and may fail.
  (ignore-errors
   (let* ((config-file (make-pathname
                        :directory nil
                        :defaults  (source-pathname
                                    (first (source-sources object)))))
          (name        (namestring config-file)))
     `((:config-file ,name " ~S" ((:before :num-sources)))))))

;;; `directory-source'

(defclass directory-source (cascade-source
                            print-items-mixin)
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
   "Instances of this class collect files according specified rules
    and create subordinate sources for these files.

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
  (let+ ((other-args (remove-from-plist args :pattern :ignore :compare))
         (files (sort (remove-if ignore (directory pattern)) compare)))
    (apply #'call-next-method
           instance slot-names
           :sources (mapcar (lambda (file)
                              (list* :file :pathname file other-args))
                            files)
           args)))

(defmethod print-items append ((object directory-source))
  `((:pattern ,(source-pattern object) " ~S" ((:before :num-sources)))))

;;; `common-cascade-source'

(defclass common-cascade-source (cascade-source
                                 print-items-mixin)
  ()
  (:default-initargs
   :basename (missing-required-initarg
               'common-cascade-source :basename))
  (:documentation
   "Instances of this class construct a cascade which is typical for
    commandline programs.

    The cascade consists of the following sources:
    1. Commandline options
    2. Environment variables
    3. Configuration files
       1. /etc/BASENAME.TYPE
       2. ~/.config/BASENAME.TYPE
       3. ./BASENAME.TYPE
    4. Default values

    where TYPE defaults to \"conf\"."))

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
     (prefix/commandline           (format nil "~(~A~)-" basename))
     (prefix/environment-variables (format nil "~:@(~A~)_" basename))
     &allow-other-keys)
  (cond
    ((not basename)
     (call-next-method))
    (sources-supplied?
     (incompatible-initargs 'common-cascade-source
                            :sources  sources
                            :basename basename))
    (t
     (let ((pathname (merge-pathnames
                      basename (make-pathname :name "_" :type type))))
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
                    ,@(when prefix/environment-variables
                        `(:prefix ,prefix/environment-variables)))
                   ;; Configuration files
                   (:config-file-cascade
                    :config-file       ,pathname
                    :if-does-not-exist nil
                    ,@(remove-from-plist
                       args :basename :type :prefix/commandline
                       :prefix/environment-variables))
                   ;; Default values
                   (:defaults)))))))
