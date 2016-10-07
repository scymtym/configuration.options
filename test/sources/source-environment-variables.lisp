;;;; source-environment-variable.lisp --- Unit tests for the environment variables source.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources.test)

(in-suite options.sources)

(test environment-variables-source.construction
  "Test constructing `environment-variables-source' instances."

  (signals incompatible-initargs
    (make-source :environment-variables :prefix       "foo"
                                        :name-mapping #'identity)))

(test environment-variables-source.smoke
  "Smoke test for `environment-variables-source' class."

  (let* ((prefix        (concatenate 'string (make-random-string) "_"))
         (rest          (make-random-string))
         #+sbcl (rest/downcase (string-downcase rest))
         (name          (format nil "~A~A" prefix rest))
         (value         "value")
         (entry         (format nil "~A=~A" name value)))
    (with-environment-variable (name value)
      (with-source-and-sink ((:environment-variables :prefix prefix)
                             :sink-var sink)
        (expecting-sink-calls (sink)
          #+sbcl `(:added     (,rest/downcase) nil    :entry ,entry)
          #+sbcl `(:new-value (,rest/downcase) ,value :entry ,entry))))))

(test environment-variables-source.non-default-name-mapping
  "Test non-default variable name mapping in
   `environment-variables-source' class."

  (let* ((name  (make-random-string))
         (value "value")
         (entry (format nil "~A=~A" name value)))
    (with-environment-variable (name value)
      (with-source-and-sink ((:environment-variables
                              :name-mapping (lambda (name1)
                                              (when (string= name1 name)
                                                (list name1))))
                             :sink-var sink)
        (expecting-sink-calls (sink)
          #+sbcl `(:added     (,name) nil    :entry ,entry)
          #+sbcl `(:new-value (,name) ,value :entry ,entry))))))
