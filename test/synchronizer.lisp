;;;; synchronizer.lisp --- Unit tests for synchronizers.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(in-suite options)

(test standard-synchronizer.smoke
  "Smoke test for the `standard-synchronizer' class."

  (let* ((schema        +simple-schema+)
         (configuration (make-configuration schema))
         (synchronizer  (make-instance 'standard-synchronizer
                                       :target configuration)))
    (mapc (lambda+ ((events queries))
            (mapc (lambda+ ((event name value
                                   &rest args &key &allow-other-keys))
                    (apply #'notify synchronizer event (make-name name) value
                           args))
                  events)
            (mapc (lambda+ ((name (value &optional (value? t))))
                    (let ((option (find-option name configuration)))
                      (if value?
                          (is (equal value (option-value option)))
                          (is (null (nth-value
                                     1 (option-value
                                        option :if-does-not-exist nil)))))))
                  queries))

          '((((:added     "foo"     nil              :index 1))
             (("foo" (1))))

            (((:new-value "foo"     "3"    :raw? t   :index 1))
             (("foo" (3))))

            (((:added     "foo"     nil              :index 0)
              (:new-value "foo"     2      :raw? nil :index 0))
             (("foo" (2))))

            (((:added     "bar"     nil              :index 0))
             (("bar" (nil nil))))

            (((:new-value "bar"     "true" :raw? t   :index 0))
             (("bar" (t))))

            (((:added     "bar" nil                  :index 1)
              (:new-value "bar"     nil    :raw? nil :index 1))
             (("bar" (t))))

            (((:added     "foo.fez" nil              :index 0))
             (("foo.fez" (nil nil))))

            (((:new-value "foo.fez" 1      :raw? nil :index 1))
             (("foo.fez" (1))))))
    configuration))
