;;;; debug.lisp --- Test for debug functionality.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.test)

(def-suite debug
  :in options
  :description
  "Test suite for debug functionality provided by the
   configuration.options system.")
(in-suite debug)

(test with-level.smoke
  "Smoke test for the `with-level' macro."

  (configuration.options.debug:with-level (:next)
    (is (= 0 configuration.options.debug::*level*))
    (configuration.options.debug:with-level (:next)
      (is (= 1 configuration.options.debug::*level*))))
  (configuration.options.debug:with-level (2)
    (is (= 2 configuration.options.debug::*level*))))

(test with-indent.smoke
  "Smoke test for the `with-indent' macro."

  (is (string= #.(format nil "  foo~%")
               (with-output-to-string (stream)
                 (let ((configuration.options.debug::*stream* stream))
                   (configuration.options.debug:with-level (0)
                     (configuration.options.debug:with-indent (2)
                       (configuration.options.debug:output "foo")))))))

  (is (string= "  foo"
               (with-output-to-string (stream)
                 (let ((configuration.options.debug::*stream* stream))
                   (configuration.options.debug:with-level (1)
                     (configuration.options.debug:with-indent (2)
                       (configuration.options.debug:output "foo"))))))))
