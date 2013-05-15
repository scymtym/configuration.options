;;;; macros.lisp --- Test for the macros provided by the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.test)

(defvar *schema*
  (define-schema ()
    ("rsb"
     ("logging"
      ("level"
       :type '(member :info :warning)))
     ("transport"
      (:wild
       ("enabled"
        :schema-item-class standard-schema-item
        :option-class 'foo
        :type 'boolean
        #+no :description
        #+no "Bla"
        :default nil)
       (:wild
        :type 'integer))))))

(make-configuration *schema*)
