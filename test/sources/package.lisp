;;;; package.lisp --- Package definition for unit tests of the sources module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options.sources.test
  (:use
   #:cl
   #:let-plus

   #:eos

   #:options)

  (:export
   #:options.sources.root)

  (:documentation
   "This package contains unit tests for the sources module"))

(cl:in-package #:options.sources.test)

(def-suite options.sources
  :in options
  :description
  "Root unit test suite for the sources module.")
