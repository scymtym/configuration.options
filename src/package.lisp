;;;; package.lisp --- Package definition for cl-options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:options
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:more-conditions
   #:print-items)

  ;; Types
  (:export
   #:wild-name-component
   #:name-component

   #+maybe-later #:name
   #+maybe-later #:wild-name)

  ;; Conditions
  (:export
   #:name-parse-error
   #:name-parse-error-text

   #:no-such-option
   #:no-such-option-name
   #:no-such-option-container

   )

  ;; Name protocol
  (:export
   #:name-components
   #:name-equal
   #:name-matches
   #:name-<
   #:merge-names

   #:parse-name

   #:wildcard-name)

  ;; Value protocol
  (:export
   #:value
   #:value-using-configuration)

  ;; Container protocol
  (:export
   #:options
   #:find-options
   #:find-option)

  ;; Schema protocol
  (:export
   #:make-configuration)

  ;; Option-like protocol
  ;; Applies to options and schema items
  (:export
   #:option-name
   #:option-type
   #:option-has-default?
   #:option-default
   #:option-description)

  ;; Schema item protocol
  (:export
   #:option-class
   #:make-option

   #:validate-value

   #:value->string
   #:string->value)

  ;; Type-based conversion protocol
  (:export
   #:value->string-using-type
   #:string->value-using-type)

  ;; Option protocol
  (:export
   #:option-configuration
   #:option-schema-item
   #:option-value)

  ;; Configuration protocol
  (:export
   #:configuration-schema

   #:make-option

   #:standard-configuration
   #:standard-option

   #:standard-schema-item
   #:standard-schema

   #:wildcard-option-name
   #:wildcard-schema-item)

  ;; Macros
  (:export
   #:define-schema)

  (:documentation
   "This package contains, options and option containers which are
either parts of a configuration or of a schema. Thus, the basic
structure is as follows:

Concept           | Protocol                   | Class(es)     |
------------------+----------------------------+---------------+
+ container-like  | container protocol         |               |
  + schema        | schema protocol            |               |
  + configuration | configuration protocol     |               |
+ option-like     | option protocol            |               |
  + schema-item   | schema item protocol class | `schema-item' |
  + option        | option value protocol      |               |

Population of configuration options with actual values is handled by
functions and classes in the `options.sources' package."))
