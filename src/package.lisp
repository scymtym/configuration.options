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
   #:iterate
   #:let-plus
   #:more-conditions
   #:print-items)

  ;; Types
  (:export
   #:non-wild-name-component
   #:wild-name-component
   #:name-component)

  ;; Conditions
  (:export
   #:name-parse-error
   #:name-parse-error-text

   #:no-such-option
   #:no-such-option-name
   #:no-such-option-container

   #:option-condition-option

   #:value-condition-value

   #:no-such-value-error
   #:no-such-value-error-which

   #:option-value-error

   #:option-value-type-error

   #:option-syntax-error
   #:option-syntax-error-type

   #:notification-error
   #:notification-error-sink
   #:notification-error-event
   #:notification-error-name
   #:notification-error-source)

  ;; Name protocol
  (:export
   #:make-name

   #:name-components

   #:name-equal
   #:name-matches
   #:name<
   #:merge-names

   #:parse-name

   #:print-name

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
   #:option-default
   #:option-description)

  ;; Schema item protocol
  (:export
   #:option-class
   #:make-option

   #:validate-value
   #:validate-value-using-type

   #:merge-values
   #:merge-values-using-type

   #:value->string
   #:string->value
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

  ;; Sink protocol
  (:export
   #:notify)

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
