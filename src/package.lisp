;;;; package.lisp --- Package definition for cl-options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:configuration.options
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

  ;; Restart names
  (:export
   #:retry)

  (:export
   #:*configuration*)

  ;; Conditions
  (:export
   #:name-parse-error
   #:name-parse-error-text

   #:binding-condition
   #:binding-condition-name
   #:binding-condition-container

   #:binding-missing-condition

   #:binding-exists-condition
   #:binding-exists-condition-existing

   #:option-missing-warning
   #:option-missing-error
   #:option-exists-warning
   #:option-exists-error

   #:child-missing-warning
   #:child-missing-error
   #:child-exists-warning
   #:child-exists-error

   #:option-condition-option

   #:value-condition-value

   #:value-missing-condition
   #:value-missing-condition-which
   #:value-missing-warning
   #:value-missing-error

   #:option-value-error

   #:option-syntax-error
   #:option-syntax-error-type

   #:schema-syntax-error
   #:schema-syntax-error-specification

   #:notification-error
   #:notification-error-sink
   #:notification-error-event
   #:notification-error-name
   #:notification-error-source)

  ;; Event hook protocol
  (:export
   #:event-hook)

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
   #:value)

  ;; Container protocol
  (:export
   #:options
   #:map-options
   #:map-matching-options
   #:find-options
   #:find-option)

  ;; Schema protocol
  (:export
   #:schema-children
   #:find-child

   #:make-configuration)

  ;; Option-like protocol
  ;; Applies to options and schema items
  (:export
   #:option-name
   #:option-type
   #:option-default
   #:option-documentation)

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
   #:option-value
   #:option-values)

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

  ;; Synchronizer protocol
  (:export
   #:synchronizer-target

   #:standard-synchronizer)

  ;; Macros
  (:export
   #:map-schema-spec
   #:eval-schema-spec

   #:define-schema)

  ;; Let-plus integration
  (:export
   #:&options-r/o
   #:&options
   #:&options/synchronizer)

  (:documentation
   "This package contains, options and option containers which are
    either parts of a configuration or of a schema. Thus, the basic
    structure is as follows:

    Concept           | Protocol                   | Class(es)                |
    ------------------+----------------------------+--------------------------+
    + container-like  | container protocol         |                          |
      + schema        | schema protocol            | `standard-schema'        |
      + configuration | configuration protocol     | `standard-configuration' |
    + option-like     | option protocol            |                          |
      + schema-item   | schema item protocol class | `standard-schema-item'   |
      + option        | option value protocol      | `standard-option'        |

    Population of configuration options with actual values is handled
    by functions and classes in the `configuration.options.sources'
    package."))
