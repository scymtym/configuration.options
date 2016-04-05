;;;; conversion.lisp --- TODO.
;;;;
;;;; Copyright (C) 2012, 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.rsb)

#. (progn
     #1=(ql:quickload :rsb-tools-common)
     '#1#)

#. (progn
     #1=(unless (find-package "RST.CONFIGURATION")
          (let ((pbf:*proto-load-path* `(,#p"~/code/cor-lab/rst/rst-proto/proto/stable/"
                                            ,#p"~/code/cor-lab/rst/rst-proto/proto/sandbox/")))
            (rsb.tools.common:load-idl
             (append
              (directory #p"~/code/cor-lab/rst/rst-proto/proto/sandbox/rst/type/*.proto")
              (directory #p"~/code/cor-lab/rst/rst-proto/proto/sandbox/rst/configuration/*.proto"))
             :auto
             :purpose '(:serializer :deserializer :packed-size))))
     '#1#)

;;; Conversion for rst.type.Type

(defun type->rst (type)
  (make-instance 'rst.type:type
                 :syntax       "builtin" #+no "protocol-buffer/text"
                 :definition   (prin1-to-string type)
                 :hash         ""
                 :display-name (princ-to-string type)))

(defun rst->type (type)
  (read-from-string (rst.type:type-definition type)))

;;; Conversion for rst.configuration.Value

(defun value->rst (value)
  (let+ (((&values data wire-schema)
          (rsb.converter:domain->wire
           (rsb:default-converter 'nibbles:octet-vector) value)))
    (make-instance 'rst.configuration:value
                   :wire-schema (sb-ext:string-to-octets (string wire-schema))
                   :data        data)))

(defun rst->value (value)
  (rsb.converter:wire->domain
   (rsb:default-converter 'nibbles:octet-vector)
   (rst.configuration:value-data value)
   (make-keyword
    (sb-ext:octets-to-string
     (rst.configuration:value-wire-schema value)))))

;;; Conversion for rst.configuration.{Option, Configuration}

(defun option->rst (option)
  (let+ (((&accessors-r/o (name option-name)) option)
         ((&values value value?) (option-value
                                  option :if-does-not-exist nil))
         (name (with-output-to-string (stream)
                 (print-name stream name))))
   (apply #'make-instance 'rst.configuration:option
          :name name
          (when value?
            (list :value (value->rst value))))))

(defun rst->option (option)
  (apply #'make-instance 'standard-option
         :name (parse-name (rst.configuration:option-name option))
         (when (rst.configuration:option-value? option)
           (list :value (rst->value (rst.configuration:option-value option))))))

(defun configuration->rst (value)
  (let ((result (make-instance 'rst.configuration:configuration)))
    (iter (for option in (options value))
          (vector-push-extend
           (option->rst option)
           (rst.configuration:configuration-option result)))
    result))

(defun rst->configuration (configuration)
  (let ((result (make-instance 'configuration.options:standard-configuration)))
    (iter (for option in-sequence (rst.configuration:configuration-option
                                   configuration))
          (let ((option (rst->option option)))
            (setf (find-option (option-name option) result) option)))
    result))

;;; Conversion for rst.configuration.{SchemaItem, Schema}

(defun schema-item->rst (schema-item)
  (let+ (((&structure-r/o option- name type documentation) schema-item)
         ((&values default default?) (option-default
                                      schema-item :if-does-not-exist nil))
         (name (with-output-to-string (stream)
                 (print-name stream name))))
    (apply #'make-instance 'rst.configuration:schema-item
           :name name
           :type (type->rst type)
           (append
            (when default?
              (list :default (value->rst default)))
            (when documentation
              (list :description documentation))))))

(defun rst->schema-item (schema-item)
  (apply #'make-instance 'standard-schema-item
         :name (parse-name (rst.configuration:schema-item-name schema-item))
         :type (rst->type (rst.configuration:schema-item-type schema-item))
         (when (rst.configuration:schema-item-description? schema-item)
           (list :documentation (rst.configuration:schema-item-description schema-item)))))

(defun schema->rst (schema)
  ;; TODO sub-schemata?
  (let ((result (make-instance 'rst.configuration:schema)))
    (iter (for schema-item in (options schema))
      (vector-push-extend
       (schema-item->rst schema-item)
       (rst.configuration:schema-item result)))
    result))

(defun rst->schema (schema)
  ;; TODO sub-schemata?
  (let ((result (make-instance 'options:standard-schema)))
    (iter (for schema-item in-sequence (rst.configuration:schema-item schema))
          (let ((schema-item (rst->schema-item schema-item)))
            (setf (find-option (option-name schema-item) result)
                  schema-item)))
    result))

;;; Converter for rst.configuration.Schema

(defmethod rsb.converter:wire->domain? ((converter   (eql :foo))
                                        (write-data  simple-array)
                                        (wire-schema (eql :|.rst.configuration.Schema|)))
  (values converter 'standard-schema))

(defmethod rsb.converter:domain->wire? ((converter     (eql :foo))
                                        (domain-object standard-schema))
  (values converter 'nibbles:octet-vector :|.rst.configuration.Schema|))

(defmethod rsb.converter:wire->domain ((converter   (eql :foo))
                                       (write-data  simple-array)
                                       (wire-schema (eql :|.rst.configuration.Schema|) ))
  (rst->schema
   (rsb.converter:wire->domain :protocol-buffer write-data wire-schema)))

(defmethod rsb.converter:domain->wire ((converter     (eql :foo))
                                       (domain-object standard-schema))
  (rsb.converter:domain->wire :protocol-buffer (schema->rst domain-object)))

;;; Converter for rst.configuration.Configuration

(defmethod rsb.converter:wire->domain? ((converter   (eql :foo))
                                        (write-data  simple-array)
                                        (wire-schema (eql :|.rst.configuration.Configuration|)))
  (values converter 'standard-configuration))

(defmethod rsb.converter:domain->wire? ((converter     (eql :foo))
                                        (domain-object standard-configuration))
  (values converter 'nibbles:octet-vector :|.rst.configuration.Configuration|))

(defmethod rsb.converter:wire->domain ((converter   (eql :foo))
                                       (write-data  simple-array)
                                       (wire-schema (eql :|.rst.configuration.configuration|) ))
  (rst->configuration
   (rsb.converter:wire->domain :protocol-buffer write-data wire-schema)))

(defmethod rsb.converter:domain->wire ((converter     (eql :foo))
                                       (domain-object standard-configuration))
  (rsb.converter:domain->wire :protocol-buffer (configuration->rst domain-object)))
