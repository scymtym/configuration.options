;;;; syntax-xml.lisp --- Use XML documents as configuration sources.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defclass xml-syntax ()
  ;; TODO this slot is duplicated in `ini-syntax'
  ((source         :initarg  :source
                   :reader   syntax-source
                   :documentation
                   "Stores the source originally containing the
                    configuration information (e.g. a
                    `file-source'). The sole purpose of storing the
                    source is the possibility to annotate provided
                    options with the source from which they
                    originated.")
   (option-pattern :initarg  :option-pattern
                   :type     string
                   :reader   syntax-option-pattern
                   :documentation
                   "Stores the XPath pattern XML elements
                    corresponding to options have to match.

                    An example could be \"group/option\" for documents
                    of the form

                      <group>
                        <option ...>
                        ...")
   (name-pattern   :initarg  :name-pattern
                   :type     string
                   :reader   syntax-name-pattern
                   :documentation
                   "Stores the XPath pattern XML nodes corresponding
                    to option names have to match.

                    The XPath is evaluated with the elements matching
                    the `option-pattern' as context node.

                    An example could be \"@name\" for option elements
                    of the form

                      <option name=\"NAME\">VALUE</option>")
   (value-pattern  :initarg  :value-pattern
                   :type     string
                   :reader   syntax-value-pattern
                   :documentation
                   "Stores the XPath pattern XML nodes corresponding
                    to option values have to match.

                    The XPath is evaluated with the elements matching
                    the `option-pattern' as context node.

                    An example could be \"text()\" for option elements
                    of the form

                      <option name=\"NAME\">VALUE</option>"))
  (:default-initargs
   :source         (missing-required-initarg 'xml-syntax :source)
   :option-pattern (missing-required-initarg 'xml-syntax :option-pattern)
   :name-pattern   (missing-required-initarg 'xml-syntax :name-pattern)
   :value-pattern  (missing-required-initarg 'xml-syntax :value-pattern))
  (:documentation
   "This syntax allows using some kinds of XML documents as
    configuration sources via XPath patterns for option elements,
    option names and option values."))

(service-provider:register-provider/class
 'syntax :xml :class 'xml-syntax)

(defmethod process-content ((syntax xml-syntax)
                            (source stream)
                            (sink   t))
  (let+ ((source (if (eq (stream-element-type source) '(unsigned-byte 8))
                     source
                     (sb-ext:string-to-octets
                      (read-stream-content-into-string source))))
         ((&structure-r/o syntax- option-pattern name-pattern value-pattern)
          syntax)
         (document (cxml:parse source (stp:make-builder))))
    (xloc:with-locations-r/o (((:loc options) option-pattern
                               :if-multiple-matches :all))
        document
      (iter (for option in options)
            (restart-case
                (xloc:with-locations-r/o ((name  name-pattern)
                                          (value value-pattern))
                    option
                  (let+ ((name (parse-name name :wild-allowed nil))
                         ((&flet notify (event &optional value)
                            (notify sink event name value
                                    :source (syntax-source syntax)))))
                    (notify :added)
                    (notify :new-value value)))
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Ignore option ~A.~@:>" option))
                (declare (ignore condition)))))))
  (values))
