;;;; configuration.options-syntax-xml.asd --- System definition for XML support.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-syntax-xml
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "XML-syntax configuration source."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                (:version :xml.location          "0.2.0"))
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-xml"))))

  :in-order-to ((test-op (test-op :configuration.options-syntax-xml-test))))

(defsystem :configuration.options-syntax-xml-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the configuration.options-syntax-xml system."
  :depends-on  (:alexandria
                (:version :let-plus                         "0.2")

                (:version :configuration.options-syntax-xml (:read-file-form "version-string.sexp"))

                (:version :fiveam                           "1.1")
                (:version :configuration.options-test       (:read-file-form "version-string.sexp")))
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-xml")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-syntax-xml-test))))
  (funcall (find-symbol "RUN-TESTS" :configuration.options.test)))
