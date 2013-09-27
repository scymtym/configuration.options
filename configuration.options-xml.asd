;;;; configuration.options-xml.asd --- System definition for XML support.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "configuration.options.asd"
                       (or *load-truename* *compile-file-truename*)))

(cl:in-package #:configuration.options-system)

;;; System definition

(defsystem :configuration.options-xml
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "XML-based configuration source."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options #.(version/string))

                (:version :xml.location          "0.2.0"))
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-xml"))))

  :in-order-to ((test-op (test-op :configuration.options-xml-test))))

(defsystem :configuration.options-xml-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the options-and-xml.location system."
  :depends-on  (:alexandria
                (:version :let-plus                   "0.2")

                (:version :configuration.options-xml  #.(version/string))

                (:version :fiveam                     "1.1")
                (:version :configuration.options-test #.(version/string)))
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-xml")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-xml-test))))
  (funcall (find-symbol "RUN-TESTS" :options.test)))
