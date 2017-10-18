;;;; configuration.options-and-puri.asd --- Puri URIs as option values.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-and-puri
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Integration with the puri system."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                :puri)
  :encoding    :utf-8
  :components  ((:module     "src"
                 :components ((:file       "value-type-puri"))))

  :in-order-to ((test-op (test-op :configuration.options-and-puri/test))))

(defsystem :configuration.options-and-puri/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the puri integration of the options system."
  :depends-on  (:alexandria
                (:version :let-plus                       "0.2")

                (:version :configuration.options-and-puri (:read-file-form "version-string.sexp"))

                (:version :fiveam                         "1.3")

                (:version :configuration.options/test     (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :components ((:file       "value-type-puri")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-and-puri/test))))
  (uiop:symbol-call '#:configuration.options.puri.test '#:run-tests))
