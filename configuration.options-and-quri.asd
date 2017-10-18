;;;; configuration.options-and-quri.asd --- Quri URIs as option values.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-and-quri
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Integration with the quri system."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                :quri)
  :encoding    :utf-8
  :components  ((:module     "src"
                 :components ((:file       "value-type-quri"))))

  :in-order-to ((test-op (test-op :configuration.options-and-quri/test))))

(defsystem :configuration.options-and-quri/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the quri integration of the options system."
  :depends-on  (:alexandria
                (:version :let-plus                       "0.2")

                (:version :configuration.options-and-quri (:read-file-form "version-string.sexp"))

                (:version :fiveam                         "1.3")

                (:version :configuration.options/test     (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :components ((:file       "value-type-quri")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-and-quri/test))))
  (uiop:symbol-call '#:configuration.options.quri.test '#:run-tests))
