;;;; configuration.options-and-quri.asd --- Quri URIs as option values.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options-and-quri"
  :description "Integration with the quri system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"              "0.2")

                (:version "configuration.options" (:read-file-form "version-string.sexp"))

                "quri")

  :components  ((:module     "src"
                 :components ((:file       "value-type-quri"))))

  :in-order-to ((test-op (test-op "configuration.options-and-quri/test"))))

(defsystem "configuration.options-and-quri/test"
  :description "Unit tests for the quri integration of the options system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                       "0.2")

                (:version "configuration.options-and-quri" (:read-file-form "version-string.sexp"))

                (:version "fiveam"                         "1.3")

                (:version "configuration.options/test"     (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :components ((:file       "value-type-quri"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:configuration.options.quri.test '#:run-tests)))
