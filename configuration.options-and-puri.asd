;;;; configuration.options-and-puri.asd --- Puri URIs as option values.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options-and-puri"
  :description "Integration with the puri system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"              "0.2")

                (:version "configuration.options" (:read-file-form "version-string.sexp"))

                "puri")

  :components  ((:module     "src"
                 :components ((:file       "value-type-puri"))))

  :in-order-to ((test-op (test-op "configuration.options-and-puri/test"))))

(defsystem "configuration.options-and-puri/test"
  :description "Unit tests for the puri integration of the options system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                       "0.2")

                (:version "configuration.options-and-puri" (:read-file-form "version-string.sexp"))

                (:version "fiveam"                         "1.3")

                (:version "configuration.options/test"     (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :components ((:file       "value-type-puri"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:configuration.options.puri.test '#:run-tests)))
