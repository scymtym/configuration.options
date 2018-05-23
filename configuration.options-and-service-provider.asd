;;;; configuration.options-and-service-provider.asd --- Integration with the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options-and-service-provider"
  :description "Configuration for services defined using the architecture.service-provider system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                                "0.2")
                "log4cl"

                (:version "configuration.options"                   (:read-file-form "version-string.sexp"))
                (:version "configuration.options-and-mop"           (:read-file-form "version-string.sexp"))

                (:version "architecture.service-provider"           "0.1")
                (:version "architecture.service-provider-and-hooks" "0.1"))

  :components  ((:module     "service-provider"
                 :pathname   "src/service-provider"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "value-type")
                              (:file       "service-provider")

                              (:file       "hooks"))))

  :in-order-to ((test-op (test-op "configuration.options-and-service-provider/test"))))

(defsystem "configuration.options-and-service-provider/test"
  :description "Unit tests for the architecture.service-provider integration."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                                   "0.2")

                (:version "fiveam"                                     "1.3")

                (:version "configuration.options-and-service-provider" (:read-file-form "version-string.sexp"))

                (:version "architecture.service-provider/test"         "0.1")
                (:version "configuration.options/test"                 (:read-file-form "version-string.sexp")))

  :components  ((:module     "service-provider"
                 :pathname   "test/service-provider"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "value-type")
                              (:file       "service-provider")

                              (:file       "hooks"))))

  :perform     (test-op (operation component)
                 (symbol-call '#:configuration.options.service-provider.test '#:run-tests)))
