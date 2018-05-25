;;;; configuration.options.asd --- System definition for the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options"
  :description    "An extensible configuration system that supports multiple option sources."
  :license        "LLGPLv3" ; see COPYING file for details.
  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage       "https://github.com/scymtym/configuration.options"
  :bug-tracker    "https://github.com/scymtym/configuration.options/issues"
  :source-control (:git "https://github.com/scymtym/configuration.options.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     ("alexandria"
                   "split-sequence"
                   (:version "let-plus"                      "0.2")
                   (:version "more-conditions"               "0.3")
                   (:version "utilities.print-items"         "0.1")
                   (:version "utilities.print-tree"          "0.1")
                   (:version "cl-hooks"            "0.2") ; TODO
                   (:version "architecture.service-provider" "0.1")
                   "log4cl"

                   (:version "esrap"                         "0.9"))

  :components     ((:module     "src"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "types")
                                 (:file       "variables")
                                 (:file       "conditions")
                                 (:file       "protocol")
                                 (:file       "util")
                                 (:file       "mixins")
                                 (:file       "name")
                                 (:file       "schema")
                                 (:file       "configuration")
                                 (:file       "value-types")

                                 (:file       "synchronizer")

                                 (:file       "macros")
                                 (:file       "let-plus")

                                 ;; Debugging
                                 (:file       "debug")))

                   (:module     "sources"
                    :pathname   "src/sources"
                    :depends-on ("src")
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "conditions")
                                 (:file       "protocol")

                                 ;; Debugging
                                 (:file       "debug")

                                 ;; Utilities
                                 (:file       "configuration-files")

                                 ;; Sources
                                 (:file       "source-defaults")
                                 (:file       "source-environment-variables")
                                 (:file       "source-stream")
                                 (:file       "source-file")
                                 (:file       "source-cascade"))))

  :in-order-to    ((test-op (test-op "configuration.options/test"))))

(defsystem "configuration.options/test"
  :description "Unit tests for the configuration.options system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"              "0.2")

                (:version "configuration.options" (:read-file-form "version-string.sexp"))

                (:version "fiveam"                "1.3"))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "mocks")

                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "name")
                              (:file       "schema")
                              (:file       "configuration")
                              (:file       "value-types")

                              (:file       "synchronizer")

                              (:file       "macros")
                              (:file       "let-plus")

                              ;; Debugging
                              (:file       "debug")))

                (:module     "sources"
                 :pathname   "test/sources"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              ;; Utilities
                              (:file       "configuration-files")

                              ;; Sources
                              (:file       "source-defaults")
                              (:file       "source-environment-variables")
                              (:file       "source-stream")
                              (:file       "source-file")
                              (:file       "source-cascade"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:configuration.options.test '#:run-tests)))
