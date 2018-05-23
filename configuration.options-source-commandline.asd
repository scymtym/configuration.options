;;;; configuration.options.asd --- System definition for the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options-source-commandline"
  :description    "Commandline configuration source."
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
                   (:version "architecture.service-provider" "0.1")
                   "log4cl"

                   (:version "configuration.options"         (:read-file-form "version-string.sexp"))

                   "net.didierverna.clon") ; TODO temp for commandline source

  :components     ((:module     "commandline"
                    :pathname   "src/sources/commandline"
                    :serial     t
                    :components ((:file       "package")

                                 (:file       "source"))))

  :in-order-to    ((test-op (test-op "configuration.options-source-commandline/test"))))

(defsystem "configuration.options-source-commandline/test"
  :description "Unit tests for the configuration.options.source-commandline system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                                 "0.2")

                (:version "fiveam"                                   "1.3")

                (:version "configuration.options-source-commandline" (:read-file-form "version-string.sexp"))

                (:version "configuration.options/test"               (:read-file-form "version-string.sexp")))

  :components  ((:module     "commandline"
                 :pathname   "test/sources/commandline"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "source"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:log '#:config :warn)
                 (uiop:symbol-call '#:configuration.options.sources.commandline.test
                                   '#:run-tests)))
