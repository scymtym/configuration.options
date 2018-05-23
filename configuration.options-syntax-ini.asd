;;;; configuration.options-syntax-ini.asd --- Ini syntax for the options system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "configuration.options-syntax-ini"
  :description    "INI syntax configuration source."
  :license        "LLGPLv3" ; see COPYING file for details.
  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :homepage       "https://github.com/scymtym/configuration.options"
  :bug-tracker    "https://github.com/scymtym/configuration.options/issues"
  :source-control (:git "https://github.com/scymtym/configuration.options.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     ("alexandria"
                   (:version "let-plus"              "0.2")

                   (:version "configuration.options" (:read-file-form "version-string.sexp"))

                   (:version "parser.ini"            "0.4"))

  :components     ((:module     "sources"
                    :pathname   "src/sources"
                    :components ((:file       "syntax-ini"))))

  :in-order-to    ((test-op (test-op "configuration.options-syntax-ini/test"))))

(defsystem "configuration.options-syntax-ini/test"
  :description "Unit tests for the configuration.options-syntax-ini system."
  :license     "LLGPLv3" ; see COPYING file for details.
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                         "0.2")

                (:version "configuration.options-syntax-ini" (:read-file-form "version-string.sexp"))

                (:version "fiveam"                           "1.3")

                (:version "configuration.options/test"       (:read-file-form "version-string.sexp")))

  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-ini"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:configuration.options.sources.syntax-ini.test
                                   '#:run-tests)))
