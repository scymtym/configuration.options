;;;; configuration.options.asd --- System definition for the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-source-commandline
  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version        (:read-file-form "version-string.sexp")
  :license        "LLGPLv3" ; see COPYING file for details.
  :description    "Commandline configuration source."
  :homepage       "https://github.com/scymtym/configuration.options"
  :bug-tracker    "https://github.com/scymtym/configuration.options/issues"
  :source-control (:git "https://github.com/scymtym/configuration.options.git")
  :depends-on     (:alexandria
                   :split-sequence
                   (:version :let-plus                      "0.2")
                   (:version :architecture.service-provider "0.1")
                   :log4cl

                   (:version :configuration.options         (:read-file-form "version-string.sexp"))

                   :net.didierverna.clon) ; TODO temp for commandline source
  :encoding       :utf-8
  :components     ((:module     "commandline"
                    :pathname   "src/sources/commandline"
                    :serial     t
                    :components ((:file       "package")

                                 (:file       "source"))))

  :in-order-to    ((test-op (test-op :configuration.options-source-commandline/test))))

(defsystem :configuration.options-source-commandline/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the configuration.options.source-commandline system."
  :depends-on  (:alexandria
                (:version :let-plus                                 "0.2")

                (:version :configuration.options-source-commandline (:read-file-form "version-string.sexp"))

                (:version :fiveam                                   "1.3"))
  :encoding    :utf-8
  :components  ((:module     "commandline"
                 :pathname   "test/sources/commandline"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "source")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-source-commandline/test))))
  (uiop:symbol-call '#:log '#:config :warn)
  (uiop:symbol-call '#:configuration.options.sources.commandline.test
                    '#:run-tests))
