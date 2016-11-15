;;;; configuration.options-syntax-ini.asd --- Ini syntax for the options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-syntax-ini
  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version        (:read-file-form "version-string.sexp")
  :license        "LLGPLv3" ; see COPYING file for details.
  :description    "Ini syntax configuration source."
  :homepage       "https://github.com/scymtym/configuration.options"
  :bug-tracker    "https://github.com/scymtym/configuration.options/issues"
  :source-control (:git "https://github.com/scymtym/configuration.options.git")
  :depends-on     (:alexandria
                   (:version :let-plus              "0.2")

                   (:version :configuration.options (:read-file-form "version-string.sexp"))

                   (:version :parser.ini            "0.4"))
  :encoding       :utf-8
  :components     ((:module     "sources"
                    :pathname   "src/sources"
                    :components ((:file       "syntax-ini"))))

  :in-order-to    ((test-op (test-op :configuration.options-syntax-ini-test))))

(defsystem :configuration.options-syntax-ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the configuration.options-syntax-ini system."
  :depends-on  (:alexandria
                (:version :let-plus                         "0.2")

                (:version :configuration.options-syntax-ini (:read-file-form "version-string.sexp"))

                (:version :fiveam                           "1.3")

                (:version :configuration.options-test       (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-ini")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-syntax-ini-test))))
  (uiop:symbol-call '#:configuration.options.sources.syntax-ini.test
                    '#:run-tests))
