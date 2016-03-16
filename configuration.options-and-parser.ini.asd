;;;; configuration.options-and-parser.ini.asd --- Ini syntax for the options system.
;;;;
;;;; Copyright (C) 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-and-parser.ini
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Ini syntax support for options system."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                (:version :parser.ini            "0.1.0"))
  :encoding    :utf-8
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-ini"))))

  :in-order-to ((test-op (test-op :configuration.options-and-parser.ini-test))))

(defsystem :configuration.options-and-parser.ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for ini syntax support of options system."
  :depends-on  (:alexandria
                (:version :let-plus                             "0.2")

                (:version :configuration.options-and-parser.ini (:read-file-form "version-string.sexp"))

                (:version :fiveam                               "1.1")

                (:version :configuration.options-test           (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-ini")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-and-parser.ini-test))))
  (funcall (find-symbol "RUN-TESTS" :configuration.options.test)))
