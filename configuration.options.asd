;;;; configuration.options.asd --- System definition for the configuration.options system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "A simple options system that supports multiple options sources."
  :depends-on  (:alexandria
                :split-sequence
                (:version :let-plus                      "0.2")
                (:version :more-conditions               "0.3")
                (:version :utilities.print-items         "0.1")
                (:version :utilities.print-tree          "0.1")
                (:version :cl-hooks            "0.2") ; TODO
                (:version :architecture.service-provider "0.1")

                (:version :esrap                         "0.9")

                :net.didierverna.clon) ; TODO temp for commandline source
  :encoding    :utf-8
  :components  ((:module     "src"
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
                              (:file       "conversion")

                              (:file       "synchronizer")

                              (:file       "macros")))

                (:module     "sources"
                 :pathname   "src/sources"
                 :depends-on ("src")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              ;; Sources
                              (:file       "source-defaults")
                              (:file       "source-environment-variables")
                              (:file       "source-commandline")
                              (:file       "source-stream")
                              (:file       "source-file")
                              (:file       "source-cascade"))))

  :in-order-to ((test-op (test-op :configuration.options-test))))

(defsystem :configuration.options-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the configuration.options system."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                (:version :fiveam                "1.1"))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "name")
                              (:file       "mixins")
                              (:file       "configuration")
                              (:file       "schema")
                              (:file       "synchronizer")
                              (:file       "macros")))

                (:module     "sources"
                 :pathname   "test/sources"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              ;; Sources
                              (:file       "source-defaults")
                              (:file       "source-environment-variables")
                              (:file       "source-commandline")
                              (:file       "source-stream")
                              (:file       "source-file")
                              (:file       "source-cascade")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :configuration.options-test))))
  (uiop:symbol-call '#:configuration.options.test '#:run-tests))
