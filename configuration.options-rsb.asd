;;;; configuration.options-rsb.asd --- System definition for the options integration with RSB.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-rsb
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description ""
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")
                :log4cl

                (:version :configuration.options (:read-file-form "version-string.sexp"))

                (:version :cl-rsb                "0.9"))
  :components  ((:module     "rsb"
                 :pathname   "src/rsb"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "protocol")
                              (:file       "variables")
                              (:file       "conversion2") ; TODO types
                              (:file       "conversion") ; TODO RST/protobuf
                              (:file       "mixins")
                              (:file       "sources")
                              (:file       "sinks"))))

  :in-order-to ((test-op (test-op :configuration.options-rsb/test))))

(defsystem :configuration.options-rsb/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests"
  :depends-on  (:alexandria
                (:version :let-plus                   "0.2")

                (:version :configuration.options-rsb  (:read-file-form "version-string.sexp"))

                (:version :fiveam                     "1.3")
                (:version :configuration.options/test (:read-file-form "version-string.sexp")))
  :components  ((:module     "rsb"
                 :pathname   "test/rsb"
                 :components ((:file       "source")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-rsb/test))))
  (uiop:symbol-call '#:configuration.options.rsb.test '#:run-tests))
