;;;; configuration.options-and-mop.asd --- MOP integration.
;;;;
;;;; Copyright (C) 2013, 2014, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-and-mop
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "MOP-based configuration of CLOS instances."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")
                (:version :closer-mop            "1.0")

                (:version :configuration.options (:read-file-form "version-string.sexp")))
  :components  ((:module     "src"
                 :components ((:file       "mop"))))

  :in-order-to ((test-op (test-op :configuration.options-and-mop/test))))

(defsystem :configuration.options-and-mop/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for MOP integration."
  :depends-on  (:alexandria
                (:version :let-plus                      "0.2")

                (:version :configuration.options-and-mop (:read-file-form "version-string.sexp"))

                (:version :fiveam                        "1.3")

                (:version :configuration.options/test    (:read-file-form "version-string.sexp")))
  :components  ((:module     "test"
                 :components ((:file       "mop")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-and-mop/test))))
  (uiop:symbol-call '#:configuration.options.mop.test '#:run-tests))
