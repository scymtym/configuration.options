;;;; configuration.options-and-parser.ini.asd --- Ini syntax for the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "configuration.options.asd"
                       (or *load-truename* *compile-file-truename*)))

(cl:in-package #:configuration.options-system)

;;; System definition

(defsystem :configuration.options-and-parser.ini
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Ini syntax support for options system."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")

                (:version :configuration.options #.(version/string))

                (:version :parser.ini            "0.1.0"))
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-ini"))))

  :in-order-to ((test-op (test-op :configuration.options-and-parser.ini-test))))

(defsystem :configuration.options-and-parser.ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for ini syntax support of options system."
  :depends-on  (:alexandria
                (:version :let-plus                             "0.2")

                (:version :configuration.options-and-parser.ini #.(version/string))

                (:version :fiveam                               "1.1")

                (:version :configuration.options-test           #.(version/string)))
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-ini")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :configuration.options-and-parser.ini-test))))
  (funcall (find-symbol "RUN-TESTS" :options.test)))
