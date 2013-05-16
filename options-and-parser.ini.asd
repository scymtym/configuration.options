;;;; options-and-parser.ini.asd --- Ini syntax for the options system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "options.asd" (or *load-truename* *compile-file-truename*)))

(cl:in-package #:options-system)

;;; System definition

(defsystem :options-and-parser.ini
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Ini syntax support for options system."
  :depends-on  (:alexandria
                (:version :let-plus         "0.2")

                (:version :options          #.(version/string))

                (:version :parser.ini       "0.1.0"))
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-ini"))))

  :in-order-to ((test-op (test-op :options-and-parser.ini-test))))

(defsystem :options-and-parser.ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for ini syntax support of options system."
  :depends-on  (:alexandria
                (:version :let-plus               "0.2")

                (:version :options-and-parser.ini #.(version/string))

                :eos
                (:version :options-test           #.(version/string)))
  :components  ((:module     "sources"
                 :pathname   "test/sources"
                 :components ((:file       "syntax-ini")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :options-and-parser.ini-test))))
  (funcall (find-symbol "RUN-TESTS" :options.test)))
