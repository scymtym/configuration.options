;;;; options-and-ini.asd --- System definition for the options system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
  :description "A simple options system that supports multiple options sources."
  :depends-on  (:alexandria
                (:version :let-plus         "0.2")

                (:version :options          #.(version/string))

                (:version :parser.ini       "0.1.0"))
  :components  ((:module     "sources"
                 :pathname   "src/sources"
                 :components ((:file       "syntax-ini"))))

  #+no :in-order-to #+no ((test-op (test-op :options-and-parser.ini-test))))

#+no (defsystem :options-and-parser.ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "A simple options system that supports multiple options sources."
  :depends-on  (:alexandria
                (:version :let-plus "0.2")

                :eos)
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "name")
                              (:file       "schema")))

                (:module     "sources"
                 :pathname   "test/sources"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")))))

#+no (defmethod perform ((op     test-op)
                    (system (eql (find-system :options-and-parser.ini-test))))
  (funcall (find-symbol "RUN-TESTS" :options.test)))
