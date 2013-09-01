;;;; configuration.options-and-gtk.asd --- System definition for GTK integration of the configuration.options system.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :configuration.options-and-gtk
  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version        (:read-file-form "version-string.sexp")
  :license        "LLGPLv3" ; see COPYING file for details.
  :description    ""
  :homepage       "https://github.com/scymtym/configuration.options"
  :bug-tracker    "https://github.com/scymtym/configuration.options/issues"
  :source-control (:git "https://github.com/scymtym/configuration.options.git")
  :depends-on     (:alexandria
                   (:version :let-plus                         "0.2")

                   (:version :configuration.options            (:read-file-form "version-string.sexp"))
                   (:version :configuration.options-syntax-ini (:read-file-form "version-string.sexp")) ; TODO temp
                   (:version :configuration.options-rsb        (:read-file-form "version-string.sexp")) ; TODO temp

                   :lparallel

                   :cl-gtk2-gtk)
  :components     (

                   (:file "src/queuing-synchronizer")  ; TODO temp

                   (:module     "sources"
                    :pathname   "src/gtk"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "mixins")
                                 (:file       "editor")
                                 (:file       "option-tree")))))
