;;;; protocol.lisp ---
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources.test)

#+no (make-source '((:commandline)
               (:environment-variables :prefix   "RSB_")
               (:ini-file              :pathname "rsb.conf")
               (:ini-file              :pathname "~/.config/rsb.conf")
               (:ini-file              :pathname "/etc/rsb.conf")))
