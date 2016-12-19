;;;; source-defaults.lisp --- Provide defaults as values.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.sources)

(defclass defaults-source ()
  ((schema :accessor source-%schema))
  (:documentation
   "This source assigns default values to options."))

(service-provider:register-provider/class
 'source :defaults :class 'defaults-source)

(defmethod initialize ((source defaults-source)
                       (schema t))
  (setf (source-%schema source) schema))

(defmethod process ((source defaults-source)
                    (sink   t))
  (map-options
   (lambda (schema-item &key prefix &allow-other-keys)
     (let+ ((name (merge-names prefix (option-name schema-item)))
            ((&values default default?)
             (option-default schema-item :if-does-not-exist nil)))
       (unless (typep name 'wild-name)
         (notify sink :added name nil)
         (when default?
           (notify sink :new-value name default
                   :source :default
                   :raw?   nil)))))
   (source-%schema source)))
