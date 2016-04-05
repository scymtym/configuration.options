;;;; variables.lisp --- Variables used in the RSB integration.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.rsb)

(declaim (type rsb:scope *configuration-control-scope*
                         *configuration-changes-scope*
                         *schema-control-scope*))

(defvar *configuration-control-scope*
  (rsb:make-scope "/configuration/options/control/" :intern? t))

(defvar *configuration-changes-scope*
  (rsb:make-scope "/configuration/options/changes/" :intern? t))

(defvar *schema-control-scope*
  (rsb:make-scope "/configuration/schema/control/" :intern? t))

;;; helper functions

(defun base-scope (kind scope)
  "Return the `rsb:scope' for KIND and SCOPE.

   KIND has to be one of :control and :changes.
   SCOPE has to be one of :configuration and :schema."
  (etypecase scope
    ((eql :configuration) (ecase kind
                            (:control *configuration-control-scope*)
                            (:changes *configuration-changes-scope*)))
    ((eql :schema)        (ecase kind
                            (:control *schema-control-scope*)
                            (:changes (error "~@<Not specified, yet.~@:>"))))
    ((not keyword)        scope)))

(defun name->scope (kind scope name)
  "Return the `rsb:scope' for KIND, SCOPE and name.

   KIND has to be one of :control and :changes.
   SCOPE has to be one of :configuration and :schema.

   NAME is merged (by appending components at the end) with the base
   scope specified by KIND and SCOPE."
  (declare (type endpoint-name name))
  (rsb:merge-scopes name (base-scope kind scope)))

(defun scope->name (kind scope request-scope)
  "Return the list of name components of REQUEST-SCOPE assuming a base
   scope specified by KIND and SCOPE.
   See `base-scope' for a description of KIND and SCOPE."
  (subseq (rsb:scope-components request-scope)
          (length (rsb:scope-components (base-scope kind scope)))))
