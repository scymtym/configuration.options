;;;; name.lisp --- Option names.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options)

;;; `wildcard-name' class

(defclass wildcard-name (standard-object
                                sequence)
  ((components :initarg  :components
               :type     list
               :reader   name-components
               :documentation
               ""))
  (:default-initargs
   :components (missing-required-initarg 'wildcard-name :components))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :before ((instance   wildcard-name)
                                      (slot-names t)
                                      &key
                                      (components nil components-supplied?))
  (when components-supplied?
    (check-type components wild-name)))

(defmethod print-object ((object wildcard-name) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/options::print-name/" object)))

;; sequence protocol

(defmethod sequence:make-sequence-like ((sequence wildcard-name)
                                        (length   integer)
                                        &key
                                        initial-element
                                        initial-contents)
  (make-instance
   'wildcard-name
   :components (or (coerce initial-contents 'list)
                   (make-list length
                              :initial-element initial-element))))

(defmethod sequence:length ((sequence wildcard-name))
  (length (name-components sequence)))

(defmethod sequence:elt ((sequence wildcard-name) (index integer))
  (nth index (name-components sequence)))

(defmethod (setf sequence:elt) ((new-value t)
                                (sequence  wildcard-name)
                                (index     integer))
  (setf (nth index (name-components sequence)) new-value))

;; relation protocols

(defmethod name-matches ((query wildcard-name)
                         (name  t))
  (let+ (((&labels+ recur ((&optional query-first  &rest query-rest)
                           (&optional name-first &rest name-rest))
            (etypecase query-first
              (null
               (null name-first))
              (string
               (when (and name-first (string= query-first name-first))
                 (recur query-rest name-rest)))
              ((eql :wild)
               (when name-first
                 (recur query-rest name-rest)))
              ((eql :wild-inferiors)
               (some (curry #'recur query-rest)
                     (cons '() (maplist #'identity name-rest))))))))
    (recur (name-components query) (name-components name))))

(defmethod name-< ((left wildcard-name) (right wildcard-name))
  (let+ (((&labels+ recur ((&optional left-first  &rest left-rest)
                           (&optional right-first &rest right-rest))
            (etypecase left-first
              (null
               t)
              (string
               (typecase right-first
                 (null   nil)
                 (string (recur left-rest right-rest))
                 (t      t)))
              ((eql :wild)
               (typecase right-first
                 ((or null string) nil)
                 ((eql :wild)      (recur left-rest right-rest))
                 (t                t)))
              ((eql :wild-inferiors)
               (when (eq right-first :wild-inferiors)
                 (recur left-rest right-rest)))))))
    (recur (name-components left) (name-components right))))

(defmethod name-< ((left list) (right wildcard-name))
  t)

(defmethod name-< ((left wildcard-name) (right list))
  nil)

(defmethod merge-names ((left t) (right wildcard-name))
  (concatenate 'wildcard-name left right))

;;; Utility functions

(defun print-name (stream name &optional colon? at?)
  "TODO(jmoringe): document"
  (declare (ignore colon? at?))

  (format stream "~{~A~^.~}"
          (map 'list (lambda (component)
                       (case component
                         (:wild           "*")
                         (:wild-inferiors "**")
                         (t               component)))
               name)))
