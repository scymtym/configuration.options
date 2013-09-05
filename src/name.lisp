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
               "Stores the components of the name."))
  (:default-initargs
   :components (missing-required-initarg 'wildcard-name :components))
  (:documentation
   "Instances of this class represent names which contain at least one
    wild component."))

(defmethod shared-initialize :before ((instance   wildcard-name)
                                      (slot-names t)
                                      &key
                                      (components nil components-supplied?))
  (when components-supplied?
    (check-type components wild-name)))

(defmethod print-object ((object wildcard-name) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/options:print-name/" object)))

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

;; construction protocol

(defmethod make-name ((thing string))
  (values (parse-name thing)))

(defmethod make-name ((thing sequence))
  (etypecase thing
    (wild-name (make-instance 'wildcard-name :components thing))
    (name      (coerce thing 'list))))

(defmethod make-name ((thing wildcard-name))
  thing)

;; relation protocols

(defmethod name-matches ((query wildcard-name)
                         (name  t))
  (let+ (((&labels+ recur ((            &optional query-first &rest query-rest)
                           (&whole name &optional name-first  &rest name-rest))
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
                     (cons '() (maplist #'identity name))))))))
    (recur (name-components query) (name-components name))))

(defmethod name< ((left sequence) (right wildcard-name))
  (or (not (typep left 'wild-name)) (call-next-method)))

(defmethod name< ((left wildcard-name) (right sequence))
  (and (typep right 'wild-name) (call-next-method)))

(defmethod merge-names ((left t) (right wildcard-name))
  (concatenate 'wildcard-name left right))

;;; Name grammar and parsing

(defvar *parse-wild-allowed* t
  "Controls whether \"*\" is accepted and interpreted as :wild
   component when parsing names.")

(defvar *parse-wild-inferiors-allowed* t
  "Controls whether \"**\" is accepted and interpreted
   as :wild-inferiors component when parsing names.")

#+esrap.grammar-class
(esrap:defgrammar #:options.option-name
  (:documentation
   "This grammar contains rules for parsing option names consisting of
    multiple string components separated by \".\". Wildcards of the
    form \"*\" (\"wild\") and \"**\" (\"wild inferiors\") are also
    supported."))
#+esrap.grammar-class
(esrap:in-grammar #:options.option-name)

(esrap:defrule wild-inferiors
    (and #\* #\*)
  (:constant :wild-inferiors)
  (:when *parse-wild-inferiors-allowed*))

(esrap:defrule wild
    #\*
  (:constant :wild)
  (:when *parse-wild-allowed*))

(esrap:defrule escaped/character
    (and #\\ (or #\" #\\))
  (:function second))

;; TODO should we use \. instead?
(esrap:defrule component/quoted
    (and #\" (+ (or escaped/character (not (or #\\ #\")))) #\")
  (:function second))

(esrap:defrule component/stringish
    (+ (or component/quoted (not (or #\. #\*))))
  (:text t))

(esrap:defrule component
    (or wild-inferiors wild component/stringish))

(esrap:defrule dot-and-component
    (and #\. component)
  (:function second))

(esrap:defrule name
    (and component (* dot-and-component))
  (:destructure (first rest)
    (let ((components (cons first rest)))
      (if (every #'stringp components)
          components
          (make-instance 'wildcard-name :components components)))))

(defun parse-name (string &key
                          (start 0)
                          end
                          junk-allowed
                          (wild-allowed           *parse-wild-allowed*)
                          (wild-inferiors-allowed *parse-wild-inferiors-allowed*))
  "Parse STRING as an option name and return the result.

   START and END, when supplied, select a sub-string of STRING for
   parsing.

   WILD-ALLOWED and WILD-INFERIORS-ALLOWED control whether \"*\" and
   \"**\" respectively are accepted as name components in STRING."
  (with-condition-translation
      (((esrap:esrap-error name-parse-error
                           :var           condition
                           :cause-initarg nil)
        :text (esrap:esrap-error-text condition)))
    (let ((*parse-wild-allowed*           wild-allowed)
          (*parse-wild-inferiors-allowed* wild-inferiors-allowed))
      (esrap:parse 'name string
                   #+esrap.grammar-class :grammar
                   #+esrap.grammar-class '#:options.option-name
                   :start        start
                   :end          end
                   :junk-allowed junk-allowed))))

;;; Utility functions

(defun print-name (stream name &optional colon? at? width)
  "Print dot-separated components of NAME onto STREAM. If WIDTH is
   supplied pad output to WIDTH.

   If COLON? is non-NIL, print the empty name as \"<root>\" instead of
   the empty string."
  (declare (ignore at?))

  (check-type width (or null positive-integer))
  (let ((components (map 'list (lambda (component)
                                 (case component
                                   (:wild           "*")
                                   (:wild-inferiors "**")
                                   (t               component)))
                         name)))
    (cond
      ((and (not components) colon?)
       (format stream "<root>"))
      (width
       (format stream "~V@<~{~A~^.~}~>" width components))
      (t
       (format stream "~{~A~^.~}" components)))))
