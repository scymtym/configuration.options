;;;; ini.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:options.sources)

#. (progn
     (ql:quickload :parser.ini)
     '(ql:quickload :parser.ini))

(defclass ini-syntax ()
  ()
  (:documentation
   "TODO(jmoringe): document"))

(service-provider::register-provider/class 'syntax :ini :class 'ini-syntax)

(defmethod parser.ini:make-node ((builder (eql 'event))
                                 (kind    t)
                                 &rest args &key &allow-other-keys)
  args)

(defmethod parser.ini:add-child ((builder (eql 'event))
                                 (parent  list)
                                 (child   list))
  ;; TODO record location
  (let+ (((&plist-r/o (parent-name :name)) parent)
         ((&plist-r/o (child-name :name)
                      (value      :value)) child)
         (name (mapcar #'make-keyword (append parent-name child-name))))
    (notify *sink* :added     name)
    (notify *sink* :new-value name value)
    parent))

(defmethod process ((syntax ini-syntax)
                    (source stream))
  (let ((content (read-stream-content-into-string source)))
    (parser.ini:parse content 'event))
  (values))


(defclass writer ()
  ((sink :initarg  :sink
         :reader   writer-sink
         :documentation
         ""))
  (:default-initargs
   :sink (missing-required-initarg 'writer :sink))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod notify ((sink  writer)
                   (event (eql :added))
                   (name  t)
                   &optional value)
  "TODO(jmoringe): document"
  (declare (ignore value))
  (let+ (((&accessors-r/o (sink writer-sink)) sink))
    (or (find-option name sink :if-does-not-exist nil)
        (setf (find-option name sink)
              (make-option (find-option name (configuration-schema sink)
                                        :if-does-not-exist #'error)
                           name))))
  (values))

(defmethod notify ((sink  writer)
                   (event (eql :removed))
                   (name  t)
                   &optional value)
  "TODO(jmoringe): document"
  (declare (ignore value))
  (let+ (((&accessors-r/o (sink writer-sink)) sink))
    (setf (find-option name sink) nil))
  (values))

(defmethod notify ((sink  writer)
                   (event (eql :new-value))
                   (name  t)
                   &optional value)
  "TODO(jmoringe): document"
  (let+ (((&accessors-r/o (sink writer-sink)) sink)
         (option (find-option name sink)))
    (setf (option-value option)
          (string->value (option-schema-item option) value)))
  (values))

(let* ((s  (make-instance 'standard-schema))
       (s2 (make-instance 'standard-schema))
       (c  (make-instance 'standard-configuration
                          :schema s)))
  (setf (options::find-child (esrap:parse 'options::name "child.**") s) s2)

  (setf (find-option '("a") s2)
        (make-instance 'standard-schema-item
                       :name '("a")
                       :type '(member :child)))

  (setf (find-option '("a" "b") s)
        (make-instance 'standard-schema-item
                       :name '("a" "b")
                       :type 'integer))
  (setf (find-option '("a" "b" :wild-inferiors) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("a" "b" :wild-inferiors))
                       :type 'string))

  (setf (find-option '("a" "b" :wild) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("a" "b" :wild))
                       :type '(member :wild)))

  (setf (find-option '("rsb" "transport" :wild "enabled") s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '("rsb" "transport" :wild "enabled"))
                       :type 'boolean))

  (setf (find-option '(:wild-inferiors) s)
        (make-instance 'wildcard-schema-item
                       :name (make-instance 'wildcard-name
                                            :components '(:wild-inferiors))
                       :type '(member :a :b)))

  (defvar *sink* (make-instance 'writer :sink c)))
