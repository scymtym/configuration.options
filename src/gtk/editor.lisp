;;;; editor.lisp --- Editors for different option types.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.gtk)

(defvar *propagate-value-changes* t
  "TODO(jmoringe): document")

(defmacro define-option-widget (name superclasses slots &body options)
  (let (body
        (other-options '()))
    (dolist (option options)
      (destructuring-case option
        ((:body &rest content)
         (setf body content))
        ((t &rest stuff)
         (declare (ignore stuff))
         (push option other-options))))
   `(progn
      (defclass ,name (,@superclasses
                       associated-option-mixin)
        ,slots
        (:metaclass gobject:gobject-class)
        ,@other-options)

      ,@(when body
          `((defmethod (setf widget-option) :after ((new-value t)
                                                    (widget    ,name))
              ,@body))))))

(define-option-widget option-name (gtk:label)
    ()
  (:body
   (setf (gtk:label-label widget)
         (if new-value
             (options:print-name nil (option-name new-value))
             "<nothing selected>"))))

(define-option-widget option-type (gtk:label)
    ()
  (:body
   (setf (gtk:label-label widget)
         (if new-value
             (princ-to-string (option-type new-value))
             "<nothing selected>"))))

(define-option-widget option-documentation (gtk:text-view)
    ()
  (:body
   (setf (gtk:text-buffer-text (gtk:text-view-buffer widget))
         (cond
           ((not new-value)                  "<nothing selected>")
           ((option-documentation new-value))
           (t                                "<no description>")))))

(define-option-widget value-editor (gtk:frame)
    ()
  (:body
   (when-let ((child (gtk:bin-child widget)))
     (gtk:container-remove widget child))
   (gtk:container-add
    widget (if new-value
               (make-editor widget new-value)
               (make-instance 'gtk:label :label "<nothing selected>")))))

(define-option-widget option-editor (gtk:table)
  ((name          :initform (make-instance 'option-name)
                  :reader   widget-name)
   (type          :initform (make-instance 'option-type)
                  :reader   widget-type)
   (value-editor  :initform (make-instance 'value-editor)
                  :reader   widget-value-editor)
   (documentation :initform (make-instance 'option-documentation)
                  :reader   widget-documentation))
  (:default-initargs
   :n-columns 2 :column-spacing 4
   :n-rows    4 :row-spacing    4)
  (:body
   (setf (widget-option (widget-name          widget)) new-value
         (widget-option (widget-type          widget)) new-value
         (widget-option (widget-value-editor  widget)) new-value
         (widget-option (widget-documentation widget)) new-value)))

(defmethod initialize-instance :after ((instance option-editor)
                                       &key &allow-other-keys)
  (let+ (((&structure-r/o widget- name type value-editor documentation)
          instance))
    (iter (for name  in    '("Name" "Type" "Value"))
          (for child in    (list name type value-editor))
          (for line  :from 0)
          (gtk:table-attach instance (make-instance
                                      'gtk:label :label name :xalign 0)
                            0 1 line (1+ line)
                            :x-options '() :y-options '())
          (gtk:table-attach instance child
                            1 2 line (1+ line)
                            :x-options '() :y-options '()))
    (gtk:table-attach instance documentation
                      0 2 3 4
                      :x-options '() :y-options '())))

;;; Editor

(defmethod make-editor ((widget value-editor)
                        (option standard-option))
  (make-editor-using-type widget option (option-type option)))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   list)
                                   &key
                                   inner-type)
  (declare (ignore inner-type))
  (make-editor-using-type widget option (first type) :inner-type (rest type)))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'boolean))
                                   &key
                                   inner-type)
  (declare (ignore inner-type))
  (make-instance 'gtk:toggle-button :label "bla"))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'integer))
                                   &key
                                   inner-type)
  (let+ (((&optional lower upper) inner-type)
         (adjustment (make-instance 'gtk:adjustment
                                    :step-increment 1))
         (widget     (if (and lower upper (< (- upper lower) 1000))
                         (make-instance 'gtk:h-scale
                                        :adjustment adjustment
                                        :digits     0)
                         (make-instance 'gtk:spin-button
                                        :adjustment adjustment
                                        :climb-rate 1))))
    ;;
    (setf (gtk:adjustment-lower adjustment)
          (typecase lower
            (integer        lower)
            ((cons integer) (1+ (first lower)))
            (t              most-negative-fixnum))
          (gtk:adjustment-upper adjustment)
          (typecase upper
            (integer        upper)
            ((cons integer) (1- (first upper)))
            (t              most-positive-fixnum)))

    (when-let ((value (option-value option :if-does-not-exist nil)))
      (setf (gtk:adjustment-value adjustment) value))

    (gobject:connect-signal
     adjustment "value-changed"
     (lambda (adjustment)
       (setf (option-value option) (floor (gtk:adjustment-value adjustment)))))

    widget))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'string))
                                   &key
                                   inner-type)
  (declare (ignore inner-type))
  (let ((value (option-value option :if-does-not-exist nil)))
    (make-instance 'gtk:entry :text (or value ""))))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'pathname))
                                   &key
                                   inner-type)
  (declare (ignore inner-type))
  (let ((value (option-value option :if-does-not-exist nil)))
    (make-instance 'gtk:file-chooser-button
                   :filename (if value (namestring value) ""))))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'member))
                                   &key
                                   inner-type)
  (let* ((model (make-instance 'gtk:array-list-store))
         (box   (make-instance 'gtk:combo-box :model model)))
    (let ((renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
      (gtk:cell-layout-pack-start box renderer :expand t)
      (gtk:cell-layout-add-attribute box renderer "text" 0))

    (gtk:store-add-column model "gchararray" #'princ-to-string)
    (mapc (curry #'gtk:store-add-item model) inner-type)

    box))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'list))
                                   &key
                                   inner-type)
  (declare (ignore inner-type))
  (gtk:let-ui (gtk:table :var box
                         (gtk:v-box :var editors)
                         :left 0 :right 2 :top 0 :bottom 1
                         :y-options '()
                         (gtk:button :label "+")
                         :left 0 :right 1 :top 1 :bottom 2
                         :x-options '() :y-options '()
                         (gtk:button :label "-")
                         :left 1 :right 2 :top 1 :bottom 2
                         :x-options '() :y-options '())

    (iter (for element in (option-value option :if-does-not-exist '()))
          (gtk:box-pack-end editors (make-editor-using-type widget option '(member) #+no (first inner-type))))

    box))

(defmethod make-editor-using-type ((widget value-editor)
                                   (option standard-option)
                                   (type   (eql 'or))
                                   &key
                                   inner-type)
  (gtk:let-ui (gtk:notebook :var notebook)
    (iter (for inner in inner-type)
          (gtk:notebook-add-page
           notebook
           (make-editor-using-type widget option inner)
           (make-instance 'gtk:label :label (princ-to-string inner))))
    notebook))
