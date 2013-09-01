;;;; option-tree.lisp --- TODO.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:configuration.options.gtk)

;;; `option-tree-store'

(defclass option-tree-store (gtk:tree-lisp-store)
  ((nodes :type       hash-table
          :reader     store-%nodes
          :initform   (make-hash-table :test #'equal)))
  (:metaclass gobject:gobject-class)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod initialize-instance :after ((instance option-tree-store)
                                       &key)
  (gtk:tree-lisp-store-add-column
   instance "gchararray"
   (lambda (thing)
     (if (compute-applicable-methods #'option-name (list thing))
         (string (lastcar (option-name thing)))
         thing)))
  (gtk:tree-lisp-store-add-column
   instance "gchararray"
   (lambda (thing)
     (if (compute-applicable-methods #'option-type (list thing))
         (princ-to-string (option-type thing))
         ""))))

(defmethod find-option ((name      t)
                        (store     option-tree-store)
                        &key
                        if-does-not-exist
                        if-exists)
  (let+ (((&accessors-r/o (nodes store-%nodes)) store)
         (key (name-components name)))
    (values (gethash key nodes))))

(defmethod (setf find-option) ((new-value t)
                               (name      t)
                               (store     option-tree-store)
                               &key
                               if-does-not-exist
                               if-exists)
  (let+ (((&accessors-r/o (nodes store-%nodes)) store)
         ((&labels ensure-node (name value)
            (let* ((key        (name-components name))
                   (parent-key (butlast key))
                   (parent     (if key
                                   (ensure-node parent-key (or (lastcar parent-key) "<root>"))
                                   (gtk:tree-lisp-store-root store))))
              (ensure-gethash
               key nodes
               (let ((node (gtk:make-tree-node :item value)))
                 (gtk:tree-node-insert-at
                  parent node (length (gtk:tree-node-children parent)))
                 node))))))
    (ensure-node name new-value)))

;;; `option-tree-view'

(defclass option-tree-view (gtk:tree-view)
  ()
  (:metaclass gobject:gobject-class)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod initialize-instance :after ((instance option-tree-view)
                                       &key)
  ;; TODO redundant
  (let ((column   (make-instance 'gtk:tree-view-column
                                 :title          "Name"
                                 :sort-column-id 0))
        (renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
    (gtk:tree-view-column-pack-start column renderer)
    (gtk:tree-view-column-add-attribute column renderer "text" 0)
    (gtk:tree-view-append-column instance column))

  (let ((column   (make-instance 'gtk:tree-view-column
                                 :title          "Type"
                                 :sort-column-id 1))
        (renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
    (gtk:tree-view-column-pack-start column renderer)
    (gtk:tree-view-column-add-attribute column renderer "text" 1)
    (gtk:tree-view-append-column instance column)))
