;;;; gtk-editor.lisp ---
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    #1=(ql:quickload '(:configuration.options
                       :configuration.options-and-parser.ini
                       :rsb-transport-socket
                       :configuration.options-and-rsb
                       :configuration.options-and-gtk))
    '#1#)

(cl:defpackage #:configuration.options.examples.gtk-editor
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:configuration.options
   #:configuration.options.sources
   #:configuration.options.gtk)

  (:export
   #:main))

(cl:in-package #:configuration.options.examples.gtk-editor)

(setf sb-ext:*muffled-warnings*
      `(or style-warning ,sb-ext:*muffled-warnings*))

(rsb:enable-id-random-state-reseed) ; TODO temp

(defun main ()
  (let* ((name          '("foo" "bar"))
         (schema        (rsb.patterns:with-remote-server
                            (server "socket:/configuration/schema/control/foo/bar"
                                    :converters `((nibbles:octet-vector . (:foo ,@(rsb:default-converter 'nibbles:octet-vector)))))
                          (rsb.patterns:call server "get" rsb.converter:+no-value+)))
         (source        (make-source :rsb/pull :name name))
         (sink          (make-instance 'options.rsb::rsb-sink :name name :method :put)) ; TODO
         (configuration (make-configuration schema))
         (synchronizer  (make-instance
                         'configuration.options::queuing-synchronizer
                         :target (make-instance 'standard-synchronizer
                                                :target configuration))))
    (initialize source schema)
    #+no (com.dvlsoft.clon:help)
    (process source synchronizer)

    (hooks:add-to-hook (event-hook configuration)
                       (lambda (event name value)
                         (print (list event name value options.gtk::*propagate-value-changes*))
                         (notify sink event name value)))

    (gtk:within-main-loop-and-wait
     (handler-bind
         ((error (lambda (condition)
                   (print (bt:current-thread))
                   (princ condition)
                   (sb-debug:print-backtrace)
                   (continue))))

       (let* ((model (make-instance 'option-tree-store))
              (filter (make-instance 'gtk:tree-model-filter :child-model model))
              (mask))

         (hooks:add-to-hook (event-hook configuration)
                            (lambda (event name value)
                              (declare (ignore value))
                              (case event
                                (:added
                                 (let ((option (find-option name configuration)))
                                   (setf (find-option name model) option)))
                                (:remove
                                 ;; TODO
                                 ))))


         #+no (iter (for item in (options schema))
                    (notify synchronizer :added (option-name item) nil))

         (labels ((do-it ()
                    (unwind-protect ; TODO with-inhibited-sink
                         (progn
                           (setf (configuration.options.rsb::sink-inhibited? sink) t)
                           (configuration.options::pump-event synchronizer)
                           (if (configuration.options::pump-event synchronizer) ; TODO
                               (gtk:call-from-gtk-main-loop #'do-it)
                               (gtk:gtk-main-add-timeout 100 #'do-it)))
                      (setf (configuration.options.rsb::sink-inhibited? sink) nil))
                    nil))
           (gtk:call-from-gtk-main-loop #'do-it))

         (gtk:let-ui (gtk:gtk-window
                      :var            window
                      :title          "Configuration"
                      :default-width  800
                      :default-height 600
                      (gtk:h-paned :position 300
                                   (gtk:v-box
                                    (gtk:entry :var search
                                               :primary-icon-stock "gtk-find"
                                               :secondary-icon-stock "gtk-clear")
                                    :fill nil :expand nil
                                    (option-tree-view :var tree :model filter))
                                   (option-editor :var editor)))

                     (gobject:connect-signal search "changed"
                                             (lambda (widget)
                                               (setf mask (gtk:entry-text widget))
                                               (gtk:tree-model-filter-refilter filter)))

                     (gtk:tree-model-filter-set-visible-function
                      filter (lambda (model iterator)
                               (or (not mask)
                                   (let ((item (gtk:tree-node-item
                                                (gtk::get-node-by-iter model iterator))))
                                     (typecase item
                                       (configuration.options::named-mixin
                                        (search mask (format nil "~/options:print-name/"
                                                             (option-name item))))
                                       (t t))))))

                     (let ((selection (gtk:tree-view-selection tree)))
                       (setf (gtk:tree-selection-mode selection) :single)
                       (gobject:connect-signal
                        selection "changed"
                        (lambda (selection)
                          (let* ((row     (first (gtk:tree-selection-selected-rows selection)))
                                 (selected (when row
                                             (gtk:tree-node-item
                                              (gtk::get-node-by-path model row))))
                                 (selected (when (typep selected 'standard-option)
                                             selected)))
                            (setf (widget-option editor) selected))
                          (gtk:widget-show window :all t))))

                     (gobject:connect-signal window "unmap" (lambda (&rest args)
                                                              (declare (ignore args))
                                                              #'gtk:leave-gtk-main))

                     (gtk:widget-show window :all t)))))))
