(in-package :de.anvi.girc)

(defclass user-interface ()
  ((main-screen
    :initarg       :screen
    :initform      nil
    :accessor      main-screen
    :type          (or null crt:screen)
    :documentation "The main screen, not used for displaying content.")

   (layout
    :initarg       :layout
    :initform      nil
    :accessor      layout
    :type          (or null crt:layout)
    :documentation "Layout object containing the windows.")

   (input-field
    :initarg       :input-field
    :initform      nil
    :accessor      input-field
    :type          (or null crt:field)
    :documentation "Field for the user command input."))

  (:documentation "Elements of the user input and output interface based on croatoan/ncurses."))

(defmethod initialize-instance :after ((ui user-interface) &key)
  "Initialize the window and field objects that are part of the user interface."
  (with-slots (main-screen layout input-field) ui
    (setf main-screen (make-instance 'crt:screen
                                     :input-echoing nil
                                     :input-buffering nil
                                     :process-control-chars t
                                     :cursor-visible t
                                     :enable-colors nil
                                     :enable-function-keys t))

    (setf layout (make-instance 'crt::column-layout :parent main-screen :children
                                (list (list 'crt:window :name :topic :height 1)
                                      (make-instance 'crt::row-layout :children
                                                     (list (list 'crt:window :name :output)))
                                      (list 'crt:window :name :status :height 1)
                                      (list 'crt:window :name :input :height 1 :enable-function-keys t))))
    ;; calc window positions and dimensions
    (crt:calculate-layout layout)
    ;; make window objects
    (crt:initialize-leaves layout)
    (setf input-field (make-instance 'crt:field
                                     :position (list 0 0)
                                     :width (crt:width main-screen)
                                     :max-buffer-length 400
                                     :window (find :input (crt:leaves layout) :key #'crt:name)
                                     :style (list :foreground nil :background nil)
                                     :keymap 'girc-input-map
                                     ;; poll the server and update the display 10 times per second.
                                     :frame-rate 10))
    ;; reverse the display of the topic and the status line
    (setf (crt:background (find :topic (crt:leaves layout) :key #'crt:name))
          (make-instance 'crt:complex-char :simple-char #\space :attributes '(:reverse)))
    (setf (crt:background (find :status (crt:leaves layout) :key #'crt:name))
          (make-instance 'crt:complex-char :simple-char #\- :attributes '(:reverse)))))

(defparameter *ui* nil "Global variable holding an instance of the user interface.")

(defun topic-window (ui)
  (find :topic (crt:leaves (layout ui)) :key #'crt:name))

(defun output-window (ui)
  (find :output (crt:leaves (layout ui)) :key #'crt:name))

(defun status-window (ui)
  (find :status (crt:leaves (layout ui)) :key #'crt:name))

(defun input-window (ui)
  (find :input (crt:leaves (layout ui)) :key #'crt:name))

(defun buffers-window (ui)
  (find :buffers (crt:leaves (layout ui)) :key #'crt:name))

(defmethod refresh ((ui user-interface) &rest args)
  (with-slots (layout) ui
    (mapc #'crt:touch (crt:leaves layout))
    (mapc #'crt:mark-for-refresh (crt:leaves layout))
    (crt:refresh-marked)))

(defun finalize-user-interface (ui)
  "Cleanly free ncurses object memory."
  (with-slots (layout) ui
    (mapc #'close (crt:leaves layout))
    (crt:end-screen)))

(defun find-node (name1 tree)
  "Return the first node (or leaf) that matches the name."
  (labels ((dfs (name node)
             ;; check if we found a node by name
             (when (and node
                        (eq name (crt:name node)))
               (return-from find-node node))
             ;; if a node contains children
             (when (crt:children node)
               (dolist (ch (crt:children node))
                 (when ch
                   (if (typep ch 'crt::node)
                       (dfs name ch)
                       ;; check the leaf
                       (when (eq name (crt:name ch))
                         (return-from find-node ch))))))))
    (dfs name1 tree)))

;; (let ((wbuf (de.anvi.girc::buffer-window de.anvi.girc::*ui*)))
;; used in key bindings and commands
(defun show-buffer-list (&optional (flag t))
  "If t, show the buffers window, if nil, hide it."
  (let ((wbuf (find-node :buffers (slot-value *ui* 'layout))))
    (if flag
        ;; add only if the window doesnt only exist.
        (unless wbuf
          (crt:add-child (nth 1 (crt:children (slot-value *ui* 'layout)))
                         (list 'crt:window :name :buffers :width 13))
          (crt:calculate-layout (slot-value *ui* 'layout))
          (crt:initialize-leaves (slot-value *ui* 'layout)))
        ;; only close, if the window exists.
        (when wbuf
          (close wbuf)
          (setf (crt:children (nth 1 (crt:children (slot-value *ui* 'layout))))
                (remove wbuf (crt:children (nth 1 (crt:children (slot-value *ui* 'layout))))))
          (crt:calculate-layout (slot-value *ui* 'layout)))))
  ;; redraw the buffer and resize the input field
  (setf (changedp (current-buffer)) t
        (crt:width (input-field *ui*)) (crt:width (input-window *ui*)))
  (update))
