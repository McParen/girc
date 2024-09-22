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
    (setf layout (make-instance 'crt:column-layout :parent main-screen :children
                                (list ;; buffer tab bar is added dynamically
                                      ;; (list 'crt:window :name :buffer-line :height 1)
                                      (list 'crt:window :name :topic :height 1)
                                      (make-instance 'crt:row-layout :name :lay1 :children
                                                     (list (list 'crt:window :name :output)
                                                           ;; the buffers win is added dynamically
                                                           ;;(list 'crt:window :name :buffers :width 13)
                                                           ))
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
                                     ;; poll the server and redraw the display 10 times per second.
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

(defun buffer-line-window (ui)
  (find :buffer-line (crt:leaves (layout ui)) :key #'crt:name))

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

;; used in key bindings and commands
(defun show-buffer-column (&optional (flag t))
  "If t, show the buffers window, if nil, hide it."
  (let ((wbuf (crt:find-node :buffers (slot-value *ui* 'layout))))
    (if flag
        ;; add only if the window doesnt only exist.
        (unless wbuf
          (crt:add-child (crt:find-node :lay1 (slot-value *ui* 'layout))
                         (list 'crt:window :name :buffers :width 13))
          (crt:calculate-layout (slot-value *ui* 'layout))
          (crt:initialize-leaves (slot-value *ui* 'layout)))
        ;; only close, if the window exists.
        (when wbuf
          (close wbuf)
          (setf (crt:children (crt:find-node :lay1 (slot-value *ui* 'layout)))
                (remove wbuf (crt:children (crt:find-node :lay1 (slot-value *ui* 'layout)))))
          (crt:calculate-layout (slot-value *ui* 'layout)))))
  ;; redraw the buffer and resize the input field
  (setf (changedp (current-buffer)) t
        (crt:width (input-field *ui*)) (crt:width (input-window *ui*)))
  (redraw))

;; add a single-line window for a horizontal buffer list like a tab bar.
(defun show-buffer-line (&optional (flag t))
  (let ((wbufl (crt:find-node :buffer-line (slot-value *ui* 'layout))))
    (if flag
        (unless wbufl
          (setf (crt:children (slot-value *ui* 'layout))
                (cons (list 'crt:window :name :buffer-line :height 1)
                      (crt:children (slot-value *ui* 'layout))))
          (crt:calculate-layout (slot-value *ui* 'layout))
          (crt:initialize-leaves (slot-value *ui* 'layout))
          (setf (crt:background (crt:find-node :buffer-line (slot-value *ui* 'layout)))
                (make-instance 'crt:complex-char :simple-char #\space)))
        (when wbufl
          (close wbufl)
          (setf (crt:children (slot-value *ui* 'layout))
                (remove wbufl (crt:children (slot-value *ui* 'layout))))
          (crt:calculate-layout (slot-value *ui* 'layout)))))
  (setf (changedp (current-buffer)) t)
  (redraw))

(defun show-topic-line (&optional (flag t))
  "If t, show the buffers window, if nil, hide it."
  (let ((wtop (crt:find-node :topic (slot-value *ui* 'layout))))
    (if flag
        ;; only add if it doesnt exist
        (unless wtop
          (setf (crt:children (slot-value *ui* 'layout))
                (crt:insert-nth (if (crt:find-node :buffer-line (slot-value *ui* 'layout)) 1 0)
                                (list 'crt:window :name :topic :height 1)
                                (crt:children (slot-value *ui* 'layout))))
          (crt:calculate-layout (slot-value *ui* 'layout))
          (crt:initialize-leaves (slot-value *ui* 'layout))

          ;; reverse the topic display after initializing the window object
          (setf (crt:background (crt:find-node :topic (slot-value *ui* 'layout)))
                (make-instance 'crt:complex-char :simple-char #\space :attributes '(:reverse))))
        ;; only close if it exists
        (when wtop
          (close wtop)
          (setf (crt:children (slot-value *ui* 'layout))
                (remove wtop (crt:children (slot-value *ui* 'layout))))
          (crt:calculate-layout (slot-value *ui* 'layout)))))
  (setf (changedp (current-buffer)) t)
  (redraw))

;; we cant call eval from .gircrc because it requires ncurses to be initialized first.
(defun eval (input)
  "Take an input line, parse command and args and pass them to a handler function."
  (destructuring-bind (cmd . args) (parse-user-input input)
    (crt:save-excursion (input-window *ui*)
      (if cmd
          (let ((fun (cmd::get-command-handler cmd)))
            (if fun
                (apply fun (parse-user-arguments (sb-introspect:function-lambda-list fun) args))
                ;; if no handler was found, use the default handler
                (funcall (lambda (cmd args)
                           (display t "-!- Undefined command: ~A ~A" cmd args))
                         cmd args)))
          ;; if no command was given, send the input to the current buffer target
          (cmd::say args)))))

;; used in girc.lisp
(defun handle-user-command (field)
  "Parse the content of the input line, call the function associated with the user command.

At the moment, no default command is called if the first input token is not a /command.

Bound to #\newline in girc-input-map."
  (let ((input (crt:value field)))
    (when input
      (crt:reset field)
      (eval input)
      ;; redraw the screen if a command has been called from the input line.
      ;; if a command has been directly called from a binding, it has to call redraw explicitely.
      ;; event functions call redraw from connection/handle-server-input.
      (redraw))))
