(in-package :de.anvi.girc)

(defparameter *ui* nil "Global variable holding an instance of the user interface.")

(defclass user-interface ()
  ((main-screen
    :initarg       :screen
    :initform      nil
    :accessor      main-screen
    :type          (or null crt:screen)
    :documentation "The main screen, not used for displaying content.")

   (output-window
    :initarg       :output-window
    :initform      nil
    :accessor      output-window
    :type          (or null crt:window)
    :documentation "Window for the display of server and command output.")

   (status-window
    :initarg       :status-window
    :initform      nil
    :accessor      status-window
    :type          (or null crt:window)
    :documentation "Window for the status line.")
   
   (input-window
    :initarg       :input-window
    :initform      nil
    :accessor      input-window
    :type          (or null crt:window)
    :documentation "Window for the user command input field.")

   (input-field
    :initarg       :input-field
    :initform      nil
    :accessor      input-field
    :type          (or null crt:field)
    :documentation "Field for the user command input."))

  (:documentation "Elements of the user input and output interface based on croatoan/ncurses."))

(defmethod initialize-instance :after ((ui user-interface) &key)
  "Initialize the window and field objects that are part of the user interface."
  (with-slots (main-screen input-window status-window output-window input-field) ui
    (setf main-screen   (make-instance 'crt:screen
                                       :input-echoing nil
                                       :input-buffering nil
                                       :process-control-chars t
                                       :cursor-visible t
                                       :enable-colors nil
                                       :enable-function-keys t)
          output-window (make-instance 'crt:window
                                       :height (- (crt:height main-screen) 2)
                                       :width (crt:width main-screen)
                                       :position '(0 0)
                                       :enable-scrolling t)
          status-window (make-instance 'crt:window
                                       :height 1
                                       :width (crt:width main-screen)
                                       :position (list (- (crt:height main-screen) 2) 0))
          input-window  (make-instance 'crt:window
                                       :height 1
                                       :width (crt:width main-screen)
                                       :position (list (1- (crt:height main-screen)) 0)
                                       :enable-function-keys t)
          input-field   (make-instance 'crt:field
                                       :position (list 0 0)
                                       :width (crt:width main-screen)
                                       :window input-window
                                       :style (list :foreground nil :background nil)
                                       :keymap 'girc-input-map
                                       ;; poll the server and update the display twice per second.
                                       :frame-rate 2))
    ;; format the status line
    (setf (crt:background status-window) (make-instance 'crt:complex-char :simple-char #\- :attributes '(:reverse)))
    (refresh ui)))

(defmethod refresh ((ui user-interface) &rest args)
  (with-slots (main-screen input-window status-window output-window input-field) ui
    (crt:refresh input-window)
    (crt:refresh status-window)
    (crt:refresh output-window)))

(defun finalize-user-interface (ui)
  "Cleanly free ncurses object memory."
  (with-slots (input-window status-window output-window) ui
    (close input-window)
    (close status-window)
    (close output-window)
    (crt:end-screen)))

;; triggered by: connect
;; called from: connect / make-instance / after
(defun set-status (connection)
  "Set the status line of the current connection."
  (with-accessors ((swin status-window)) *ui*
    (with-accessors ((nick connection-nickname) (host connection-hostname)) connection
      (crt:clear swin)
      (when (and nick host)
        (crt:move swin 0 1)
        (format swin "[~A]" nick)
        (crt:move swin 0 20)
        (format swin "[~A]" host))

      ;; after we refresh the window, return the cursor to the input line, use save-excursion in echo?
      (crt:refresh swin))))
