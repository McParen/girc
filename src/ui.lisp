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
                                       :enable-function-keys t
                                       ;; note that when this is nil, we plan to perform work during the nil event.
                                       :input-blocking nil)
          input-field   (make-instance 'crt:field
                                       :position (list 0 0)
                                       :width (crt:width main-screen)
                                       :window input-window
                                       :style (list :foreground nil :background nil)
                                       :keymap 'girc-input-map))
    ;; format the status line
    (setf (crt:background status-window) (make-instance 'crt:complex-char :simple-char #\- :attributes '(:reverse)))
    (refresh ui)))

(defun display (template &rest args)
  "Display the format template and the args in the output window."
  (let ((wout (output-window *ui*)))
    (apply #'format wout template args)
    (crt:refresh wout)))

(defun echo (&rest args)
  "Join the args to a string, then display the line in the output window.

The args are separated by the #\space character.

A line ending is automatically added before output.

Calling echo with no arguments just outputs the newline.

The argument strings can not contain format control characters.

The formating should happen before the strings are passed to echo, 
or the display function can be used which allows format controls."
  (let ((wout (output-window *ui*)))
    (format wout "~{~A~^ ~}~%" args)
    (crt:refresh wout)))

(defmethod refresh ((ui user-interface) &rest args)
  (with-slots (main-screen input-window status-window output-window input-field) ui
    (crt:refresh main-screen)
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

;; display a line as a placeholder for the real status line
;; TODO 200519 add a function to set the status line.
;; WORK 201001
;; triggered by: connect
;; called from: connect / make-instance / after
(defun set-status (nick host)
  "Set the status line of the current connection."
  (with-accessors ((swin status-window)) *ui*
    (crt:clear swin)
    (when (and nick host)
      (crt:move swin 0 1)
      (format swin "[~A]" nick)
      (crt:move swin 0 20)
      (format swin "[~A]" host))
    (crt:refresh swin)))
