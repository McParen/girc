(in-package :de.anvi.girc)

(defclass buffer ()
  ((connection
    :initarg       :connection
    :initform      nil
    :accessor      buffer-connection
    :type          (or null connection)
    :documentation "Pointer to the connection object associated with the buffer. Set by /connect.")

   (target
    :initarg       :target
    :initform      nil
    :accessor      buffer-target
    :type          (or null string)
    :documentation "Target channel of the messages sent and received form the connection.")

   (changedp
    :initform      nil
    :accessor      buffer-changed-p
    :type          boolean
    :documentation "Flag to denote that the buffer was changed an can be redisplayed.")

   (max-length
    :initform      100
    :accessor      buffer-max-length
    :type          integer
    :documentation "Number of lines to store in the display buffer.")

   ;; should this be a list, a queue or an array
   ;; a list should be the easiest to implement, but maybe too inefficient if we have a very long buffer
   ;; a list should be ok if the length is 50 lines.
   (lines
    :initform      nil
    :accessor      buffer-lines
    :type          (or null cons)
    :documentation "List of strings the buffer contains."))

  (:documentation "A buffer is a list of strings to be displayed to the output window."))

(defparameter *current-buffer* (make-instance 'buffer)
  "Pointer to the current selected buffer.")

(defparameter *current-buffer-number* 0)

(defparameter *buffers* (list *current-buffer*)
  "List of buffers.")

(defun select-next-buffer (field event)
  "Set the next buffer in the buffer list as the currently displayed buffer."
  (setf *current-buffer-number* (mod (1+ *current-buffer-number*) (length *buffers*))
        *current-buffer* (nth *current-buffer-number* (reverse *buffers*))
        ;; flag the next buffer for display
        (buffer-changed-p *current-buffer*) t)
  (update-status))

(defun push-to-buffer (string buffer)
  "Push a new line to the output buffer, to be displayed by display-buffer.

If buffer is t, the current buffer is used."
  (let ((buffer (if (eq buffer t) *current-buffer* buffer)))
    (with-accessors ((lines buffer-lines) (changedp buffer-changed-p) (max buffer-max-length)) buffer
      (push string lines)
      (when (> (length lines) max)
        (setf lines (subseq lines 0 max)))
      ;; flag the buffer for redisplay
      (setf changedp t))))

(defgeneric buffer (obj)
  (:documentation "Return the buffer associated with the object (connection, message)."))

(defmethod buffer ((msg irc-message))
  "Loop through the list of buffers, return the buffer associated with the connection of the message.

First try to return a buffer without a specified target, i.e. the main buffer for the connection."
  (find-buffer (connection msg) nil))

(defun find-buffer (connection target)
  "Check if there is a buffer associated with a connection and a target channel.

When there is no connection with that target, return the buffer for the connection."
  (let ((buf1 (loop for buf in *buffers*
                    when (and (eq connection (buffer-connection buf))
                              (equal target (buffer-target buf)))
                      return buf)))
    (if buf1
        buf1
        ;; if buf for the target was not found, return the buffer for target nil
        (loop for buf in *buffers*
              when (and (eq connection (buffer-connection buf))
                        (eq (buffer-target buf) nil))
                return buf))))

(defun display (buffer template &rest args)
  "Display the format template and the args in the buffer.

If buffer is t, the current buffer is used."
  (push-to-buffer (apply #'format nil template args) buffer))

(defun echo (buffer &rest args)
  "Join the args to a string, then add the line to the buffer.

If buffer is t, the current buffer is used.

The args are separated by the #\space character.

A line ending is automatically added before output.

Calling echo with no arguments just outputs the newline.

The argument strings can not contain format control characters.

The formating should happen before the strings are passed to echo,
or the display function can be used which allows format controls."
  (push-to-buffer (format nil "~{~A~^ ~}" args) buffer))

(defun display-buffer (buffer)
  "Display at most height lines to the output window."
  (with-accessors ((lines buffer-lines) (changedp buffer-changed-p)) buffer
    (let* ((win (output-window *ui*))
           (h (crt:height win))
           (w (crt:width win))
           ;; lists of strings of max width w to be displayed in the window
           (scrlines ())
           ;; number of lines to display
           (n 0))
      (dolist (ln lines)
        (dolist (item (if (> (length ln) w)
                          ;; if a buffer line is wider than the window,
                          ;; split it into mutiple lines
                          (reverse (crt:split-lines (crt:wrap-string ln w)))
                          (list ln)))
          ;; lines are displayed in the reversed order, oldest first, newest last
          (push item scrlines)
          (incf n)
          (when (= n h) (go end)))
        end)
      (crt:clear win)
      (crt:move win 0 0)
      (dolist (ln scrlines)
        (fresh-line win)
        (princ ln win))
      ;; causes flicker
      (crt:refresh win))
    ;; after the changes have been displayed, set the flag to nil
    (setf changedp nil)))

;; called from: connect (command), error (event)
(defun update-status ()
  "Set the status line of the current buffer."
  (with-accessors ((swin status-window)) *ui*
    (with-accessors ((nick connection-nickname) (name connection-name)) (buffer-connection *current-buffer*)
      ;; put the cursor back to the input window after updating the status window.
      (crt:save-excursion (input-window *ui*)
        (crt:clear swin)
        (crt:move swin 0 2)
        (format swin
                (with-output-to-string (str)
                  ;; the accessors dont work without a connection
                  (if (buffer-connection *current-buffer*)
                      (when (and nick name)
                        (format str "[~A ()] [~A" nick name))
                      ;; default placeholders
                      (format str "[~A ()] [~A" :nick :network))
                  ;; if the current target is a channel, add it after the network
                  (if (buffer-target *current-buffer*)
                      (format str "/~A ()]" (buffer-target *current-buffer*))
                      (format str "]"))))
        ;; add the current buffer number at the end of the line
        (crt:move swin 0 (- (crt:width swin) 6))
        (format swin "~5@A" (format nil "[~A]" *current-buffer-number*))

        ;; after we refresh the window, return the cursor to the input line, use save-excursion in echo?
        (crt:refresh swin)))))

;; unused
(defun lastn (n list)
  "If list is longer than n, return the last n elements, else return the list."
  (let ((len (length list)))
    (if (< len n)
        list
        (subseq list (- len n)))))

;; unused
(defun firstn (n list)
  "If list is longer than n, return the first n elements, else return the list."
  (let ((len (length list)))
    (if (< len n)
        list
        (subseq list 0 n))))
