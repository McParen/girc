(in-package :de.anvi.girc)

(defclass buffer ()
  ((connection
    :initarg       :connection
    :initform      nil
    :accessor      connection
    :type          (or null connection)
    :documentation "Pointer to the connection object associated with the buffer. Set by /connect.")

   (target
    :initarg       :target
    :initform      nil
    :accessor      target
    :type          (or null string)
    :documentation "Target channel of the messages sent and received form the connection.")

   (changedp
    :initform      nil
    :accessor      changedp
    :type          boolean
    :documentation "Flag to denote that the buffer was changed and should be redisplayed by update-output.")

   (max-length
    :initform      100
    :accessor      max-length
    :type          integer
    :documentation "Number of lines to store in the display buffer.")

   ;; should this be a list, a queue or an array
   ;; a list should be the easiest to implement, but maybe too inefficient if we have a very long buffer
   ;; a list should be ok if the length is 50 lines.
   (lines
    :initform      nil
    :accessor      lines
    :type          (or null cons)
    :documentation "List of strings the buffer contains."))

  (:documentation "A buffer is a list of strings to be displayed to the output window."))

(defparameter *buffers* (make-instance 'crt:collection :items (list (make-instance 'buffer)))
  "Collection of buffers.")

(defun current-buffer ()
  (crt:current-item *buffers*))

(defun current-buffer-number ()
  (crt:current-item-number *buffers*))

(defun remove-buffer ()
  (crt:remove-item *buffers*))

(defun append-buffer (buffer)
  (crt:append-item *buffers* buffer))

(defun add-buffer (&optional connection-name target)
  (crt:append-item *buffers* (make-instance 'girc:buffer :connection (find-connection connection-name) :target target)))

(defun select-previous-buffer ()
  (with-accessors ((buf crt:current-item)) *buffers*
    (crt:select-previous-item *buffers*)
    (setf (changedp buf) t) ;; flag current buffer for redisplay
    (update)))

(defun select-next-buffer ()
  (with-accessors ((buf crt:current-item)) *buffers*
    (crt:select-next-item *buffers*)
    (setf (changedp buf) t)
    (update)))

(defun select-last-buffer ()
  (with-accessors ((buf crt:current-item)) *buffers*
    (crt:select-last-item *buffers*)
    (setf (changedp buf) t) ;; flag current buffer for redisplay
    (update)))

(defun push-to-buffer (string buffer)
  "Push a new line to the output buffer, to be displayed by display-buffer.

If buffer is t, the current buffer is used."
  (let ((buffer (if (eq buffer t) (current-buffer) buffer)))
    (with-accessors ((lines lines) (changedp changedp) (max max-length)) buffer
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
  (find-buffer nil (connection msg)))

(defun find-buffer (target connection)
  "Return the buffer associated with the connection and the target (channel or query).

When there is no connection with that target, return the connection buffer (target nil)."
  (if target
      ;; try to find a buffer where both connection and target match
      (let ((buf1 (loop for buf in (crt:items *buffers*)
                        when (and (eq connection (connection buf))
                                  (equal target (target buf)))
                          return buf)))
        (if buf1
            buf1
            ;; if buf for the target was not found, return the buffer for target nil
            (loop for buf in (crt:items *buffers*)
                  when (and (eq connection (connection buf))
                            (eq (target buf) nil))
                    return buf)))

      ;; if target is nil,
      ;; find the first buffer where only the connection matches
      (loop for buf in (crt:items *buffers*)
            when (eq connection (connection buf))
              return buf)))

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
  (with-accessors ((lines lines) (changedp changedp)) buffer
    (let* ((win (output-window *ui*))
           (h (crt:height win))
           (w (crt:width win))
           ;; lists of strings of max width w to be displayed in the window
           (scrlines ())
           ;; number of lines to display
           n)
      (dolist (line lines)
        (let ((ln (remove-if #'crt:control-char-p line)))
          (dolist (items (if (> (length ln) w)
                             ;; if a buffer line is wider than the window,
                             ;; split it into mutiple lines
                             (reverse (crt:split-lines (crt:wrap-string ln w)))
                             (list ln)))
            ;; lines are displayed in the reversed order, oldest first, newest last
            (push items scrlines))))
      (crt:clear win)
      (crt:move win 0 0)
      (if (>= (length scrlines) h)
          (setq n h)
          (setq n (length scrlines)))
      (dolist (ln (last scrlines n))
        (fresh-line win)
        (princ ln win))
      ;; causes flicker
      (crt:refresh win))
    ;; after the changes have been displayed, set the flag to nil
    (setf changedp nil)))

(defun update-topic ()
  "Display the current buffer channel topic, if available."
  (let ((win (input-window *ui*))
        (wtp (topic-window *ui*)))
    (crt:clear wtp)
    (when (and (target (current-buffer))
               (connection (current-buffer))
               (connectedp (connection (current-buffer))))
      (let* ((chan (find (target (current-buffer))
                         (channels (connection (current-buffer)))
                         :key #'name :test #'string=))
             (text (when chan (topic chan))))
        (when text
          (if (> (length text) (crt:width wtp))
              (crt:add-string wtp (subseq text 0 (crt:width wtp)))
              (crt:add-string wtp text)))))))

(defun update-output ()
  "If the current buffer has been changed, update the output window."
  (when (changedp (current-buffer))
    (display-buffer (current-buffer))))

;; called from: connect (command), error (event)
(defun update-status ()
  "Set the status line of the current buffer."
  (with-accessors ((swin status-window)) *ui*
    (with-accessors ((nick nickname) (name name)) (connection (current-buffer))
      (crt:clear swin)
      (crt:move swin 0 2)
      (format swin
              (with-output-to-string (str)
                ;; the accessors dont work without a connection
                (if (connection (current-buffer))
                    (when (and nick name)
                      (format str "[~A ()] [~A" nick name))
                    ;; default placeholders
                    (format str "[~A ()] [~A" :nick :server))
                ;; if the current target is a channel, add it after the network
                (if (target (current-buffer))
                    (format str "/~A ()]" (target (current-buffer)))
                    (format str "]"))))
      ;; add the current buffer number at the end of the line
      (crt:move swin 0 (- (crt:width swin) 6))
      (format swin "~5@A" (format nil "[~A]" (current-buffer-number))))))

(defun update-buffers ()
  "If there is a buffer list window, update its contents."
  (with-accessors ((win buffers-window)) *ui*
    (when win
      (crt:clear win)
      ;;(loop for i from 0 to (1- (length (crt:items *buffers*))) do
      (dotimes (i (length (crt:items *buffers*)))
        (crt:move win i 0)
        ;; if the buffer has a target, write the target,
        ;; otherwise the connection name, otherwise nil
        (if (target (nth i (crt:items *buffers*)))
            (format win " ~A" (target (nth i (crt:items *buffers*))))
            (if (connection (nth i (crt:items *buffers*)))
                (format win "~A" (name (connection (nth i (crt:items *buffers*)))))
                (format win "nil")))
        ;; highlight the current buffer
        (when (= i (current-buffer-number))
          (crt:move win i 0)
          (crt:change-attributes win 13 '(:reverse)))))))

(defun update ()
  (crt:save-excursion (input-window *ui*)
    (update-topic)
    (update-output)
    (update-status)
    (update-buffers))
  (refresh *ui*))

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
