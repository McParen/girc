(in-package :de.anvi.girc)

(defclass buffer ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      buffer-name
    :type          (or null string)
    :documentation "Name of the buffer.")

   (connection
    :initarg       :connection
    :initform      nil
    :accessor      buffer-connection
    ;;:type          (or null string)
    :documentation "Pointer to the connection object.")

   (changedp
    :initform      nil
    :accessor      buffer-changed-p
    :type          boolean
    :documentation "Flag to denote that the buffer was changed an can be redisplayed.")
   
   ;; should this be a list, a queue or an array
   ;; a list should be the easiest to implement, but maybe too inefficient if we have a very long buffer
   ;; a list should be ok if the length is 50 lines.
   (lines
    :initform      nil
    :accessor      buffer-lines
    :type          (or null cons)
    :documentation "List of strings the buffer contains."))

  (:documentation "A buffer is a list of strings to be displayed to the output window."))

;; list of buffer objects
;; use find and the buffer name to get an object
(defparameter *buffers* nil)

;; pointer to the current selected buffer
(defparameter *current-buffer* (make-instance 'buffer))

(defun push-to-buffer (string buffer)
  "Push a new line to a buffer."
  (with-accessors ((lines buffer-lines) (changedp buffer-changed-p)) buffer
    (let* ((height (crt:height (output-window *ui*)))
           (len (length lines)))
      (push string lines)
      ;; if buffer is longer than some limit delete old lines
      ;; currently the limit is the max window height
      ;; we dont yet account for lines that are longer than one window line
      (when (> len height)
        (setf lines (subseq lines 0 height)))
      ;; flag the buffer for redisplay
      (setf changedp t))))

(defun display (template &rest args)
  "Display the format template and the args in the output window."
  (push-to-buffer (apply #'format nil template args)
                  *current-buffer*))

(defun echo (&rest args)
  "Join the args to a string, then display the line in the output window.

The args are separated by the #\space character.

A line ending is automatically added before output.

Calling echo with no arguments just outputs the newline.

The argument strings can not contain format control characters.

The formating should happen before the strings are passed to echo, 
or the display function can be used which allows format controls."
  (push-to-buffer (format nil "~{~A~^ ~}~%" args)
                  *current-buffer*))

(defun display-buffer (buffer)
  "Display at most height lines to the output window."
  (with-accessors ((lines buffer-lines) (changedp buffer-changed-p)) buffer
    (let ((win (output-window *ui*))
          (len (length lines)))
      (crt:clear win)
      (crt:move win 0 0)
      (loop for i from 0
            for line in (reverse lines) do
              (crt:move win i 0)
              (princ line win))
      ;; causes flicker
      (crt:refresh win))
    ;; after the changes have been displayed, set the flag to nil
    (setf changedp nil)))

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
