(in-package :de.anvi.girc)

(defclass buffer (crt:node)
  ((changedp
    :initform      nil
    :accessor      changedp
    :type          boolean
    :documentation "Flag to denote that the buffer was changed and should be redisplayed by update-output.")

   ;; needed for the cursor
   (currentp
    :initform      nil
    :type          boolean
    :accessor      currentp
    :documentation "Flag denoting whether the buffer is currently selected in the buffer tree.")

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

  (:documentation "A buffer contains a list of strings to be displayed to the output window."))

(defclass connection-buffer (buffer)
  ((connection
    :initarg       :connection
    :initform      nil
    :type          (or null connection)
    :documentation "Pointer to the connection object associated with the buffer. Set by /connect."))

  (:documentation ""))

(defclass target-buffer (buffer)
  ((target
    :initarg       :target
    :initform      nil
    :accessor      target
    :type          (or null string)
    :documentation "Target channel or nickname (for a query) for the messages sent to and received form the connection."))

  (:documentation ""))

(defgeneric connection (buffer)
  (:documentation ""))

(defmethod connection ((obj buffer))
  nil)

(defmethod connection ((obj connection-buffer))
  (slot-value obj 'connection))

(defmethod connection ((obj target-buffer))
  (slot-value (crt:parent obj) 'connection))

(defgeneric (setf connection) (con buffer)
  (:documentation ""))

(defmethod (setf connection) (con (obj connection-buffer))
  (setf (slot-value obj 'connection) con))

(defmethod (setf connection) (con (obj target-buffer))
  (setf (slot-value (crt:parent obj) 'connection) con))

(defparameter *buffers* (make-instance 'buffer)
  "Main client buffer not associated with a server, can't be killed.

Its children are server buffers and their children are channel and
query buffers.")

(defstruct (cursor (:conc-name nil)
                   (:constructor %make-cursor))
  node)

(defun make-cursor (node)
  ;; select the root node when the cursor is initialized.
  (setf (currentp node) t)
  (%make-cursor :node node))

(defparameter *buffer-tree-cursor* (make-cursor *buffers*))

(defun current-buffer ()
  (node *buffer-tree-cursor*))

#|
 dfs pre-order traversal:
 - if there are next siblings, go to next
 - if we are at the last sibling, go to previous
 - if there are no previous, go to parent
|#
(defun next (cursor)
  (with-accessors ((node node)) cursor
    (with-accessors ((children crt:children)) node
      (if children
          (setf (currentp node) nil
                node (car children)
                (currentp node) t)
          (when (crt:parent node)
            (let ((n (position node (crt:children (crt:parent node)))))
              (if (and n (> (length (crt:children (crt:parent node))) (1+ n)))
                  (setf (currentp node) nil
                        node (nth (1+ n) (crt:children (crt:parent node)))
                        (currentp node) t)
                  (when (crt:parent node)
                    (labels ((step-up ()
                               (when (crt:parent node)
                                 (setf node (crt:parent node))
                                 (when (crt:parent node)
                                   (let ((n (position node (crt:children (crt:parent node)))))
                                     (if (and n (> (length (crt:children (crt:parent node))) (1+ n)))
                                         (setf node (nth (1+ n) (crt:children (crt:parent node))))
                                         (step-up)))))))
                      (setf (currentp node) nil)
                      (step-up)
                      (setf (currentp node) t))))))))))
#|
 dfs pre-order backwards:
 - if there is a previos sibling, go to previous
 - if the previous sibling has children, go to last child
 - if there are no previous, go to parent
|#
(defun prev (cursor)
  (with-accessors ((node node)) cursor
    (with-accessors ((children crt:children)) node
      (if (crt:parent node)
          (let ((n (position node (crt:children (crt:parent node)))))
            (if (and n (> n 0))
                (progn
                  (setf (currentp node) nil)
                  (setf node (nth (1- n) (crt:children (crt:parent node))))
                  (when (crt:children node)
                    (setf node (nth (1- (length (crt:children node))) (crt:children node))))
                  (setf (currentp node) t))
                (progn
                  (setf (currentp node) nil)
                  (setf node (crt:parent node))
                  (setf (currentp node) t))))
          ;; if there is no parent, were at the root node
          ;; then go to the last child of the last child
          (when (crt:children node)
            (setf (currentp node) nil)
            (setf node (nth (1- (length (crt:children node))) (crt:children node)))
            (when (crt:children node)
              (setf node (nth (1- (length (crt:children node))) (crt:children node))))
            (setf (currentp node) t))))))

(defun remv (cursor)
  "Remove a buffer with all children."
  (with-accessors ((node node)) cursor
    (with-accessors ((children crt:children)) node
      (when (crt:parent node)
        (let ((n (position node (crt:children (crt:parent node)))))
          (if (and n (> (length (crt:children (crt:parent node))) (1+ n)))
              ;; if there are next siblings, go to next, then remove previous
              (progn
                (setf (currentp node) nil)
                (setf node (nth (1+ n) (crt:children (crt:parent node))))
                (setf (currentp node) t)
                (setf (crt:children (crt:parent node))
                      (delete (nth n (crt:children (crt:parent node)))
                              (crt:children (crt:parent node)))))
              ;; check if we have previous sibling or we are at the last child
              (if (> (length (crt:children (crt:parent node))) 1)
                  ;; if there is a previous sibling, go to last child of previous sibling, then remove last.
                  (progn
                    (setf (currentp node) nil)
                    ;; go to previous sibling
                    (setf node (nth (1- n) (crt:children (crt:parent node))))
                    ;; remove nth
                    (setf (crt:children (crt:parent node))
                          (delete (nth n (crt:children (crt:parent node)))
                                  (crt:children (crt:parent node))))
                    ;; then go to last child of previous sibling
                    (when (crt:children node)
                      (setf node (nth (1- (length (crt:children node))) (crt:children node))))
                    (setf (currentp node) t))
                  ;; if there is no previous sibling, delete the last which means set children to nil.
                  (progn
                    (setf (currentp node) nil)
                    (setf node (crt:parent node))
                    (setf (currentp node) t)
                    (setf (crt:children node) nil)))))))))

(defun find-buffer (connection-name &optional target)
  "Check if a server or target buffer exists and return it.

If the correct buffer is not found, return nil."
  (when (crt:children *buffers*)
    (dolist (i (crt:children *buffers*))
      ;; a server buffer can only exist if a server connection
      (when (string-equal connection-name (name (connection i)))
        (if target
            ;; if a target is given, look for its buffer.
            (dolist (j (crt:children i))
              (when (string-equal target (target j))
                (return-from find-buffer j)))
            ;; if a target was not given, return a server buffer.
            (return-from find-buffer i))))))

(defun get-buffer (connection-name target)
  "Return the most appropriate buffer for a message.

Return a target buffer if it exists, otherwise return its parent
server buffer.

This function ensures that some buffer will be returned in which
the message can be displayed."
  (if (find-buffer connection-name target)
      (find-buffer connection-name target)
      (find-buffer connection-name)))

(defun add-server-buffer (&optional server-name)
  "Take an existing server name, add a new server buffer."
  (if server-name
      ;; server name given, check if the server exists
      (let ((con (find-connection server-name)))
        (if con
            ;; server found
            ;; check if server buffer exists
            (if (find-buffer server-name)
                (echo t "-!- Buffer already exists:" server-name)
                ;; Add new server buffer
                (progn
                  (push (make-instance 'connection-buffer
                                       :connection con
                                       :parent *buffers*)
                        (crt:children *buffers*))
                  (setf (crt:children *buffers*) (nreverse (crt:children *buffers*)))))
            ;; server not found.
            (echo t "-!- Server unknown:" server-name)))
      (echo t "-!- Server not given.")))

(defun add-target-buffer (server-name &optional target)
  "Take a server name and a channel, add a new channel buffer."
  (if (and target
           (find-buffer server-name target))
      ;; warn that buffer exists
      (echo t "-!- Buffer already exists:" server-name target)
      ;; if the buffer doesnt already exist.
      (let ((buf (find-buffer server-name)))
        (if buf
            ;; parent buffer exists
            (if target
                (progn
                  (push (make-instance 'target-buffer :target target :parent buf) (crt:children buf))
                  (setf (crt:children buf) (nreverse (crt:children buf))))
                ;; add unnamed target buf
                (progn
                  (push (make-instance 'target-buffer :parent buf) (crt:children buf))
                  (setf (crt:children buf) (nreverse (crt:children buf)))))
            ;; warn that requested parent doesnt exist
            (echo t "-!- Server buffer doesn't exist:" server-name)))))

(defun remove-buffer ()
  (remv *buffer-tree-cursor*)
  (setf (changedp (node *buffer-tree-cursor*)) t)
  (update))

(defun select-previous-buffer ()
  (prev *buffer-tree-cursor*)
  (setf (changedp (node *buffer-tree-cursor*)) t)
  (update))

(defun select-next-buffer ()
  (next *buffer-tree-cursor*)
  (setf (changedp (node *buffer-tree-cursor*)) t)
  (update))

(defun select-buffer (connection-name &optional target)
  (let ((buf (find-buffer connection-name target)))
    (when buf
      (with-accessors ((node node)) *buffer-tree-cursor*
        (setf (currentp node) nil
              node buf
              (currentp node) t)
        (setf (changedp node) t)
        (update)))))

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
  (find-buffer (name (connection msg))))

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
  (let ((wtp (topic-window *ui*)))
    (when wtp
      (crt:clear wtp)
      (when (and (typep (current-buffer) 'target-buffer)
                 (target (current-buffer))
                 (connection (current-buffer))
                 (connectedp (connection (current-buffer))))
        (let* ((chan (find (target (current-buffer))
                           (channels (connection (current-buffer)))
                           :key #'name :test #'string=))
               (text (when chan (topic chan))))
          (when text
            (if (> (length text) (crt:width wtp))
                (crt:add-string wtp (subseq text 0 (crt:width wtp)))
                (crt:add-string wtp text))))))))

(defun update-output ()
  "If the current buffer has been changed, update the output window."
  (when (changedp (current-buffer))
    (display-buffer (current-buffer))))

;; called from: connect (command), error (event)
(defun update-status ()
  "Set the status line of the current buffer."
  (with-accessors ((swin status-window)) *ui*
    (crt:clear swin)
    (crt:move swin 0 2)
    (if (or (typep (current-buffer) 'target-buffer)
            (typep (current-buffer) 'connection-buffer))
        (with-accessors ((nick nickname) (name name)) (connection (current-buffer))
          (format swin
                  (with-output-to-string (str)
                    ;; the accessors dont work without a connection
                    (if (connection (current-buffer))
                        (when (and nick name)
                          (format str "[~A ()] [~A" nick name))
                        ;; default placeholders
                        (format str "[~A ()] [~A" :nick :server))
                    ;; if the current target is a channel, add it after the network
                    (if (and (typep (current-buffer) 'target-buffer)
                             (target (current-buffer)))
                        (format str "/~A ()]" (target (current-buffer)))
                        (format str "]")))))
        (format swin "[main]"))))

;; add the current buffer number at the end of the line
;;(crt:move swin 0 (- (crt:width swin) 6))
;;(format swin "~5@A" (format nil "[~A]" (current-buffer-number)))

(defun update-buffers ()
  "If there is a buffer list window, update its contents."
  (with-accessors ((win buffers-window)) *ui*
    (when win
      (crt:clear win)
      (labels ((show (buf)
                 (typecase buf
                   (target-buffer
                    (let ((text (format nil "  ~A" (target buf))))
                      (if (> (length text) 12)
                          (format win (crt:text-ellipsize text 12 :truncate-string (format nil "~C" (code-char #x2026))))
                          (format win text))))
                   (connection-buffer
                    (if (connection buf)
                        (format win " ~A" (name (connection buf)))
                        (format win " nil")))
                   (buffer
                    (format win "~A" "main")))
                 ;; highlight the current buffer
                 (setf (crt:cursor-position-x win) 0)
                 (when (currentp buf)
                   (setf (crt:cursor-position-x win) 0)
                   (crt:change-attributes win 13 '(:reverse)))
                 (incf (crt:cursor-position-y win))
                 ;; recursively print any children
                 (when (crt:children buf)
                   (dolist (i (crt:children buf))
                     (show i)))))
        (show *buffers*)))))

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
