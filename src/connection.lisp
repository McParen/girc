(in-package :de.anvi.girc)

(defparameter *current-connection* nil)

(defclass connection ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      connection-name
    :type          (or null string)
    :documentation "Name of the server to which the connection is established.")

   (hostname
    :initarg       :hostname
    :initform      nil
    :accessor      connection-hostname
    :type          (or null string)
    :documentation "Hostname of the IRC server to which the connection is established.")

   (port
    :initarg       :port
    :initform      6667
    :accessor      connection-port
    :type          integer
    :documentation "Port to which the server connection is established.")

   (socket
    :initform      nil
    :accessor      connection-socket
    ;;:type          (or null usocket:socket??)
    :documentation "Connected socket.")

   ;; SB-SYS:FD-STREAM
   (stream
    :initform      nil
    :accessor      connection-stream
    :type          (or null stream)
    :documentation "Stream associated with the connection socket.")

   (nickname
    :initarg       :nickname
    :initform      nil
    :accessor      connection-nickname
    :type          (or null string)
    :documentation "Nickname of the user to be registered with the connected server.")

   (username
    :initarg       :username
    :initform      "myuser"
    :accessor      connection-username
    :type          (or null string)
    :documentation "Username of the user to be registered with the connected server.")

   (realname
    :initarg       :realname
    :initform      "Realname"
    :accessor      connection-realname
    :type          (or null string)
    :documentation "Realname of the user to be registered with the connected server."))

  (:documentation "Parameters necessary to establish a connection to an IRC server."))

(defmethod initialize-instance :after ((connection connection) &key)
  "Initialize the window and field objects that are part of the user interface."
  (with-slots (stream hostname port nickname username realname) connection
    (setf socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8))
          stream (usocket:socket-stream socket))
    (register connection nickname 0 username realname)))

(defun write-irc-line (rawmsg stream)
  "Write rawmsg to the stream followed by the line ending CRLF \r\n (13 10)."
  (write-sequence (sb-ext:string-to-octets rawmsg) stream)
  (write-byte 13 stream)
  (write-byte 10 stream)
  (force-output stream))

(defun make-raw-message (command params text)
  "Assemble a valid raw IRC protocol message without the CRLF line ending.

Params is a list of string parameters.

The proper CRLF line ending is added before it is sent."
  (format nil "~A~{ ~A~}~@[ :~A~]" command params text))

#|

The command should not be nil or an empty string.

CL-USER> (make-raw-message 'a '("x" "y" "z") 'b)
"A x y z :B"
CL-USER> (make-raw-message 'a '() 'b)
"A :B"
CL-USER> (make-raw-message 'a '("x" "y" "z") nil)
"A x y z"
CL-USER> (make-raw-message 'a nil nil)
"A"

|#

(defun send-irc-message (connection command params text)
  "Assemble an irc message, then send it as a string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((rawmsg (make-raw-message command params text))
        (stream (connection-stream connection)))
    (write-irc-line rawmsg stream)))

(defun send (command &optional params text)
  "Make and then send an IRC message to the current connection."
  (send-irc-message *current-connection* command params text))

;; TODO: check that the string is max 512 bytes long including CRLF before sending.
;; Example: (send-raw-message stream "USER ~A ~A * :~A" username mode realname)
(defun send-raw-message (connection rawmsg-template &rest args)
  "Send an irc message string to the connection.

A proper CRLF \r\n ending is added to the message before it is sent.

If there are additional args, ircmsg has to be a template accepting
the proper number of format-style control strings.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((stream (connection-stream connection))
        (rawmsg (apply #'format nil rawmsg-template args)))
    (write-irc-line rawmsg stream)))

(defun send-raw (rawmsg-template &rest args)
  "Send a raw IRC message to the current connection."
  (apply #'send-raw-message *current-connection* rawmsg-template args))

(defun read-raw-message (connection)
  "Read a single IRC message from the server terminated with CRLF or EOF.

Return the message without the trailing CRLF or :EOF if end of file is reached.

The max allowed length of a single message is 512 bytes.

If the read length including CRLF exceeds that limit, nil is returned."
  (let ((buf (make-array 512 :fill-pointer 0 :element-type '(unsigned-byte 8)))
        (cr-flag nil))
    (loop for ch = (read-byte (connection-stream connection) nil :eof) do
      (cond ((and (not (eq ch :eof))
                  (/= ch 13)  ; CR
                  (/= ch 10)) ; LF
             (when cr-flag (setq cr-flag nil))
             (unless (vector-push ch buf)
               ;; nil is returned when the 512-byte buffer is full without a proper CRLF ending.
               (return nil)))
            ;; if a CR is read, set the flag to t
            ;; check whether a received CR is a start of CRLF.
            ((= ch 13)
             (setq cr-flag t))
            ;; if a LF is read immediately after a CR, return the buffer as a string.
            ;; when we get a LF and the previous char was CR, we have a proper irc message ending.
            ((and (= ch 10) cr-flag)
             (return (sb-ext:octets-to-string buf)))
            ;; connection is ended.
            ((eq ch :eof)
             (return :eof))))))

(defun handle-server-input (field event)
  "Handle the nil event during a non-blocking edit of the input field.

Bound to nil in girc-input-map."
  ;; do not process if a connection has not been established first.
  (when *current-connection*
    ;; do not read a single message then sleep then read the next.
    ;; read as many messages as we can until listen returns nil.
    ;; do not set a frame-rate for the nil event here, because that uses sleep, which slows down typing.
    ;; set the input-blocking delay instead because that does not affect the input rate.
    (loop while (listen (connection-stream *current-connection*))
          do (let ((rawmsg (read-raw-message *current-connection*))) ; see connection.lisp
               (if rawmsg
                   (if (eq rawmsg :eof)
                       (echo "Server connection lost (encountered End Of File)")
                       ;; after anything is written to the output window, return the cursor to the input window.
                       (crt:save-excursion (input-window *ui*)
                         ;; message handline writes to the screen, so it has to happen in the main thread
                         (handle-message rawmsg *current-connection*))) ; see event.lisp
                   (echo "Not a valid IRC message (missing CRLF ending)")))))
  ;; if the current buffer has been changed, update the display.
  (when (buffer-changed-p *current-buffer*)
    (crt:save-excursion (input-window *ui*)
      (display-buffer *current-buffer*))))
