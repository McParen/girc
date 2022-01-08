(in-package :de.anvi.girc)

(defparameter *connections* nil)

(defclass connection ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      connection-name
    :type          (or null string)
    :documentation "Name by which to refer to the connection, for example the name of the IRC network.")

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
    :documentation "Realname of the user to be registered with the connected server.")

   (connectedp
    :initform      nil
    :accessor      connectedp
    :type          boolean
    :documentation "Flag to denote that the server is connected."))

  (:documentation "Parameters necessary to establish a connection to an IRC server."))

(defun connect (connection)
  "Connect a socket to the host of the connection and register a nickname."
  (with-slots (socket stream hostname port nickname username realname connectedp) connection
    (setf socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8))
          stream (usocket:socket-stream socket)
          connectedp t)
    (register connection nickname 0 username realname)))

(defun disconnect (connection)
  "Cleanly close the socket of the connection."
  (with-slots (socket stream connectedp) connection
    (usocket:socket-close socket)
    (setf socket nil
          stream nil
          connectedp nil)))

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

(defun send-raw-message (connection rawmsg)
  "Send a valid raw irc message string to the connection.

A proper CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((connection (if (eq connection t)
                        (buffer-connection *current-buffer*)
                        connection)))
    (if connection
        (if (connectedp connection)
            (write-irc-line rawmsg (connection-stream connection))
            (display t "-!- ~A not connected." (connection-name connection)))
        (display t "-!- Current buffer not associated with a connection."))))

(defun send (connection command &optional params text)
  "Assemble an irc message, then send it as a string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the irc message including CRLF is 512 bytes.

If connection is t, send to the current connection."
  (send-raw-message connection (make-raw-message command params text)))

(defun send-raw (connection rawmsg-template &rest args)
  "Send a raw irc message to the connection.

If there are additional args, rawmsg has to be a template accepting
the proper number of format-style control strings.

Example: send-raw stream \"USER ~A ~A * :~A\" username mode realname"
  (apply #'send-raw-message connection rawmsg-template args))

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

;; we need the class definition of connection before we can specialize on it.
;; (buffer con) is only used here in handle-server-input
(defmethod buffer ((con connection))
  "Loop through the list of buffers, return the buffer associated with the connection."
  (loop for buf in *buffers* do
    (when (eq con (buffer-connection buf))
      (return buf))))

(defun handle-server-input ()
  "Handle the nil event during a non-blocking edit of the input field.

Bound to nil in girc-input-map."
  ;; do not process if a connection has not been established first.
  (when *connections*
    (loop for con in *connections* do
      ;; do not read a single message then sleep then read the next.
      ;; read as many messages as we can until listen returns nil.
      ;; do not set a frame-rate for the nil event here, because that uses sleep, which slows down typing.
      ;; set the input-blocking delay instead because that does not affect the input rate.
      (when (connectedp con)
        (loop while (listen (connection-stream con)) do
          (let ((rawmsg (read-raw-message con))) ; see connection.lisp
            (if rawmsg
                (if (eq rawmsg :eof)
                    (echo (buffer con) "Server connection lost (End Of File)")
                    ;; after anything is written to the output window, return the cursor to the input window.
                    (crt:save-excursion (input-window *ui*)
                      ;; message handline writes to the screen, so it has to happen in the main thread
                      (handle-message rawmsg con))) ; see event.lisp
                (echo (buffer con) "Not a valid IRC message (missing CRLF ending)")))))))
  ;; if the current buffer has been changed, update the display.
  (when (buffer-changed-p *current-buffer*)
    (crt:save-excursion (input-window *ui*)
      (display-buffer *current-buffer*))))

(defun find-connection (name)
  "Return the connection object associated with the given connection name."
  (find name *connections* :key #'connection-name :test #'string=))
