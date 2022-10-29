(in-package :de.anvi.girc)

(defparameter *connections* nil)

(defclass connection ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      name
    :type          (or null string)
    :documentation "Unique name by which to refer to the connection, for example the network.")

   (hostname
    :initarg       :hostname
    :initform      nil
    :accessor      hostname
    :type          (or null string)
    :documentation "Hostname of the IRC server to which the connection is established.")

   (port
    :initarg       :port
    :initform      6667
    :accessor      port
    :type          integer
    :documentation "Port to which the server connection is established.")

   ;; Requires the foreign library libssl.so has to be available on the system.
   (sslp
    :initarg       :ssl
    :initform      nil
    :accessor      sslp
    :type          boolean
    :documentation
    "Set to t to enable a secure SSL/TLS connection to the server.

When SSL is enabled, the user mode Z is set, and the numeric reply 671
:rpl-whoissecure is returned by whois:

<nick> is using a secure connection [TLSv1.3, TLS_AES_256_GCM_SHA384]")

   (socket
    :initform      nil
    :accessor      socket
    ;;:type          (or null usocket:socket??)
    :documentation "Connected socket.")

   ;; SB-SYS:FD-STREAM
   (stream
    :initform      nil
    :accessor      stream
    :type          (or null cl:stream)
    :documentation "Stream associated with the connection socket.")

   (nickname
    :initarg       :nickname
    :initform      nil
    :accessor      nickname
    :type          (or null string)
    :documentation "Nickname of the user to be registered with the connected server.")

   (username
    :initarg       :username
    :initform      "myuser"
    :accessor      username
    :type          (or null string)
    :documentation "Username of the user to be registered with the connected server.")

   (realname
    :initarg       :realname
    :initform      "Realname"
    :accessor      realname
    :type          (or null string)
    :documentation "Realname of the user to be registered with the connected server.")

   (server-password
    :initarg       :server-password
    :initform      nil
    :accessor      server-password
    :type          (or null string)
    :documentation "Connection password used to connect to the server.

Generally, this is different than the NickServ password. Some networks (notably
Libera) forward the connection password to NickServ identify if it is given in
the form nickname:password.

This client will send the NickServ login credentials as the server password if
the login-method is set to :pass, which is currently the default.")

   (nickserv-account
    :initarg       :nickserv-account
    :initform      nil
    :accessor      nickserv-account
    :type          (or null string)
    :documentation "")

   (nickserv-password
    :initarg       :nickserv-password
    :initform      nil
    :accessor      nickserv-password
    :type          (or null string)
    :documentation "")

   (login-method
    :initarg       :login-method
    :initform      :pass
    :accessor      login-method
    :type          (or null keyword)
    :documentation
    "Keyword describing the method to perform the login to NickServ.

The following login methods are available:

nil    The client will not login to NickServ automatically
:pass  The login credentials are passed as server-password during registration")

;; :msg   The login is performed as /msg nickserv identify nickname password
;; :sasl  The login is performed by the SASL protocol if supported by the server"

   (connectedp
    :initform      nil
    :accessor      connectedp
    :type          boolean
    :documentation "Flag to denote that the server is connected.")

   (channels
    :initform      nil
    :accessor      channels
    :type          (or null cons)
    :documentation "List of joined channels."))

  (:documentation "Parameters necessary to establish a connection to an IRC server."))

(defmethod initialize-instance :after ((obj connection) &key nickserv)
  (with-slots (nickserv-account nickserv-password login-method server-password) obj
    ;; if the nickserv login is given in the form account:password,
    ;; parse the account and the passwort
    (when (and nickserv
               (null nickserv-account)
               (null nickserv-password))
      (destructuring-bind (acct pass)
          (split-sequence:split-sequence #\: nickserv)
        (setf nickserv-account acct
              nickserv-password pass)))
    ;; if the login-method is :pass, make account:password the server password
    (when (and (null server-password)
               nickserv-account
               nickserv-password
               (eq login-method :pass))
      (setf server-password (format nil "~A:~A" nickserv-account nickserv-password)))))

(defclass channel ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      name
    :type          (or null string)
    :documentation "Name of the channel.")

   (nicknames
    :initform      nil
    :accessor      nicknames
    :type          (or null cons)
    :documentation "List of nicknames in a channel.")

   (topic
    :initform      nil
    :accessor      topic
    :type          (or null string)
    :documentation "Topic of the channel.")

   (rpl-namreply-started-p
    :initform      nil
    :type          boolean
    :documentation
    "Boolean flag to mark the start of a sequence of rpl-namreply (353) events.

It is set to t by the first rpl-namreply event in a sequence and is set to nil
by rpl-endofnames (366)."))

  (:documentation ""))

(defun connect (connection)
  "Connect a socket to the host of the connection and register a nickname."
  (with-slots (socket stream hostname port connectedp sslp) connection
    (setf socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8))
          stream (if sslp
                     ;; to enable ssl, we wrap the simple stream into an encrypted ssl stream.
                     (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                                    :hostname hostname
                                                    :verify t)
                     (usocket:socket-stream socket))
          connectedp t)
    (register connection)))

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
                        (connection (current-buffer))
                        connection)))
    (if connection
        (if (connectedp connection)
            (write-irc-line rawmsg (stream connection))
            (display t "-!- ~A not connected." (name connection)))
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
    (loop for ch = (read-byte (stream connection) nil :eof) do
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
  (loop for buf in (crt:items *buffers*) do
    (when (eq con (connection buf))
      (return buf))))

(defun handle-server-input ()
  "Handle the nil event during a non-blocking edit of the input field.

Bound to nil in girc-input-map."
  ;; do not process if a connection has not been established first.
  (when *connections*
    ;;(loop for con in *connections* do
    (dolist (con *connections*)
      ;; do not read a single message then sleep then read the next.
      ;; read as many messages as we can until listen returns nil.
      ;; do not set a frame-rate for the nil event here, because that uses sleep, which slows down typing.
      ;; set the input-blocking delay instead because that does not affect the input rate.
      (when (connectedp con)
        (loop while (listen (stream con)) do
          (let ((rawmsg (read-raw-message con))) ; see connection.lisp
            (if rawmsg
                (if (eq rawmsg :eof)
                    (echo (buffer con) "-!- Server connection lost (End Of File)")
                    ;; after anything is written to the output window, return the cursor to the input window.
                    (crt:save-excursion (input-window *ui*)
                      ;; message handline writes to the screen, so it has to happen in the main thread
                      (handle-message rawmsg con))) ; see event.lisp
                (echo (buffer con) "-!- Not a valid IRC message (missing CRLF ending)"))))))
    (update-output)))

(defun find-connection (name)
  "Return the connection object associated with the given connection name."
  (find name *connections* :key #'name :test #'string=))
