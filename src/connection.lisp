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
    :initform      nil
    :accessor      port
    :type          (or null integer)
    :documentation
    "Port to which the server connection is established.

For a plaintext connection the default port is 6667, if SSL encryption
is enabled, the port 6697 is used by default.")

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
    :initform      conf:nickname
    :accessor      nickname
    :type          (or null string)
    :documentation "Nickname of the user to be registered with the connected server.")

   (username
    :initarg       :username
    :initform      conf:username
    :accessor      username
    :type          (or null string)
    :documentation "Username of the user to be registered with the connected server.")

   (realname
    :initarg       :realname
    :initform      conf:realname
    :accessor      realname
    :type          (or null string)
    :documentation "Realname of the user to be registered with the connected server.")

   (server-password
    :initarg       :server-password
    :initform      nil
    :accessor      server-password
    :type          (or null string)
    :documentation
    "Connection password used to connect to the server.

Generally, this is different than the NickServ password. Some networks (notably
Libera) forward the connection password to NickServ identify if it is given in
the form nickname:password.

This client will send the NickServ login credentials as the server password if
the login-method is set to :pass.")

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
    :initform      nil
    :accessor      login-method
    :type          (or null keyword)
    :documentation
    "Keyword describing the method to perform the login to NickServ.

The following login methods are available:

nil    The client will not login to NickServ automatically
:sasl  The login is performed by the SASL protocol if supported by the server
:pass  The login credentials are passed as server-password during registration")

   (connectedp
    :initform      nil
    :accessor      connectedp
    :type          boolean
    :documentation "Flag to denote that the server is connected.")

   (rpl-list-channels
    :initform      nil
    :type          (or null cons)
    :documentation
    "List of channels visible by the server, replies to the LIST command.

It is populated by rpl-list (322).

Every channel is a list of three elements: (name number-of-users topic).")

   (rpl-list-end-p
    :initform      nil
    :type          boolean
    :documentation
    "Boolean flag to mark the end of a sequence of rpl-list (322) events.

If it is t, it is set to nil (default) by rpl-liststart (321, optional)
or rpl-list (322). rpl-list-channels is set to nil before the next channel
is added, which means that a new reply sequence to LIST has been started.

It is set to t by rpl-listend (323).")

   (autojoin
    :initarg       :autojoin
    :initform      nil
    :type          (or null string)
    :documentation
    "Comma-separated list of channel names to join upon a successful connection.

The channels are joined after rpl-statsconn, the last reply after a successful
login, is received.")

   (channels
    :initform      nil
    :accessor      channels
    :type          (or null cons)
    :documentation "List of joined channel objects."))

  (:documentation "Parameters necessary to establish a connection to an IRC server."))

(defmethod initialize-instance :after ((obj connection) &key nickserv)
  (with-slots (nickserv-account nickserv-password login-method server-password sslp port) obj
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
      (setf server-password (format nil "~A:~A" nickserv-account nickserv-password)))
    ;; set the default port depending on the encryption
    (setf port (if sslp 6697 6667))))

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
  (with-slots (socket stream hostname port connectedp sslp nickserv-account nickserv-password login-method) connection
    (setf socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8))
          stream (if sslp
                     ;; to enable ssl, we wrap the simple stream into an encrypted ssl stream.
                     (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                                    :hostname hostname
                                                    :verify t)
                     (usocket:socket-stream socket))
          connectedp t)
    (when (and nickserv-account
               nickserv-password
               (eq login-method :sasl))
      ;; To enable SASL, an IRCv3 capability negotiation has to be initialized first by CAP LS.
      ;; It has to be finalized with CAP END, which is sent by the rpl-saslsuccess (903) handler.
      (irc:cap connection "LS"))
    ;; registration (nick, user)
    (irc:register connection)))

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
            (echo t "-!- Server not connected:" (name connection)))
        (echo t "-!- Buffer not associated with a server."))))

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
             ;; if we get utf-8 decoding errors (invalid-utf8-continuation-byte),
             ;, try to convert the byte sequence to latin-1.
             (return (handler-case (sb-ext:octets-to-string buf :external-format :utf-8)
                       (sb-int:character-decoding-error (condition) (sb-ext:octets-to-string buf :external-format :latin-1)))))
            ;; connection is ended.
            ((eq ch :eof)
             (return :eof))))))

;; we need the class definition of connection before we can specialize on it.
;; (buffer con) is only used here in handle-server-input
(defmethod buffer ((con connection))
  "Loop through the list of buffers, return the buffer associated with the connection."
  (loop for buf in (crt:children *buffers*) do
    (when (eq con (connection buf))
      (return buf))))

;; this is called 10 times per second.
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
                (progn
                  (echo (buffer con) "-!- Not a valid IRC message (missing CRLF ending)")))
            ;; update the output after every message so event handlers dont have to do it manually.
            (redraw)))))))

(defun add-connection (name host &rest args &key &allow-other-keys)
  "Make and add a server object to the connection list.

Required arguments are a short name by which to refer to the server
and the hostname of the server."
  (let ((conn (apply #'make-instance 'connection :name name :hostname host args)))
    (push conn *connections*)))

(defun find-connection (name)
  "Return the connection object associated with the given connection name."
  (find name *connections* :key #'name :test #'string=))

(defun add-channel (name connection)
  "Take a channel name, make and add a channel object to the connection."
  (push (make-instance 'channel :name name)
        (channels connection)))

(defun remove-channel (name connection)
  (with-slots (channels) connection
    (setf channels
          (remove name channels :test #'string= :key #'name))))

(defun find-channel (name connection)
  (find name (channels connection) :key #'name :test #'string=))

(defun add-nick (nick channel)
  "Add a nick to the channel nicklist."
  (if (char= (char nick 0) #\@)
      ;; remove the @ before pushing the nick to the list.
      (push (subseq nick 1) (nicknames channel))
      (push nick (nicknames channel))))

(defun remove-nick (nick channel)
  (with-slots (nicknames) channel
    (setf nicknames
          (remove nick nicknames :test #'string=))))
