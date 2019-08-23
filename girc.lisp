(in-package :de.anvi.girc)

;; TODO: this should be done during the initialization of the connection object
;; TODO: defclass 'connection
(defun connect (hostname port)
  "Take a hostname (string) or IP and a port (integer), connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect hostname port))
         (stream (usocket:socket-stream socket)))
    ;; return the stream of the created client socket
    stream))

;; TODO: check that the string is max 512 bytes long.
(defun send (stream ircmsg &rest args)
  "Send an irc message string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

If there are additional args, ircmsg has to be a template accepting
the proper number of format-style control strings.

The allowed max length of the irc message including CRLF is 512 bytes."
  (apply #'format stream
         ;; then append it to the template before passing it to format.
         (concatenate 'string ircmsg
                      ;; create a string out of \r and \n, crlf.
                      (coerce '(#\return #\linefeed) 'string))
         args)
  (force-output stream))

(defun nick (stream nickname)
  "Give the user a new nickname during registration or change the existing one."
  (send stream "NICK ~A" nickname))

(defun user (stream username mode realname)
  "Specify the username, mode and realname of a new user when registering a connection."
  ;; "USER ~A 0 0 :~A"
  (send stream "USER ~A ~A * :~A" username mode realname))

(defun register (stream nickname mode username realname)
  "Register a connection to an irc server with a nickname and a username.

This is the first command that should be sent after a connection is established.

Upon success, the server will reply with a 001 RPL_WELCOME message."
  (nick stream nickname)
  (user stream username mode realname))

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (stream &optional (quit-message "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send stream "QUIT :~A" quit-message))

;; read-byte from stream
;; utf8-to-unicode byte list to character
;; char list to string

;; graphic-char-p, dann geht aber dcc ^A nicht.
;; TODO: do not read lisp characters, read byte by byte (octet by octet), then interpret them as ANSI (latin1) or ASCII/UTF-8.
;; TODO: read chars byte by byte to a list first, then convert to a string.
(defun read-irc-message (stream)
  (let ((inbuf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)) ;; empty string ""
        (ch-prev nil))
    (loop
       ;; this will not work with utf-8 encoded chars. (why not?)
       ;; TODO: we can not read lisp "chars", we have to read octets and put them together to chars.
       ;; we have to use something like read-byte instead of read-char
       (let ((ch (read-char-no-hang stream nil :eof)))
         (when ch
           (progn
             (when (eq ch :eof)
               (return :eof))
             ;; normal char, neither CR nor LF.
             (when (and (char/= ch #\return) (char/= ch #\linefeed))
               (vector-push-extend ch inbuf)
               (setq ch-prev nil))
             ;; 510 is the max number of bytes one irc message can contain.
             ;; we here count the number of characters. problem.
             ;; we also should check whether after 510 bytes we have crlf. only then we have a valid irc message,
             ;; we should not simply return inbuf after every 510 chars, whether it is a valid irc message or not.
             (when (>= (length inbuf) 510)
               (return inbuf)
               ;;(setf (fill-pointer inbuf) 0)
               )
             ;; check whether a received CR is a start of CRLF.
             (when (char= ch #\return)
               (setq ch-prev t))
             ;; when we get a LF and the previous char was CR, we have a proper irc message ending.
             (when (and (char= ch #\linefeed) ch-prev)
               (return inbuf)
               ;;(setf (fill-pointer inbuf) 0)
               )))))))

(defun make-ui (scr)
  (let* ((wout (make-instance 'crt:window
                              :height (1- (crt:height scr))
                              :width (crt:width scr)
                              :location '(0 0)
                              :enable-scrolling t))
         (win  (make-instance 'crt:window
                              :height 1
                              :width (crt:width scr)
                              :location (list (1- (crt:height scr)) 0)
                              :enable-function-keys t
                              ;; note that when this is nil, we plan to perform work during the nil event.
                              :input-blocking nil))
         (field (make-instance 'crt:field :location (list 0 0) :width (crt:width scr) :window win
                               :style (list :foreground nil :background nil))))
    (setf *print-right-margin* (crt:width scr))
    (list win wout field)))

;; Handler of the nil event during a non-blocking edit of the field.
;; TODO: check whether win is non-blocking before assuming it
;; this should run in a separate thread
(defun process-server-input (field event &rest args)
  (destructuring-bind (stream win wout) args
    (sleep 0.01)
    (when (listen stream)
      (let ((ircmsg (read-irc-message stream)))
        (when ircmsg
          (crt:save-excursion win
            (handle-irc-message ircmsg wout stream) ;; see event.lisp
            (crt:refresh wout)))))))

;; use this instead of accept-field so we dont have to return from edit.
(defun process-user-input (field event &rest args)
  "When the field is used without a parent form, accepting the field edit returns the field value."
  (with-accessors ((inbuf crt::buffer) (inptr crt::input-pointer) (dptr crt::display-pointer) (win crt:window)) field
    (when (> (length inbuf) 0)
      (let ((val (crt:value field)))
        (destructuring-bind (stream win wout) args
          (crt:clear win)
          (setf inbuf nil inptr 0 dptr 0)
          (send stream val)
          (crt:save-excursion win
            (format wout "=> ~A~%" val)
            (crt:refresh wout)))))))

(defun run (&optional (nickname "haom") (hostname "chat.freenode.net") (port 6667))
  (crt:with-screen (scr :input-echoing nil :cursor-visible t :enable-colors nil)
    (destructuring-bind (win wout field) (make-ui scr)
      (let ((stream (connect hostname port)))
        (register stream nickname 0 "myuser" "Realname")

        (crt:bind field nil 'process-server-input)
        (crt:bind field #\newline 'process-user-input)

        ;; C-a will exit the event loop.
        (crt:edit field stream win wout))

      (close win)
      (close wout))))

