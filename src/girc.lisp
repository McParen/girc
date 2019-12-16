(in-package :de.anvi.girc)

;; TODO: this should be done during the initialization of the connection object
;; TODO: defclass 'connection, then this should be called make-connection
(defun connect (hostname port)
  "Take a hostname (string) or IP and a port (integer), connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect hostname port))
         (stream (usocket:socket-stream socket)))
    ;; return the stream of the created client socket
    stream))

;; TODO: check that the string is max 512 bytes long including CRLF.
;; Example: (send stream "USER ~A ~A * :~A" username mode realname)
(defun send (connection ircmsg &rest args)
  "Send an irc message string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

If there are additional args, ircmsg has to be a template accepting
the proper number of format-style control strings.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((stream (connection-stream connection)))
    (apply #'format stream
           ;; then append it to the template before passing it to format.
           (concatenate 'string ircmsg
                        ;; create a string out of \r and \n, crlf.
                        (coerce '(#\return #\linefeed) 'string))
           args)
    (force-output stream)))

(defun make-irc-message (command params text)
  "Assemble a valid IRC protocol message without the CRLF line ending.

Params is a list of string parameters.

The proper CRLF line ending is added by send-irc-message."
    (format nil "~A~{ ~A~}~@[ :~A~]" command params text))

#|

The command should not be nil or an empty string.

CL-USER> (make-irc-message 'a '("x" "y" "z") 'b)
"A x y z :B"
CL-USER> (make-irc-message 'a '() 'b)
"A :B"
CL-USER> (make-irc-message 'a '("x" "y" "z") nil)
"A x y z"

|#

(defun send-irc-message (connection command params text)
  "Assemble an irc message, then send it as a string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((ircmsg (make-irc-message command params text))
        (stream (connection-stream connection)))
    ;; create a string out of \r and \n, CRLF.
    (write-string (concatenate 'string ircmsg (coerce '(#\return #\linefeed) 'string)) stream)
    (force-output stream)))

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

;; Handler of the nil event during a non-blocking edit of the field.
;; TODO: check whether win is non-blocking before assuming it
;; this should run in a separate thread
(defun process-server-input (field event &rest args)
  (destructuring-bind (ui con) args
    (with-accessors ((win input-window) (wout output-window)) ui
      (with-accessors ((stream connection-stream)) con
        (sleep 0.01)
        (when (listen stream)
          ;; TODO: this should happen in a worker thread
          ;; for every connected server.
          (let ((ircmsg (read-irc-message stream)))
            (when ircmsg
              (crt:save-excursion win
                ;; message handline writes to the screen, so it has to happen in the main thread
                (handle-irc-message ircmsg ui con) ;; see event.lisp
                (crt:refresh wout)))))))))

;; TODO: split this into:
;;   parse-user-input
;;   handle-user-input

;; TODO 191103: processing user input should work when the client is not connected
;; therefore we have to remove (send stream val) from here.
;; messages should only be sent when a command like say, msg or notice is used.
;; that means send should not be here, but in the handlers of user commands.

(defun process-user-input (field event &rest args)
  "When the field is used without a parent form, accepting the field edit returns the field value."
  (let ((val (crt:value field)))
    ;; only process when the input field is not empty
    (when val
      (destructuring-bind (ui con) args
        (with-accessors ((win input-window) (wout output-window)) ui
          (with-accessors ((stream connection-stream)) con
            (apply #'crt:reset-field field event args)
            (send con val)
            ;; the cursor should not leave the input field
            (crt:save-excursion win
              (format wout "=> ~A~%" val)
              (crt:refresh wout))))))))

;; after quickloading girc, start the client with (girc:run).
(defun run (&optional (nickname "haom") (hostname "chat.freenode.net"))
  (let ((ui (make-instance 'interface))
        (con (make-instance 'connection :nickname nickname :hostname hostname)))
        
    ;; instead of processed during a nil event, this should be moved to a worker thread.
    ;; as soon as we connect to a server, pass the stream to a background thread
    ;; process-server-input then should just take read messages from the thread queue
    (crt:bind (input-field ui) nil 'process-server-input)

    ;; TODO 191216: do not bind within the run definition.
    ;; bind them on the top level and use a keymap.
    ;; then associate the keymap with the input field.
        
    ;; the user input line is processed on every ENTER press.
    (crt:bind (input-field ui) #\newline 'process-user-input)

    ;; C-w = 23 = #\etb (End of Transmission Block)
    ;; sends a quit message to the server. (replied by the server with an error message)
    (crt:bind (input-field ui) #\etb (lambda (f e &rest a) (quit con)))

    ;; C-a will exit the event loop.
    ;; TODO 191103: problem: c-a = #\soh = accept-field only works when the field is not empty!
    ;; stream win and wout are passed to every routine as: &rest args

    ;; in order to be able to process several server connections,
    ;; instead of a single stream, we have to pass a list of streams here.
    (crt:edit (input-field ui)
              ;; these two are the "args" passed to run-event-loop.
              ui con)
    ;; when we accept the field with c-a, edit returns then the client exits.

    (finalize-interface ui)))
