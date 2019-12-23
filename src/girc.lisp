(in-package :de.anvi.girc)

(defparameter *user-commands* nil)

(defparameter *current-connection* nil)

(defparameter *ui* nil)

;; TODO: this should be done during the initialization of the connection object
;; TODO: defclass 'connection, then this should be called make-connection
(defun connect (hostname port)
  "Connect to the IRC server given by a hostname (string) or IP and a port (integer), return a server stream."
  (let* ((socket (usocket:socket-connect hostname port))
         (stream (usocket:socket-stream socket)))
    ;; return the stream of the created client socket
    stream))

;; TODO: check that the string is max 512 bytes long including CRLF.
;; Example: (send-raw-message stream "USER ~A ~A * :~A" username mode realname)
(defun send-raw-message (connection raw-msg-template &rest args)
  "Send an irc message string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

If there are additional args, ircmsg has to be a template accepting
the proper number of format-style control strings.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((stream (connection-stream connection)))
    (apply #'format stream
           ;; then append it to the template before passing it to format.
           (concatenate 'string raw-msg-template
                        ;; create a string out of \r and \n, crlf.
                        (coerce '(#\return #\linefeed) 'string))
           args)
    (force-output stream)))

(defun send-raw (raw-msg-template &rest args)
  "Send a raw IRC message to the current connection."
  (apply #'send-raw-message *current-connection* raw-msg-template args))

(defun make-raw-message (command params text)
  "Assemble a valid raw IRC protocol message without the CRLF line ending.

Params is a list of string parameters.

The proper CRLF line ending is added by send-irc-message."
    (format nil "~A~{ ~A~}~@[ :~A~]" command params text))

#|

The command should not be nil or an empty string.

CL-USER> (make-raw-message 'a '("x" "y" "z") 'b)
"A x y z :B"
CL-USER> (make-raw-message 'a '() 'b)
"A :B"
CL-USER> (make-raw-message 'a '("x" "y" "z") nil)
"A x y z"

|#

(defun send-irc-message (connection command params text)
  "Assemble an irc message, then send it as a string to the stream.

A proper CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the irc message including CRLF is 512 bytes."
  (let ((rawmsg (make-raw-message command params text))
        (stream (connection-stream connection)))
    ;; create a string out of \r and \n, CRLF.
    (write-string (concatenate 'string rawmsg (coerce '(#\return #\linefeed) 'string)) stream)
    (force-output stream)))

(defun send (command params text)
  "Make and then send an IRC message to the current connection."
  (send-irc-message *current-connection* command params text))

;; read-byte from stream
;; utf8-to-unicode byte list to character
;; char list to string

;; graphic-char-p, dann geht aber dcc ^A nicht.
;; TODO: do not read lisp characters, read byte by byte (octet by octet), then interpret them as ANSI (latin1) or ASCII/UTF-8.
;; TODO: read chars byte by byte to a list first, then convert to a string.

(defun read-raw-message (connection)
  (let ((stream (connection-stream connection))
        (inbuf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)) ;; empty string ""
        (ch-prev nil))
    ;; when there is something to read from the server, read until we complete a message.
    (when (listen stream)
      (loop
         ;; this will not work with utf-8 encoded chars. (why not?)
         ;; TODO: we can not read lisp "chars", we have to read octets and put them together to chars.
         ;; we have to use something like read-byte instead of read-char
         (let ((ch (read-char-no-hang stream nil :eof)))
           ;; if read returns a nil, read-irc-message returns a nil as a whole.
           (when ch
             ;; connection is ended.
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
               (return inbuf))
             ;; check whether a received CR is a start of CRLF.
             (when (char= ch #\return)
               (setq ch-prev t))
             ;; when we get a LF and the previous char was CR, we have a proper irc message ending.
             (when (and (char= ch #\linefeed) ch-prev)
               (return inbuf))))))))

;; Handler of the nil event during a non-blocking edit of the field.
;; TODO: check whether win is non-blocking before assuming it
(defun process-server-input (field event)
  ;; do not process if a connection has not been established first.
  (when *current-connection*
    (let ((raw-message (read-raw-message *current-connection*)))
      (if raw-message
          ;; after anything is written to the output window, return the cursor to the input window.
          (crt:save-excursion (input-window *ui*)
            ;; message handline writes to the screen, so it has to happen in the main thread
            (handle-message raw-message *current-connection*)) ; see event.lisp
          (sleep 0.01)))))

(defun handle-user-command (field event &rest args1)
  (let ((input-string (crt:value field)))
    (when input-string
      (apply #'crt:reset-field field event args1)
      (destructuring-bind (cmd . args) (parse-user-input input-string)
        ;; TODO: what to do when there is no command in the input?
        (when cmd
          (let ((fun (get-command cmd)))
            (if fun
                (funcall fun cmd args)
                (funcall (get-command t) cmd args))))))))

(defun display (template &rest args)
  "Display the format template on the output window."
  (let ((wout (output-window *ui*)))
    (apply #'format wout template args)
    (crt:refresh wout)))

;; passed to the field during initialization
(crt:define-keymap 'girc-input-map
  (list

   ;; C-a = ^A = #\soh = 1 = start of heading
   #\soh      'crt::accept

   ;; C-x = cancel = CAN = #\can
   #\can      'crt::cancel

   ;; C-r = reset = DC2 = #\dc2
   #\dc2      'crt::reset-field

   :left      'crt::move-previous-char
   :right     'crt::move-next-char

   :backspace 'crt::delete-previous-char
   :dc        'crt::delete-next-char
   
   :ic        (lambda (field event &rest args)
                (setf (crt:insert-mode-p (crt:window field))
                      (not (crt:insert-mode-p (crt:window field)))))

   t          'crt::field-add-char

   nil        'process-server-input
   #\newline  'handle-user-command
   
   ;; C-w = 23 = #\etb = End of Transmission Block
   #\etb      (lambda (field event &rest args)
                (quit *current-connection*))))

;; after quickloading girc, start the client with (girc:run).
(defun run ()
  (setq *ui* (make-instance 'user-interface))
#|
  ;; instead of processed during a nil event, this should be moved to a worker thread.
  ;; as soon as we connect to a server, pass the stream to a background thread
  ;; process-server-input then should just take read messages from the thread queue
  (crt:bind (input-field ui) nil 'process-server-input)

  ;; TODO 191216: do not bind within the run definition.
  ;; bind them on the top level and use a keymap.
  ;; then associate the keymap with the input field.
        
  ;; the user input line is processed on every ENTER press.
  (crt:bind (input-field ui) #\newline 'handle-user-command)

  ;; C-w = 23 = #\etb (End of Transmission Block)
  ;; sends a quit message to the server. (replied by the server with an error message)
  (crt:bind (input-field ui) #\etb (lambda (f e &rest a) (quit *current-connection*)))

  ;; C-a = #\soh = accept will exit the event loop.
|#
  (crt:edit (input-field *ui*))
  
  (finalize-user-interface *ui*))
