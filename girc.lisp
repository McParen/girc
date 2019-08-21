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
(defun read-irc-line (stream)
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

;; TODO: use edit-field instead of writing our own edit commands.
;; TODO: 190102 only switch to fields when we have implemented horizontal scrolling in fields.
;; this is old code from before we had form editing in croatoan.
(defun ui ()
  (crt:with-screen (scr :input-echoing nil :cursor-visible t :enable-colors nil)
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

           ;; TODO: dont do automatically, turn into a user command.
           (st (connect "chat.freenode.net" 6667))
           ;;(st (connect "irc.efnet.org" 6667))
           ;;(st (connect "irc.de.ircnet.net" 6667))

           ;; no of chars in the input line.
           (n 0)
           
           ;; line length for lisp pretty printing functions
           (*print-right-margin* (crt:width scr)))

      ;; TODO: do not connect and register automatically upon starting the client, make this an user command.
      (register st "haom" 0 "myuser" "Realname")

      ;; TODO: dont use event-case, convert to bind + run-event-loop.
      (crt:event-case (win event)
        (:left
         (when (> (cadr (crt:cursor-position win)) 0)
           (crt:move-direction win :left)))
        (:right 
         (when (< (cadr (crt:cursor-position win)) n)
           (crt:move-direction win :right)))
        (#\newline
         (let ((*standard-output* wout))
           (when (> n 0)
             
             ;; TODO: use input buffer instead of reading from ncurses window.
             ;; see how input buffers are implemented in croatoan forms.
             (let* ((strin (crt:extract-wide-string win :n n :y 0 :x 0)))

               ;; for now, hitting enter just sends the line as a raw irc message
               ;; TODO: add command line parsing and evaluation here.
               (send st strin)

               (setf n 0)
               (crt:clear win)
               (crt:add-string wout (format nil "=> ~A~%" strin))
               (crt:refresh wout)))))
        (:dc
         (when (> n (cadr (crt:cursor-position win)))
           (decf n)
           (crt:delete-char win)))
        (:backspace
         (when (> (cadr (crt:cursor-position win)) 0)
           (decf n)
           (crt:move-direction win :left)
           (crt:delete-char win)))

        ;; send a quit message to the irc server, cleanly disconnect
        ;; TODO: remove this later
        (#\R (quit st))

        ;; quit the client, go back to the lisp repl
        ;; TODO: remove this later
        (#\Q (return-from crt:event-case))

        ;; we cant make this input-blocking nil instead of event (nil), because
        ;; we have to check for input during the nil event.
        ;; a much better way way would be to use separate threads for ui events and
        ;; network events.
        ((nil)
         ;; when input-blocking = nil, prevent high CPU load.
         (sleep 0.01)
         ;; return t if there is a char available on the stream
         (when (listen st)
           ;; TODO: server connection should hapen in a completely separate thread, not in the user input event loop.
           ;; the thread should write server output to a shared buffer, and every time the buffer is updated,
           ;; the output window should be redisplayed.
           (let ((line (read-irc-line st)))
             ;; TODO: should we quit main loop when the connection is at the EOF?
             (when (and line (eq line :eof) (return-from crt:event-case)))
             (when line
               ;; save excursion prevents that the cursor jumps between windows.
               ;; it should always stay in the input window.
               (crt:save-excursion win
                 ;;(princ line wout)
                 ;;(princ (parse line) wout)
                 ;; TODO: add a logger here.
                 (handle-message line wout st))) ;; --> event.lisp
             (crt:refresh wout))))

        ;; non-function keys, i.e. normal character keys
        (otherwise
         (when (and (characterp event)
                    ;; dont allow the input line to be longer than the screen width.
                    (< (cadr (crt:cursor-position win)) (1- (crt:width win))))
           (incf n)
           ;; TODO: use add instead of add-wide-char.
           (crt:add-wide-char win event))))

      (close win)
      (close wout))))
