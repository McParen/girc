(in-package :de.anvi.girc)

(defun connect (hostname port)
  "Take a hostname (string) or IP and a port (integer), connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect hostname port))
         (stream (usocket:socket-stream socket)))
    ;; return the stream of the created client socket
    stream))

(defun send (stream message-template &rest args)
  "Make an IRC message from the template and args, then write it to the stream.

An IRC CRLF \r\n ending is added to the message before it is sent.

The allowed max length of the message including CRLF is 512 bytes."
  (apply #'format stream
         ;; then append it to the template before passing it to format.
         (concatenate 'string message-template
                      ;; create a string out of \r and \n, crlf.
                      (coerce '(#\return #\linefeed) 'string))
         args)
  (force-output stream))

;; TODO: pass nick user and text as keywords instead of only using nickname
(defun login (stream nickname)
  "Login to an IRC server with a nickname and a username.

This is the first command that should be sent after a connection is established.

If the login is successful, the server should reply with a 001 message."
  (send stream "NICK ~A" nickname)
  (send stream "USER ~A 0 0 :~A" nickname nickname))

;; TODO: rewrite send to work like format:
;; (send stream "USER ~A 0 0 :~A" nick nick)
;; instead of
;; (send stream (format nil "USER ~A 0 0 :~A" nickname nickname)))

;; TODO: hide the irc protocol behind gray streams:
;; (format stream "USER ~A 0 0 :~A" nickname nickname)
;; should automatically add \r and \n at the end of each message.

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (stream &optional (quit-msg "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send stream "QUIT :~A" quit-msg))

;; graphic-char-p, dann geht aber dcc nicht.
;; TODO: do not read characters, read byte by byte, then interpret them as ASCII, ANSI (latin1) or UTF-8.
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

(defun ui ()
  (with-screen (scr :input-echoing nil :cursor-visibility t :enable-colors nil)
    (let* ((wout (make-instance 'window
                                :height (1- (.height scr))
                                :width (.width scr)
                                :position '(0 0)
                                :enable-scrolling t))
           (win  (make-instance 'window
                                :height 1
                                :width (.width scr)
                                :position (list (1- (.height scr)) 0)
                                :enable-fkeys t
                                ;; note that when this is nil, we plan to perform work during the nil event.
                                :input-blocking nil))
           (st (connect "185.30.166.37" 6667))
           ;; no of chars in the input line.
           (n 0)
           ;; line length for lisp pretty printing functions
           (*print-right-margin* (.width scr)))

      (login st "haom")
      
      (event-case (win event)
        (:left
         (when (> (cadr (.cursor-position win)) 0)
           (move-to win :left)))
        (:right 
         (when (< (cadr (.cursor-position win)) n)
           (move-to win :right)))
        (#\newline
         (let ((*standard-output* wout))
           (when (> n 0)
             (let* ((strin (extract-wide-string win :n n :y 0 :x 0)))
               ;; for now, hitting enter just sends the line as a raw irc message
               ;; TODO: add command line parsing and evaluation here.
               (send st strin)
               (setf n 0)
               (clear win)
               (add-string wout (format nil "=> ~A~%" strin))
               (refresh wout)))))
        (:dc
         (when (> n (cadr (.cursor-position win)))
           (decf n)
           (delete-char win)))
        (:backspace
         (when (> (cadr (.cursor-position win)) 0)
           (decf n)
           (move-to win :left)
           (delete-char win)))

        ;; send a quit message to the irc server, cleanly disconnect
        ;; TODO: remove this later
        (#\R (quit st))

        ;; quit the client, go back to the lisp repl
        ;; TODO: remove this later
        (#\Q (return-from event-case))

        ;; we cant make this input-blocking nil instead of event (nil), because
        ;; we have to check for input during the nil event.
        ;; a much better way way would be to use separate threads for ui events and
        ;; network events.
        ((nil)
         (sleep 0.01)
         ;; return t if there is a char available
         (when (listen st)
           (let ((line (read-irc-line st)))
             ;; TODO: should we quit main loop when the connection is at the EOF?
             (when (and line (eq line :eof) (return-from event-case)))
             (when line
               ;; save excursion prevents that the cursor jumps between windows.
               ;; it should always stay in the input window.
               (save-excursion win
                 ;;(princ line wout)
                 ;;(princ (parse line) wout)
                 ;; TODO: add a logger here.
                 (evaluate-message line wout st)))
             (refresh wout))))

        ;; non-function keys, i.e. normal character keys
        (otherwise
         (when (and (characterp event)
                    (< (cadr (.cursor-position win)) (1- (.width win))))
           (incf n)
           (add-wide-char win event))))

      (close win)
      (close wout))))
