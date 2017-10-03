(in-package :de.anvi.girc)

(defun connect (server port)
  "Take a hostname or IP and a port, connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect server port))
         (stream (usocket:socket-stream socket)))
    ;; return the stream of the created client socket
    stream))

;; todo: check that the string is max 512 bytes long.
(defun raw (stream irc-msg-string)
  "Send a string containing a valid IRC message to an IRC server."
  (format stream "~A~C~C" irc-msg-string #\return #\linefeed)
  (force-output stream))

(defun login (stream nickname)
  "Login to an IRC server with a nick name and a user name.

This is the first command that should be sent after a connection is established.

If the login is successful, the server should reply with a 001 message."
  (raw stream (format nil "NICK ~A" nickname))
  (raw stream (format nil "USER ~A 0 0 :~A" nickname nickname)))

;; TODO: rewrite raw to work like format:
;; (raw stream "USER ~A 0 0 :~A" nick nick)
;; instead of
;; (raw stream (format nil "USER ~A 0 0 :~A" nickname nickname)))

;; TODO: hide the irc protocol behind gray streams:
;; (format stream "USER ~A 0 0 :~A" nickname nickname)
;; should automatically add \r and \n at the end of each message.

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (stream &optional (quit-msg "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (raw stream (format nil "QUIT :~A" quit-msg)))

;; graphic-char-p, dann geht aber dcc nicht.
(defun read-irc-line (stream)
  (let ((inbuf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)) ;; empty string ""
        (ch-prev nil))
    (loop
       ;; this will not work with utf-8 encoded chars.
       ;; TODO: we can not reach lisp "chars", we have to read octets and put them together to chars.
       ;; we have to use something like read-byte instead of read-char
       (let ((ch (read-char-no-hang stream nil :eof)))
         (when ch
           (progn
             (when (eq ch :eof)
               (return :eof))
             (when (and (char/= ch #\return) (char/= ch #\linefeed))
               (vector-push-extend ch inbuf)
               (setq ch-prev nil))
             (when (>= (length inbuf) 510)
               (return inbuf)
               (setf (fill-pointer inbuf) 0))
             (when (char= ch #\return)
               (setq ch-prev t))
             (when (and (char= ch #\linefeed) ch-prev)
               (return inbuf)
               (setf (fill-pointer inbuf) 0) )))))))

;; the cursor can not work in a multithreaded mode, there is only one cursor,
;; and it has to jump back and forth between multiple windows.
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
                                :input-blocking nil))
           (st (connect "185.30.166.37" 6667))
           ;; no of chars in the input line.
           (n 0))

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
               ;; TODO: add command line parsing and evaluation here.
               (raw st strin)
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
             (when (and line (eq line :eof) (return-from event-case)))
             (when line
               ;; save excursion prevents that the cursor jumps between windows.
               ;; it should always stay in the input window.
               (save-excursion win
               ;;(princ line wout)
               ;;(princ (parse line) wout)
               ;; TODO: add irc event handling here.
               ;; TODO: add a logger here.
               ;; http://picolisp.com/wiki/?ircclient
               (let* ((msg (parse line))
                      (event (.command msg)))
                 ;; in old girc.scm, at this point we passed msg to evaluate-message
                 ;; we will always upcase commands before comparing them.
                 (cond
                   ((string= event "PRIVMSG")
                    ;; only format PRIVMSGs
                    (princ (format nil "~A: ~A" (car (get-nick-user-host (.prefix msg))) (.text msg)) wout))
                   ((string= event "PING")
                    ;; TODO: before we send anything, we craft a proper irc message, then we send a message object,
                    ;; and before it gets sent it is serialized into a string.
                    (princ (format nil "PONG :~A" (.text msg)) wout)
                    (terpri wout)
                    (raw st (format nil "PONG :~A" (.text msg))))

                   ;; ignore 353, 372, JOIN, QUIT, PART, NOTICE
                   ((string= event "353") nil) ; nicklist
                   ((string= event "372") nil) ; motd

                   (t (princ msg wout))))));print other messages verbatim
               (terpri wout)
               (refresh wout))))

        ;; non-function keys, i.e. normal character keys
        (otherwise
         (when (and (characterp event)
                    (< (cadr (.cursor-position win)) (1- (.width win))))
           (incf n)
           (add-wide-char win event))))

      (close win)
      (close wout))))
