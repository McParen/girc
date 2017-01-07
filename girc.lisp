(in-package :de.anvi.girc)

(defun connect (server port)
  "Take a hostname or IP and a port, connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect server port))
         (stream (usocket:socket-stream socket)))
    stream))

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
           (move-by win 0 -1)))
        (:right 
         (when (< (cadr (.cursor-position win)) n)
           (move-by win 0 1)))
        (#\newline
         (let ((*standard-output* wout))
           (when (> n 0)
             (let* ((strin (extract-string win :n n :y 0 :x 0)))
               (raw st strin)
               (setf n 0)
               (clear win) 
               (refresh wout)))))
        (:dc
         (when (> n (cadr (.cursor-position win)))
           (decf n)
           (delete-char win)))
        (:backspace
         (when (> (cadr (.cursor-position win)) 0)
           (decf n)
           (move-by win 0 -1)
           (delete-char win)))

        (#\r (quit st))
        (#\Q (return-from event-case))

        ;; we cant make this input-blocking nil instead of event (nil), because
        ;; we have to check for input during the nil event.
        ;; a much better way way would be to use separate threads for ui events and
        ;; network events.
        ((nil)
         (sleep 0.01)
         (when (listen st)
           (let ((line (read-irc-line st)))
             (when (and line (eq line :eof) (return-from event-case)))
             (when line
               ;;(princ line wout)
               (princ (parse line) wout)
               (terpri wout)
               (refresh wout)))))

        ;; non-function keys, i.e. normal character keys
        (otherwise
         (when (and (typep event 'standard-char)
                    (< (cadr (.cursor-position win)) (1- (.width win))))
           (incf n)
           (format win "~A" event))))

      (close win)
      (close wout))))
