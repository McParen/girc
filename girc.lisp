(in-package :de.anvi.girc)

(defun stream-print (string stream)
  ;; we have to do princ instead of print to the irc server.
  ;; print outputs for the lisp reader and includes double quotes with strings.
  ;; the irc server bails on double quotes, it wants just ascii chars.
  (princ string stream)
  (force-output stream))

(defun connect (server port)
  "Take a hostname or IP and a port, connect to the irc server, return a server stream."
  (let* ((socket (usocket:socket-connect server port))
         (stream (usocket:socket-stream socket)))
    stream))

;; soll sofort nach dem socket-connect gesendet werden.
;; erst wenn der login angekommen ist, sendet der server 001.
(defun login (s)
  ;; nick und user sind zwei verschiedene commands, deswegen muessen das zwei separate zeilen sein.
  ;; wir koennen nicht zwei befehle in einer zeile schicken.
  (stream-print (format nil "NICK haom~C~C" #\return #\linefeed) s)
  (stream-print (format nil "USER haom 0 0 :haom~C~C" #\return #\linefeed) s))

(defun logout (s)
  (stream-print (format nil "QUIT~C~C" #\return #\linefeed) s))

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

      ;; 
      (login st)
      
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
               (stream-print (format nil "~A~C~C" strin #\return #\linefeed) st)
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
               (princ line wout)
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
