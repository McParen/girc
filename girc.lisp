(in-package :de.anvi.girc)

;; we read the irc stream char by char.
(defun sr (stream)
  (let ((str (usocket:socket-stream stream)))
    (loop
       ;; try to read a char. if there is no char waiting, return nil. dont block.
       for ch = (read-char-no-hang str)
       ;; nur wenn ein char gelesen wurde, anzeigen.
       if ch do (princ ch))))

(defun stream-print (string stream)
  ;; we have to do princ instead of print to the irc server.
  ;; print outputs for the lisp reader and includes double quotes with strings.
  ;; the irc server bails on double quotes, it wants just ascii chars.
  (princ string stream)
  (force-output stream))

;; soll sofort nach dem socket-connect gesendet werden.
;; erst wenn der login angekommen ist, sendet der server 001.
(defun login (s)
  ;; nick und user sind zwei verschiedene commands, deswegen muessen das zwei separate zeilen sein.
  ;; wir koennen nicht zwei befehle in einer zeile schicken.
  (stream-print (format nil "NICK haom~C~C" #\return #\linefeed) s)
  (stream-print (format nil "USER haom 0 0 :haom~C~C" #\return #\linefeed) s))

(defun logout (s)
  (stream-print (format nil "QUIT~C~C" #\return #\linefeed) s))
  
;;(defparameter my-stream (usocket:socket-connect "185.30.166.37" 6667))

(defun ui1 ()
    (let* ((so (usocket:socket-connect "185.30.166.37" 6667))
           (st (usocket:socket-stream so)))
      (login st)
      (loop 
         (let ((ch (read-char-no-hang st)))
           (if ch
               (princ ch))))))

;; irc messages contain #\return
;; we cant print them, because the cursor then goes to the first column and
;; we overwrite the previous line.
(defun ui2 ()
  (with-screen (scr :input-echoing nil :input-blocking t :cursor-visibility t)
    (princ "hello there" scr) (refresh scr)
    (princ #\return scr) (refresh scr)
    (princ #\linefeed scr) (refresh scr)
    (princ "dear john" scr) (refresh scr)
    (get-char scr)))


(defun ui3 ()
  (with-screen (scr :input-echoing nil :input-blocking nil :cursor-visibility t)
    (let* ((so (usocket:socket-connect "185.30.166.37" 6667))
           (st (usocket:socket-stream so)))
      (login st)
      (loop 
         (let ((ch (read-char-no-hang st)))
           (if (and ch (char/= ch #\return) (char/= ch #\linefeed))
               (progn (princ ch scr) (refresh scr))))))))

;; graphic-char-p, dann geht aber dcc nicht.
(defun read-irc-line-no-hang (stream)
  (let ((inbuf (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)) ;; empty string ""
        (ch-prev nil))
    (loop
       (let ((ch (read-char-no-hang stream nil :eof)))
         (if ch
             (progn
               (when (eq ch :eof)
                 (return :eof))
               (when (and (char/= ch #\return) (char/= ch #\linefeed))
                 (vector-push-extend ch inbuf)
                 (setq ch-prev ch))
               (when (>= (length inbuf) 510)
                 (return inbuf))
               (when (char= ch #\return)
                 (setq ch-prev ch))
               (when (and (char= ch #\linefeed) (char= ch-prev #\return))
                 (return inbuf)))
             (return nil))))))

(defun ui ()
  (with-screen (scr :input-echoing nil :cursor-visibility t :enable-colors nil)
    (let* ((wout (make-instance 'window :height (1- (.height scr)) :width (.width scr) :enable-scrolling t :position '(0 0)))
           (win (make-instance 'window :height 1 :width (.width scr) :position (list (1- (.height scr)) 0) :enable-fkeys t :input-blocking nil))
           (so (usocket:socket-connect "185.30.166.37" 6667))
           (st (usocket:socket-stream so))
           (n 0)) ;; number of characters in the input line.

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
;;                    (strout (parse (read-from-string strin))))
;;               (princ strout))
;;             (terpri wout)
               (stream-print (format nil "~A~C~C" strin #\return #\linefeed) st)
               (setf n 0)
               (clear win) 
               (refresh wout)))))
        (:dc ;; entf, das gleiche wie backspace nur ohne vorheriges move-by
         (when (> n (cadr (.cursor-position win)))
           (decf n)
           (delete-char win)))
        (:backspace
         (when (> (cadr (.cursor-position win)) 0)
           (decf n)
           (move-by win 0 -1)
           (delete-char win)))
        (#\q (return-from event-case))
        ((nil)
         (let ((line (read-irc-line-no-hang st)))
           (when (and line (eq line :eof) (return-from event-case)))
           (when line
             (princ line wout)
             (terpri wout)
             (refresh wout))))

;;         (let ((ch (read-char-no-hang st nil :eof)))
;;           (when (and ch (eq ch :eof)) (return-from event-case))
;;           (when (and ch (char/= ch #\return))
;;             (princ ch wout)
;;             (refresh wout))))

        (otherwise
         (when (and (typep event 'standard-char)
                    (< (cadr (.cursor-position win)) (1- (.width win))))
           (incf n)
           (format win "~A" event))))
      (close win)
      (close wout))))


#|

;; typep, type-of
;; character, base-char, standard-char, extended-char



slime:

C-c RET = macroexpand 1 <--
C-c M-m = macroexpand all

(with-event-loop (win event) (:up (print event)))

(with-event-loop (win)
    (:a 1)
    (:b 2)
    ((nil) nil))

(defparameter *event-handler-alist*
  '((#\Q . #'(lambda () (return-from event-loop)))))

;; add a procedure to handle an event to a windows event handler alist.
;; called by handle-window event.
(defun defevent (window event)

(defun handle-window-event (window event)
  (funcall (cadr (assoc event *event-handler-alist*))))

* we cant make readline functionality a standard for get-string.

  reason: readline functionality requires a LOT of keybindings, and
  providing those as a default would mean taking those standard keybindings
  from the application.

  that means, that _every_ application has to provide readline functionality
  by itself.

|#
