(in-package :de.anvi.girc)

;;; Implementation of default user commands.

(defmacro define-command (command function)
  `(setf *user-commands*
         (acons ,command ,function *user-commands*)))

(defun get-command (cmd)
  (let ((cmd-pair (assoc cmd *user-commands* :test #'equal)))
    (if cmd-pair
        (cdr cmd-pair)
        nil)))

(define-command "connect"
    (lambda (cmd args)
      (let ((nick (if args (string-car args) "haom"))
            (host (if args (string-cadr args) "chat.freenode.net")))
      (setq *current-connection* (make-instance 'connection :nickname nick :hostname host)))))

(define-command "whois"
    (lambda (cmd args)
      (send :whois (list args) nil)))

(define-command "msg"
    (lambda (cmd args)
      (declare (ignore cmd))
      (let ((target (string-car args)) ; a target can be a nick or a channel
            (text (string-cdr args)))
        (display "~A @ ~A: ~A~%" (connection-nickname *current-connection*) target text)
        (send :privmsg (list target) text))))

(define-command "raw"
    (lambda (cmd args)
      (display "/~A ~A~%" cmd args)
      (send-raw args)))

(define-command "exit"
    (lambda (cmd args)
      (declare (ignore cmd args))
      (crt:exit-event-loop (input-field *ui*) nil)))

;; default command handler
;; if a command isnt defined, nothing should happen at all.
(define-command t
    (lambda (cmd args)
      ;; dont send anything, just display something on the output window.
      (display "/~A ~A (default)~%" cmd args)))
