(in-package :de.anvi.girc)

;;; Implementation of default user commands.

(defparameter *user-commands* nil)

;; use equalp so that: CMD = cmd
(defun get-command-handler (cmd)
  ;; equalp or string-equal so we can test strings regardless of the case.
  (let ((cmd-pair (assoc cmd *user-commands* :test #'equalp)))
    (if cmd-pair
        (cdr cmd-pair)
        nil)))

(defun handle-user-command (field event &rest args1)
  "Bound to #\newline in girc-input-map."
  (let ((input-string (crt:value field)))
    (when input-string
      (apply #'crt:reset-field field event args1)
      (destructuring-bind (cmd . args) (parse-user-input input-string)
        (if cmd
            (let ((fun (get-command-handler cmd))) ; see command.lisp
              (if fun
                  (funcall fun args)
                  ;; if no handler was found, use the default handler
                  (funcall (lambda (cmd args)
                             (display "Undefined command: ~A ~A~%" cmd args))
                           cmd args)))
            ;; TODO 200328 the default command should be /say.
            nil)))))

;; TODO 200328 add a namend block like in a defun
(defmacro define-command (command (args) &body body)
  `(setf *user-commands*
         (acons ,(symbol-name command) ;; upcased string
                (lambda ,(list args) ,@body)
                *user-commands*)))

;; similar to define-command but binds an already existing function to a command
;; (bind-command msg 'command-msg)
(defmacro bind-command (command function)
  `(setf *user-commands*
         (acons ,(symbol-name command) ,function *user-commands*)))

;;; user commands

;; (send :command list-of-param-strings text-string)

(define-command connect (args)
  (let ((nick (if args (string-car args) "haom"))
        (host (if args (string-cadr args) "chat.freenode.net")))
    (setq *current-connection* (make-instance 'connection :nickname nick :hostname host))))

(define-command exit (args)
  (declare (ignore args))
  (crt:exit-event-loop (input-field *ui*) nil))

(define-command msg (args)
  (let ((target (string-car args)) ; a target can be a nick or a channel
        (text (string-cdr args)))
    (display "~A @ ~A: ~A~%" (connection-nickname *current-connection*) target text)
    (send :privmsg (list target) text)))

(define-command quit (args)
  (send :quit))

(define-command raw (args)
  (display "/raw ~A~%" args)
  (send-raw args))

(define-command whois (args)
  (send :whois (list args)))
