(in-package :de.anvi.girc)

;;; Implementation of default user commands.

(defparameter *user-commands* nil
  "Alist with user command strings as keys and functions as values.
Commands are added to this alist by define-command or bind-command.")

;; use equalp so that: CMD = cmd
(defun get-command-handler (cmd)
  "Take a string denoting a user command, return the handler function."
  ;; equalp or string-equal so we can test strings regardless of the case.
  (let ((cmd-pair (assoc cmd *user-commands* :test #'equalp)))
    (if cmd-pair
        (cdr cmd-pair)
        nil)))

(defun handle-user-command (field event &rest args1)
  "Parse the content of the input line, call the function associated with the user command.

At the moment, no default command is called if the first input token is not a /command.

Bound to #\newline in girc-input-map."
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
  "Add a handler for a user command (given by a symbol).

Args is a string containing all arguments given to the command."
  `(setf *user-commands*
         (acons ,(symbol-name command) ;; upcased string
                (lambda ,(list args) ,@body)
                *user-commands*)))

;; (bind-command msg 'command-msg)
(defmacro bind-command (command function)
  "Binds an already existing function to a command, similar to define-command."
  `(setf *user-commands*
         (acons ,(symbol-name command) ,function *user-commands*)))

;;; user commands

;; send to the current server:
;; (send :command list-of-param-strings text-string)

;; Syntax: /connect nickname host
(define-command connect (args)
  (let ((nick (if args (ntharg 0 args) "haom"))
        (host (if args (ntharg 1 args) "chat.freenode.net")))
    (setq *current-connection* (make-instance 'connection :nickname nick :hostname host))))

;; Syntax: /exit
(define-command exit (args)
  (declare (ignore args))
  (crt:exit-event-loop (input-field *ui*) nil))

;; Syntax /msg target text
(define-command msg (args)
  (let ((target (ntharg 0 args)) ; a target can be a nick or a channel
        (text (nthargs 1 args)))
    (display "~A @ ~A: ~A~%" (connection-nickname *current-connection*) target text)
    (send :privmsg (list target) text)))

(define-command quit (args)
  (send :quit))

(define-command raw (args)
  (display "/raw ~A~%" args)
  (send-raw args))

;; WHOIS nick nick additionally return seconds idle signon time
(define-command whois (args)
  (send :whois (list args)))
