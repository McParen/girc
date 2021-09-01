(in-package :de.anvi.girc)

;;; User commands

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
                             (display t "Undefined command: ~A ~A" cmd args))
                           cmd args)))
            ;; TODO 200328 the default command should be /say.
            nil))))
  ;; if the current buffer has been changed, update the display.
  (when (buffer-changed-p *current-buffer*)
    (crt:save-excursion (input-window *ui*)
      (display-buffer *current-buffer*))))

;; TODO 200328 add a named block like in a defun
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

;;; Implementation of user commands

;; send to the current server:
;; (send t :command list-of-param-strings text-string)

(define-command logo (args)
  (display-logo))

;; /buffer new
;; /buffer list

(define-command buffer (args)
  (let ((cmd (ntharg 0 args)))
    (alexandria:switch (cmd :test #'string=)
      ("new"
       (case (arglen args)
         (1 (push (make-instance 'buffer) *buffers*))
         (2 (push (make-instance 'buffer
                                 :connection (find (ntharg 1 args) *connections* :key #'connection-name :test #'string-equal))
                  *buffers*))
         (3 (push (make-instance 'buffer
                                 :connection (find (ntharg 1 args) *connections* :key #'connection-name :test #'string-equal)
                                 :target (ntharg 2 args))
                  *buffers*)))
       (setf *buffers* (reverse *buffers*)))
      ("target"
       (setf (buffer-target *current-buffer*) (ntharg 1 args)))
      ("connection"
       (setf (buffer-connection *current-buffer*)
             (find (ntharg 1 args) *connections* :key #'connection-name :test #'string-equal))))))

(define-command info (args)
  (case (arglen args)
    (0 (display-info))
    (1 (let ((cmd (ntharg 0 args)))
         (alexandria:switch (cmd :test #'string=)
           ("handlers"
            (apply #'echo t (loop for h in *event-handlers* collect (car h))))
           ("buffers"
            (echo t "Number" "Connection" "Target")
            (loop for i from 0 below (length *buffers*)
                  for b = (nth i *buffers*)
                  do
                     (echo t i
                           (when (buffer-connection b)
                             (connection-name (buffer-connection b)))
                           (buffer-target b))))
           ("servers"
            (echo t "Network" "Nick" "Host")
            (when *connections*
              (dolist (c *connections*)
                (echo t
                      (connection-name c)
                      (connection-nickname c)
                      (connection-hostname c)))))
           ("commands"
            (apply #'echo t (loop for c in *user-commands* collect (car c)))))))))

;; /server add <name> <nick> <host>
;; /server add freenode haom irc.freenode.net
;; /server list

(define-command server (args)
  (let ((cmd (ntharg 0 args)))
    (alexandria:switch (cmd :test #'string=)
      ("add"
       (push (make-instance 'connection :name (ntharg 1 args) :nickname (ntharg 2 args) :hostname (ntharg 3 args))
             *connections*))
      ("list"
       (when *connections*
           (loop for con in *connections* do
             (display t "~A ~A ~A" (connection-name con) (connection-hostname con) (connection-nickname con))))))))

;; /connect <name>
(define-command connect (args)
  (let* ((name (ntharg 0 args))
         (con (find name *connections* :key #'connection-name :test #'string-equal)))
    (connect con)
    ;; associate the current buffer with the new connection
    (setf (buffer-connection *current-buffer*) con)
    (update-status)))

;; /exit
(define-command exit (args)
  (declare (ignore args))
  (crt:exit-event-loop (input-field *ui*) nil))

;; /join #channel
(define-command join (args)
  (let ((channel (ntharg 0 args)))
    (send t :join (list channel))))

;; /msg target text
(define-command msg (args)
  (let ((target (ntharg 0 args)) ; a target can be a nick or a channel
        (text (nthargs 1 args)))
    (display t "~A @ ~A: ~A~%" (connection-nickname (buffer-connection *current-buffer*)) target text)
    (send t :privmsg (list target) text)))

;; /part #channel
(define-command part (args)
  (let ((channel (ntharg 0 args)))
    (send t :part (list channel))))

;; /quit
(define-command quit (args)
  (send t :quit))

;; /raw args*
(define-command raw (args)
  (display t "/raw ~A~%" args)
  (send-raw t args))

;; WHOIS nick nick additionally return seconds idle signon time
(define-command whois (args)
  (send t :whois (list args)))
