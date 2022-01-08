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

(defun handle-user-command (field)
  "Parse the content of the input line, call the function associated with the user command.

At the moment, no default command is called if the first input token is not a /command.

Bound to #\newline in girc-input-map."
  (let ((input-string (crt:value field)))
    (when input-string
      (crt:reset field)
      (destructuring-bind (cmd . args) (parse-user-input input-string)
        (if cmd
            (let ((fun (get-command-handler cmd))) ; see command.lisp
              (if fun
                  (funcall fun args)
                  ;; if no handler was found, use the default handler
                  (funcall (lambda (cmd args)
                             (display t "Undefined command: ~A ~A" cmd args))
                           cmd args)))
            ;; if no command was given, send the input to the current buffer target
            (say args)))))
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

;; /buffer kill
;; /buffer list
;; /buffer new
;; /buffer new <connection>
;; /buffer new <connection> <target>
;; /buffer target
;; /buffer target <channel>
;; /buffer connection <connection>
(define-command buffer (args)
  (let ((cmd (ntharg 0 args)))
    (alexandria:switch (cmd :test #'string=)
      ("kill"
       (if (= 1 (length *buffers*))
           (echo t "Can't kill the last buffer.")
           (progn
             (if (= 0 *current-buffer-number*)
                 (setf *current-buffer* (nth 1 (reverse *buffers*))
                       *buffers* (butlast *buffers*))
                 (setf *current-buffer-number* (1- *current-buffer-number*)
                       *current-buffer* (nth *current-buffer-number* (reverse *buffers*))
                       *buffers* (remove (nth (1+ *current-buffer-number*) (reverse *buffers*)) *buffers*)))
             (setf (buffer-changed-p *current-buffer*) t)
             (update-status))))
      ("list"
       (echo t "Number" "Connection" "Target")
       (loop for i from 0 below (length *buffers*)
             for b = (nth i (reverse *buffers*))
             do
                (echo t i
                      (when (buffer-connection b)
                        (connection-name (buffer-connection b)))
                      (buffer-target b))))
      ("new"
       (case (arglen args)
         (1 (push (make-instance 'buffer)
                  *buffers*))
         (2 (push (make-instance 'buffer :connection (find-connection (ntharg 1 args)))
                  *buffers*))
         (3 (push (make-instance 'buffer :connection (find-connection (ntharg 1 args))
                                         :target (ntharg 2 args))
                  *buffers*))))
      ("target"
       ;; if no target was given, the target is set back to nil.
       (setf (buffer-target *current-buffer*) (ntharg 1 args))
       (update-status))
      ("connection"
       ;; associate a buffer with an existing connection without connecting to it
       (setf (buffer-connection *current-buffer*)
             (find-connection (ntharg 1 args)))))))

;; /info
;; /info handler
;; /info buffer  = /buffer list
;; /info server  = /server list
;; /info command
(define-command info (args)
  (case (arglen args)
    (0 (display-info))
    (1 (let ((cmd (ntharg 0 args)))
         (alexandria:switch (cmd :test #'string=)
           ("handler"
            (apply #'echo t (loop for h in *event-handlers* collect (car h))))
           ("buffer"
            (echo t "Number" "Connection" "Target")
            (loop for i from 0 below (length *buffers*)
                  for b = (nth i (reverse *buffers*))
                  do
                     (echo t i
                           (when (buffer-connection b)
                             (connection-name (buffer-connection b)))
                           (buffer-target b))))
           ("server"
            (echo t "Network" "Nick" "Host")
            (when *connections*
              (dolist (c *connections*)
                (echo t
                      (connection-name c)
                      (connection-nickname c)
                      (connection-hostname c)))))
           ("command"
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
       (echo t "Network" "Nick" "Host")
       (when *connections*
         (dolist (c *connections*)
           (echo t
                 (connection-name c)
                 (connection-nickname c)
                 (connection-hostname c))))))))

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
    (display t "~A @ ~A: ~A" (connection-nickname (buffer-connection *current-buffer*)) target text)
    (send t :privmsg (list target) text)))

(defun say (text)
  (if (buffer-connection *current-buffer*)
      (if (connectedp (buffer-connection *current-buffer*))
          (if (buffer-target *current-buffer*)
              (progn
                (display t "<~A> ~A" (connection-nickname (buffer-connection *current-buffer*)) text)
                (send t :privmsg (list (buffer-target *current-buffer*)) text))
              (display t "-!- Current buffer not associated with a target."))
          (display t "-!- ~A not connected." (connection-name (buffer-connection *current-buffer*))))
      (display t "-!- Current buffer not associated with a connection.")))

(define-command say (args)
  (say args))

;; /part #channel
(define-command part (args)
  (let ((channel (ntharg 0 args)))
    (send t :part (list channel))))

;; /quit
(define-command quit (args)
  (send t :quit))

;; /raw args*
(define-command raw (args)
  (display t "/raw ~A" args)
  (send-raw t args))

;; WHOIS nick nick additionally return seconds idle signon time
(define-command whois (args)
  (send t :whois (list args)))
