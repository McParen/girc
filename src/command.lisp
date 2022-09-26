(in-package :de.anvi.girc)

;; we cant call eval from .gircrc because it requires ncurses to be initialized first.
(defun eval (input)
  "Take an input line, parse command and args and pass them to a handler function."
  (destructuring-bind (cmd . args) (parse-user-input input)
    (crt:save-excursion (input-window *ui*)
      (if cmd
          (let ((fun (fboundp (find-symbol (string-upcase cmd) 'de.anvi.girc.command))))
            (if fun
                (apply fun (parse-user-arguments (sb-introspect:function-lambda-list fun) args))
                ;; if no handler was found, use the default handler
                (funcall (lambda (cmd args)
                           (display t "-!- Undefined command: ~A ~A" cmd args))
                         cmd args)))
          ;; if no command was given, send the input to the current buffer target
          (cmd:say args)))))

(defun handle-user-command (field)
  "Parse the content of the input line, call the function associated with the user command.

At the moment, no default command is called if the first input token is not a /command.

Bound to #\newline in girc-input-map."
  (let ((input (crt:value field)))
    (when input
      (crt:reset field)
      (eval input)))
  (update-output))

;;; Implementation of user commands

(in-package :de.anvi.girc.command)

;; send to the current server:
;; (send t :command list-of-param-strings text-string)

(defun logo ()
  (display-logo))

;; eval the lisp form given on the command line and print the return in the buffer
(defun lisp (&rest args)
  (echo t (cl:eval (read-from-string (first args)))))

;; /buffer kill
;; /buffer list
;; /buffer new
;; /buffer new <connection>
;; /buffer new <connection> <target>
;; /buffer target
;; /buffer target <channel>
;; /buffer connection <connection>

(defun buffer (cmd arg0 arg1)
  (alexandria:switch (cmd :test #'string=)
    ("kill"
     (if (= 1 (length (crt:items *buffers*)))
         (echo t "-!- Can't kill the last buffer.")
         (progn
           (remove-buffer)
           (setf (changedp (current-buffer)) t)
           (update-status))))
    ("list"
     (echo t "Number" "Connection" "Target")
     (loop for i from 0 below (length (crt:items *buffers*))
           for b = (nth i (crt:items *buffers*))
           do
              (echo t i
                    (when (connection b)
                      (name (connection b)))
                    (target b))))
    ("new"
     (append-buffer (cond ((and arg0 arg1)
                           (make-instance 'girc:buffer :connection (find-connection arg0) :target arg1))
                          (arg0
                           (make-instance 'girc:buffer :connection (find-connection arg0)))
                          (t
                           (make-instance 'girc:buffer))))
     (select-last-buffer))
    ("names"
     (if (target (current-buffer))
         (let ((chan (find (target (current-buffer))
                           (channels (connection (current-buffer)))
                           :key #'name :test #'string=)))
           (display t "~{~A~^ ~}" (nicknames chan)))
         (echo t "-!- Not in a channel buffer.")))
    ("target"
     ;; set the buffer target or nil
     (setf (target (current-buffer)) arg0)
     (update-status))
    ("connection"
     ;; associate a buffer with an existing connection or nil
     (if arg0
         (setf (connection (current-buffer)) (find-connection arg0))
         (setf (connection (current-buffer)) nil)))
    (t
     (if cmd
         (echo t "-!- Undefined command: /buffer" cmd)
         (echo t "-!- Required command: /buffer <command>")))))

;; /info
;; /info handler
;; /info buffer  = /buffer list
;; /info server  = /server list
;; /info command
(defun info (cmd)
  (alexandria:switch (cmd :test #'string=)
    ("event"
     (apply #'echo t (loop for h in *event-handlers* collect (car h))))
    ("buffer"
     (echo t "Number" "Connection" "Target")
     (loop for i from 0 below (length (crt:items *buffers*))
           for b = (nth i (crt:items *buffers*))
           do
              (echo t i
                    (when (connection b)
                      (name (connection b)))
                    (target b))))
    ("server"
     (echo t "Network" "Host" "Nick" "Connected")
     (when *connections*
       (dolist (c *connections*)
         (echo t
               (name c)
               (nickname c)
               (hostname c)
               (connectedp c)))))
    (t
     (if cmd
         (echo t "-!- Undefined command: /info" cmd)
         ;; if no command is given
         (display-info)))))

;; /server add <name> <host> <nick>
;; /server add freenode irc.freenode.net haom
;; /server list
(defun server (cmd name host nick)
  (alexandria:switch (cmd :test #'string=)
    ("add"
     (if (and name nick host)
         (push (make-instance 'connection :name name :nickname nick :hostname host) *connections*)
         (echo t "-!- Required arguments: /server add <name> <host> <nick>")))
    ("list"
     (echo t "Network" "Host" "Nick" "Connected" "Channels")
     (when *connections*
       (dolist (c *connections*)
         (echo t
               (name c)
               (nickname c)
               (hostname c)
               (connectedp c)

               ;; list channels of the connection
               (when (connectedp c)
                 (format nil "~{~A~^ ~}"
                         (if (channels (connection (current-buffer)))
                             (mapcar #'name (channels (connection (current-buffer))))
                             nil)))))))
    (t
     (if cmd
         (echo t "-!- Undefined command: /server" cmd)
         (echo t "-!- Required command: /server <command>")))))

;; /connect <name>
(defun connect (name)
  (if name
      (let* ((con (find-connection name)))
        (if con
            (progn (girc:connect con)
                   ;; associate the current buffer with the new connection
                   (setf (connection (current-buffer)) con)
                   (update-status))
            (echo t "-!- Connection not found:" name)))
      (echo t "-!- Required argument: /connect <name>")))

;; /exit
(defun exit ()
  (crt:exit-event-loop (input-field *ui*) nil))

;; /join #channel
(defun join (channel)
  (if channel
      (progn
        ;; if the channel isnt already the target, add a new target buffer
        (unless (string= channel (target (current-buffer)))
          (buffer "new"
                  (name (connection (current-buffer)))
                  channel))

        ;; add the channel to the connection
        (push (make-instance 'channel :name channel)
              (channels (connection (current-buffer))))

        (send t :join (list channel)))
      (echo t "-!- Required argument: /join <channel>")))

;; /msg target text
(defun msg (target &rest text)
  (display t "~A @ ~A: ~A" (nickname (connection (current-buffer))) target (car text))
  (send t :privmsg (list target) (car text)))

;; /say hello there dear john
(defun say (&rest text)
  (if (connection (current-buffer))
      (if (connectedp (connection (current-buffer)))
          (if (target (current-buffer))
              (progn
                (display t "<~A> ~A" (nickname (connection (current-buffer))) (car text))
                (send t :privmsg (list (target (current-buffer))) (car text)))
              (display t "-!- Current buffer not associated with a target."))
          (display t "-!- Connection ~A not connected." (name (connection (current-buffer)))))
      (display t "-!- Current buffer not associated with a connection.")))

;; /part #channel
(defun part (channel)
  (if channel
      (progn
        ;; if the given channel is the current target, kill the buffer when leaving the channel
        (when (string= channel (target (current-buffer)))
          (buffer "kill" nil nil))

        ;; remove the channel from the channel list of the connection
        (setf (channels (connection (current-buffer)))
              (remove channel (channels (connection (current-buffer))) :test #'string= :key #'name))

        (send t :part (list channel)))
      (progn
        ;; if no channel was given, but the current buffer has a target
        (if (target (current-buffer))
            ;; recursively leave the target
            (part (target (current-buffer)))
            ;; if we call part from a server buffer without a target, we need an argument
            (echo t "-!- Required argument: /part <channel>")))))

;; /quit
(defun quit ()
  (send t :quit))

;; /quote args*
;; /quote WHOIS McParen
(defun quote (&rest args)
  (echo t "/quote" (car args))
  (send-raw t (car args)))

;; /whois nick
;; /whois nick nick
;; If nick is given a second time, additionally return 317 seconds idle, signon time
(defun whois (&rest args)
  (send t :whois (list (car args))))
