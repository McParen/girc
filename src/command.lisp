(in-package :de.anvi.girc)

;; we cant call eval from .gircrc because it requires ncurses to be initialized first.
(defun eval (input)
  "Take an input line, parse command and args and pass them to a handler function."
  (destructuring-bind (cmd . args) (parse-user-input input)
    (crt:save-excursion (input-window *ui*)
      (if cmd
          (multiple-value-bind (symbol status)
              (find-symbol (string-upcase cmd) 'de.anvi.girc.command)
            (if (and symbol
                     (eq status :external))
                (let* ((fun (fboundp symbol)))
                  (if fun
                      (apply fun (parse-user-arguments (sb-introspect:function-lambda-list fun) args))
                      ;; if no handler was found, use the default handler
                      (funcall (lambda (cmd args)
                                 (display t "-!- Undefined command: ~A ~A" cmd args))
                               cmd args)))
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

(defun logo ()
  (display-logo))

;; eval the lisp form given on the command line and print the _return_ in the buffer
(defun lisp (&rest args)
  (echo t (eval (read-from-string (car args)))))

#|
/channels load
/channels load #lisp
/channels load *lisp*,>10,<20
/channels list
/channels list 20
|#
(defun channel (cmd &rest args)
  (alexandria:switch (cmd :test #'string=)
    ("load"
     ;; load the channels from the server into a local list
     ;; args is one single comma-separated list of filters: *m*,>10,<20
     (irc:list t (car args)))
    ("list"
     ;; display the n (default 10) largest channels
     (if (connection (current-buffer))
         (with-slots (rpl-list-channels) (connection (current-buffer))
           (if rpl-list-channels
               (loop for chan in rpl-list-channels
                     ;; dont display the whole list, bit just args (default 10) channels.
                     repeat (if args
                                (parse-integer (car args))
                                10)
                     do (destructuring-bind (channel-name user-number topic) chan
                          (display t "~20A ~5@A  ~vA"
                                   channel-name
                                   user-number
                                   (- (crt:width (output-window *ui*)) 28)
                                   (if (> (length topic) (- (crt:width (output-window *ui*)) 28))
                                       (subseq topic 0 (- (crt:width (output-window *ui*)) 28))
                                       topic))))
               (echo t "-!- Channel list is empty. Run `channel load' to reload from server.")))
         (echo t "-!- Channel list: Buffer not associated with a connection.")))))

#|
/buffer kill
/buffer list
/buffer new
/buffer new <connection>
/buffer new <connection> <target>
/buffer target
/buffer target <channel>
/buffer connection <connection>
|#
(defun buffer (cmd &optional arg0 arg1)
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
     (add-buffer arg0 arg1)
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
         (let ((conn (find-connection arg0)))
           (if conn
               (progn
                 (setf (connection (current-buffer)) conn)
                 (echo t "--- Current buffer connection set to" arg0)
                 (update-status))
               (progn
                 (echo t "-!- Server connection" arg0 "not found."))))
         ;; if no connection name was given,
         ;; set the connection to nil (remove the current connection)
         (let ((conn (connection (current-buffer))))
           (if conn
               (progn
                 (setf (connection (current-buffer)) nil)
                 (echo t "--- Buffer connection removed.")
                 (update-status))
               (echo t "-!- Current buffer not associated with a connection.")))))
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

;; Syntax:
;; /server add <name> <host> [:nickname] [:port] [:ssl] [:nickserv] [:login-method]
;; /server list
;;
;; Examples:
;; /server add lib irc.libera.chat
;; /server add freenode irc.freenode.net :nickname haom
;; /server add freenode irc.freenode.net :nickname haom :port 6697 :ssl t :nickserv MyNick:MyPass :login-method :sasl
(defun server (cmd name host &rest args &key &allow-other-keys)
  (alexandria:switch (cmd :test #'string=)
    ("add"
     (if (and name host)
         (apply #'add-connection name host args)
         (echo t "-!- Required arguments: /server add <name> <host>")))
    ("list"
     (echo t "Name" "Host" "Nick" "Port" "SSL" "Connected" "Channels")
     (when *connections*
       (dolist (c *connections*)
         (echo t
               (name c)
               (hostname c)
               (nickname c)
               (port c)
               (sslp c)
               (connectedp c)

               ;; list channels of the connection
               (when (connectedp c)
                 (format nil "~{~A~^ ~}"
                         (if (channels c)
                             (mapcar #'name (channels c))
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
            (progn
              (girc:connect con)
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
      ;; send a join request to the server.
      ;; the channel is actually joined when the join event is handled.
      ;; we have to do this in the join event handler in case we cant
      ;; join the channel and receive an error message, for example 474.
      (irc:join t channel)
      (echo t "-!- Required argument: /join <channel>")))

;; /msg target text
(defun msg (target &rest text)
  ;; display the msg we just sent.
  (display t "~A @ ~A: ~A" (nickname (connection (current-buffer))) target (car text))
  (irc:privmsg t target (car text)))

;; /ctcp #testus ACTION tests this command.
;; * haoms tests this command.
(defun ctcp (target command &rest args)
  (irc:ctcp t target command (car args)))

(defun action (target &rest text)
  (if text
      (progn
        (irc:ctcp t target "ACTION" (car text))
        (display t "* ~A ~A" (nickname (connection (current-buffer))) (car text)))
      (display t "-!- CTCP ACTION requires a text argument.")))

(defun me (&rest text)
  (if (target (current-buffer))
      (action (target (current-buffer)) (car text))
      (echo t "-!- No target associated with the curent buffer.")))

;; /nick new-nick
;; The changes to the client settings happen when the server replies.
(defun nick (new-nick)
  (irc:nick t new-nick))

;; /say hello there dear john
(defun say (&rest text)
  (if (connection (current-buffer))
      (if (connectedp (connection (current-buffer)))
          (if (target (current-buffer))
              (progn
                (display t "<~A> ~A" (nickname (connection (current-buffer))) (car text))
                (irc:privmsg t (target (current-buffer)) (car text)))
              (display t "-!- Current buffer not associated with a target."))
          (display t "-!- Connection ~A not connected." (name (connection (current-buffer)))))
      (display t "-!- Current buffer not associated with a connection.")))

;; /part #channel
(defun part (channel)
  (if channel
      (progn
        ;; if the given channel is the current target, kill the buffer when leaving the channel
        (when (string= channel (target (current-buffer)))
          (buffer "kill"))

        ;; remove the channel from the channel list of the connection
        (remove-channel channel (connection (current-buffer)))

        (irc:part t channel))
      (progn
        ;; if no channel was given, but the current buffer has a target
        (if (target (current-buffer))
            ;; recursively leave the current target
            (part (target (current-buffer)))
            ;; if we call part from a server buffer without a target, we need an argument
            (echo t "-!- Required argument: /part <channel>")))))

;; /quit
(defun quit (&rest message)
  (irc:quit t (car message)))

;; /quote args*
;; /quote WHOIS McParen
(defun quote (&rest args)
  (echo t "/quote" (car args))
  (send-raw-message t (car args)))

;; /whois nick
;; /whois nick nick
;; If nick is given a second time, additionally return 317 seconds idle, signon time
(defun whois (&rest args)
  (irc:whois t (car args)))
