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
      (eval input)
      ;; redraw the screen if a command has been called from the input line.
      ;; if a command has been directly called from a binding, it has to call redraw explicitely.
      ;; event functions call redraw from connection/handle-server-input.
      (redraw))))


;;; Implementation of user commands

(in-package :de.anvi.girc.command)

(defun logo ()
  (display-logo))

;; eval the lisp form given on the command line and print the _return_ in the buffer
(defun lisp (&rest args)
  (echo t (eval (read-from-string (car args)))))

#|
/channel load
/channel load #lisp
/channel load *lisp*,>10,<20
/channel list
/channel list 20
|#
(defun channel (cmd &rest args)
  (alexandria:switch (cmd :test #'string=)
    ("load"
     ;; load the channels from the server into a local list
     ;; args is one single comma-separated list of filters: *m*,>10,<20
     (irc:list t (car args)))
    ("list"
     ;; display the n (default 10) largest channels to the server buffer
     (if (connection (current-buffer))
         (with-slots (rpl-list-channels) (connection (current-buffer))
           (if rpl-list-channels
               (let ((buf (find-buffer (name (connection (current-buffer))))))
                 (loop for chan in rpl-list-channels
                       ;; dont display the whole list, bit just args (default 10) channels.
                       repeat (if args
                                  (parse-integer (car args))
                                  10)
                       do (destructuring-bind (channel-name user-number topic) chan
                            (display buf
                                     "~20A ~5@A  ~vA"
                                     channel-name
                                     user-number
                                     (- (crt:width (output-window *ui*)) 28)
                                     (if (> (length topic) (- (crt:width (output-window *ui*)) 28))
                                         (subseq topic 0 (- (crt:width (output-window *ui*)) 28))
                                         topic)))))
               (echo t "-!- Channel list is empty. Run `channel load' to reload from server.")))
         (echo t "-!- Channel list: Buffer not associated with a connection.")))))

#|

/buffer CMD arg0 arg1

/buffer kill
/buffer list

/buffer add <server> [ <target> ]

|#
(defun buffer (cmd &optional arg0 arg1)
  (alexandria:switch (cmd :test #'string=)
    ("kill"
     (typecase (current-buffer)
       (connection-buffer
        (if (connectedp (connection (current-buffer)))
            (echo t "-!- Can't kill a connected server buffer.")
            (if (crt:children (current-buffer))
                (echo t "-!- Can't kill a server buffer with child buffers.")
                (remove-buffer))))
       (target-buffer
        (remove-buffer))
       (girc:buffer
        (echo t "-!- Can't kill the main buffer."))))
    ("list"
     (echo t "Number" "Connection" "Target")
     (let ((n 0))
       (labels ((show (buf)
                  (typecase buf
                    (target-buffer
                     (echo t n (name (connection buf)) (target buf) (girc::currentp buf)))
                    (connection-buffer
                     (echo t n (name (connection buf)) (girc::currentp buf)))
                    (girc:buffer
                     (echo t n "main" (girc::currentp buf))))
                  (incf n)
                  (when (crt:children buf)
                    (dolist (i (crt:children buf))
                      (show i)))))
         (show *buffers*))))
    ;; arg0 := <server>
    ;; arg1 := <target>
    ("add"
     (if arg0
         ;; server given, check for target
         (if arg1
             ;; server and target given
             (if (find-buffer arg0 arg1)
                 (echo t "-!- Buffer already exists:" arg0 arg1)
                 (add-select-target-buffer arg0 arg1))
             ;; target not given, arg0=server
             (progn
               (if (find-buffer arg0)
                   (echo t "-!- Buffer already exists:" arg0)
                   (add-select-server-buffer arg0))))
         ;; server not given
         (echo t "-!- Server not given.")))
    ("names"
     (if (and (typep (current-buffer) 'girc:target-buffer)
              (target (current-buffer))
              (channelp (target (current-buffer))))
         (let ((chan (find (target (current-buffer))
                           (channels (connection (current-buffer)))
                           :key #'name
                           :test #'string=)))
           (display t "~{~A~^ ~}" (nicknames chan)))
         (echo t "-!- Not in a channel buffer.")))
    ("target"
     ;; set the buffer target, or remove it by setting it to nil.
     (if (typep (current-buffer) 'girc:target-buffer)
         (progn
           (setf (target (current-buffer)) arg0))
         (echo t "-!- Not in a channel/query buffer.")))
    ("connection"
     ;; associate a buffer with an existing connection
     (if (typep (current-buffer) 'girc:connection-buffer)
         (if arg0
             (let ((conn (find-connection arg0)))
               (if conn
                   (if (and (connection (current-buffer))
                            (connectedp (connection (current-buffer))))
                       (echo t "-!- Can't set new connection while connected.")
                       (progn
                         (setf (connection (current-buffer)) conn)
                         (echo t "*** Buffer connection set to" arg0)))
                   (progn
                     (echo t "-!- Server" arg0 "not found."))))
             ;; if no connection name was given,
             ;; set the connection to nil (remove the current connection)
             (let ((conn (connection (current-buffer))))
               (if conn
                   (if (and (connection (current-buffer))
                            (connectedp (connection (current-buffer))))
                       (echo t "-!- Can't remove connection while connected.")
                       (progn
                         (setf (connection (current-buffer)) nil)
                         (echo t "*** Buffer connection removed.")))
                   (echo t "-!- Buffer not associated with a connection."))))
         (echo t "-!- Not in a server buffer.")))
    (t
     (if cmd
         (echo t "-!- Undefined command: /buffer" cmd)
         (echo t "-!- Required command: /buffer <command>")))))

;; /info
;; /info event
(defun info (arg)
  (alexandria:switch (arg :test #'string=)
    ("event"
     (apply #'echo t (loop for h in *event-handlers* collect (car h))))
    (t
     (if arg
         (echo t "-!- Undefined value:" arg)
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

;; /connect
;; /connect <server name>
;; /connect <hostname> <nick>
(defun connect (name &optional nick)
  (if name
      (progn
        (when (girc::hostname-p name)
          (if nick
              (server "add" (girc::hostname-p name) name :nickname nick)
              (server "add" (girc::hostname-p name) name)))
        (let* ((name (if (girc::hostname-p name)
                         (girc::hostname-p name)
                         name))
               (con (find-connection name)))
          (if con
              (progn
                (let ((buf (find-buffer name)))
                  (if buf
                      (select-buffer name)
                      (add-select-server-buffer name)))
                (girc:connect con)
                ;; associate the current server buffer with the new connection
                (setf (connection (current-buffer)) con))
              (echo t "-!- Server not found:" name))))
      ;; if a name was not given, try to use the connection associated with the buffer.
      (if (and (typep (current-buffer) 'connection-buffer)
               (connection (current-buffer)))
          (if (connectedp (connection (current-buffer)))
              (echo t "-!- Server already connected: " (name (connection (current-buffer))))
              (progn
                (girc:connect (connection (current-buffer)))))
          (echo t "-!- Buffer not associated with a connection."))))

;; /exit
(defun exit ()
  (crt:exit-event-loop (input-field *ui*) nil))

;; /join #channel
;; /join #chan1,#chan2

;;; TODO 231001 /join #c1,#c2 #k1,#k2

(defun join (channel)
  "Send a request to join the channel on the current server.

The channel is joined when a join event returned by the server is
handled.

This has to be done by the event handler in case we can't join the
channel and receive an error message, for example 474."
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if channel
         (irc:join t channel)
         (echo t "-!- Required argument: /join <channel>")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

;; /msg target text
;; <nick> hello channel
;; *nick* hello nick
;; <nick@#chan> hello other channel
;; *nick@nick2* hello other nick
(defun msg (target &rest text)
  (typecase (current-buffer)
    (connection-buffer
     (if (connection (current-buffer))
         (if (connectedp (connection (current-buffer)))
             (progn
               (if (channelp target)
                   (display t "<~A> ~A" (nickname (connection (current-buffer))) (car text))
                   (display t "*~A* ~A" (nickname (connection (current-buffer))) (car text)))
               (irc:privmsg t target (car text)))
             (echo t "-!- Server not connected:" (name (connection (current-buffer)))))
         (echo t "-!- Buffer not associated with a connection.")))
    (target-buffer
     (if (string-equal target (target (current-buffer)))
         (progn
           (if (channelp target)
               (display t "<~A> ~A" (nickname (connection (current-buffer))) (car text))
               (display t "*~A* ~A" (nickname (connection (current-buffer))) (car text)))
           (irc:privmsg t target (car text)))
         (progn
           (if (channelp target)
               (display t "<~A@~A> ~A" (nickname (connection (current-buffer))) target (car text))
               (display t "*~A@~A* ~A" (nickname (connection (current-buffer))) target (car text)))
           (irc:privmsg t target (car text)))))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

;; /query
;; /query <nick>
(defun query (nick)
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if (connection (current-buffer))
         (if nick
             (if (string-equal nick
                               (nickname (connection (current-buffer))))
                 (echo t "-!- ~A is your own nickname." nick)
                 (if (and nick
                          (find-buffer (name (connection (current-buffer))) nick))
                     (echo t "-!- Buffer already exists:" (name (connection (current-buffer))) nick)
                     (typecase (current-buffer)
                       (girc:target-buffer
                        (if (null (target (current-buffer)))
                            (setf (target (current-buffer)) nick)
                            (progn
                              (add-select-target-buffer (name (connection (current-buffer))) nick)))
                        (echo t "*** Starting a query with" nick))
                       (girc:connection-buffer
                        (add-select-target-buffer (name (connection (current-buffer))) nick)
                        (echo t "*** Starting a query with" nick)))))
             (if (typep (current-buffer) 'girc:target-buffer)
                 (if (and (target (current-buffer))
                          (not (channelp (target (current-buffer)))))
                     (let* ((tgt (target (current-buffer)))
                            (buf (find-buffer (name (connection (current-buffer))) tgt)))
                       (echo buf "*** Ending the query with" tgt)
                       (setf (target (current-buffer)) nil))
                     (echo t "-!- Current target is not a query."))
                 (echo t "-!- Server buffer does not have a target.")))
         (echo t "-!- Buffer not associated with a connection.")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

;; /ctcp #testus ACTION tests this command.
;; * haoms tests this command.
(defun ctcp (target command &rest args)
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (irc:ctcp t target command (car args)))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

(defun action (target &rest text)
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if text
         (progn
           (irc:ctcp t target "ACTION" (car text))
           (display t "* ~A ~A" (nickname (connection (current-buffer))) (car text)))
         (display t "-!- CTCP ACTION requires a text argument.")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

(defun me (&rest text)
  (typecase (current-buffer)
    (girc:target-buffer
     (if (target (current-buffer))
         (if text
             (action (target (current-buffer)) (car text))
             (echo t "-!- Required argument: /me <text>"))
         (echo t "-!- No target associated with the curent buffer.")))
    (girc:connection-buffer
     (echo t "-!- No target associated with a server buffer."))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

;; /nick new-nick
;; The changes to the client settings happen when the server replies,
;; because it is possible that we cant use the new nick.
(defun nick (new-nick)
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if new-nick
         (irc:nick t new-nick)
         (echo t "-!- Required argument: /nick <new-nick>")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a connection."))))

;; /say hello there dear john
(defun say (&rest text)
  (typecase (current-buffer)
    (girc:target-buffer
     (if (connection (current-buffer))
         (if (connectedp (connection (current-buffer)))
             (if (target (current-buffer))
                 (progn
                   (if (channelp (target (current-buffer)))
                       (display t "<~A> ~A" (nickname (connection (current-buffer))) (car text))
                       (display t "*~A* ~A" (nickname (connection (current-buffer))) (car text)))
                   (irc:privmsg t (target (current-buffer)) (car text)))
                 (echo t "-!- Buffer not associated with a target."))
             (echo t "-!- Server not connected:" (name (connection (current-buffer)))))
         (echo t "-!- Buffer not associated with a connection.")))
    (girc:connection-buffer
     (echo t "-!- Server buffer not associated with a target."))
    (girc:buffer
     (echo t "-!- Main buffer not connected to a server."))))

;; /part
;; /part #channel
(defun part (&optional channel)
  "Leave the given channel or the current channel, if no channel is given."
  (typecase (current-buffer)
    (girc:target-buffer
     (if channel
         (progn
           ;; remove the channel from the channel list of the connection
           (remove-channel channel (connection (current-buffer)))
           (irc:part t channel))
         ;; if no channel was given, but the current buffer has a target
         (if (target (current-buffer))
             ;; recursively leave the current target
             (part (target (current-buffer)))
             ;; if we call part from a buffer without a target, we need an argument
             (echo t "-!- Required argument: /part <channel>"))))
    (girc:connection-buffer
     (if channel
         (progn
           ;; remove the channel from the channel list of the connection
           (remove-channel channel (connection (current-buffer)))
           (irc:part t channel))
         (echo t "-!- Required argument: /part <channel>")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a server."))))

;; /quit
(defun quit (&rest message)
  "Quit the chat session, disconnect from the server."
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if message
         (irc:quit t (car message))
         (echo t "-!- Required argument: /quit <message>")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a server."))))

;; /quote args*
;; /quote WHOIS McParen
(defun quote (&rest args)
  "Send a raw, unmodified irc message to the current server."
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if args
         (progn
           (echo t "/quote" (car args))
           (send-raw-message t (car args)))
         (echo t "-!- Required argument: /quote <args>*")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a server."))))

;; /whois nick
;; /whois nick nick
;; If nick is given a second time, additionally return 317 seconds idle, signon time
(defun whois (&rest args)
  (typecase (current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if args
         (irc:whois t (car args))
         (echo t "-!- Required argument: /whois <nick>")))
    (girc:buffer
     (echo t "-!- Buffer not associated with a server."))))

(defun show (name)
  "Show the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (show-buffer-line t))
    ("buffer-column"
     (show-buffer-column t))
    ("topic"
     (show-topic-line t))))

(defun hide (name)
  "Hide the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (show-buffer-line nil))
    ("buffer-column"
     (show-buffer-column nil))
    ("topic"
     (show-topic-line nil))))
