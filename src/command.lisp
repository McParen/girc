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
  (girc:display-logo))

;; eval the lisp form given on the command line and print the _return_ in the buffer
(defun lisp (&rest args)
  (girc:echo t (eval (read-from-string (car args)))))

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
     (if (girc:connection (girc:current-buffer))
         (with-slots (girc:rpl-list-channels) (girc:connection (girc:current-buffer))
           (if girc:rpl-list-channels
               (let ((buf (girc:find-buffer (girc:name (girc:connection (girc:current-buffer))))))
                 (loop for chan in girc:rpl-list-channels
                       ;; dont display the whole list, bit just args (default 10) channels.
                       repeat (if args
                                  (parse-integer (car args))
                                  10)
                       do (destructuring-bind (channel-name user-number topic) chan
                            (girc:display buf
                                          "~20A ~5@A  ~vA"
                                          channel-name
                                          user-number
                                          (- (crt:width (girc:output-window girc:*ui*)) 28)
                                          (if (> (length topic) (- (crt:width (girc:output-window girc:*ui*)) 28))
                                              (subseq topic 0 (- (crt:width (girc:output-window girc:*ui*)) 28))
                                              topic)))))
               (girc:echo t "-!- Channel list is empty. Run `channel load' to reload from server.")))
         (girc:echo t "-!- Channel list: Buffer not associated with a connection.")))))

#|

/buffer CMD arg0 arg1

/buffer kill
/buffer list

/buffer add <server> [ <target> ]

|#
(defun buffer (cmd &optional arg0 arg1)
  (alexandria:switch (cmd :test #'string=)
    ("kill"
     (typecase (girc:current-buffer)
       (girc:connection-buffer
        (if (girc:connectedp (girc:connection (girc:current-buffer)))
            (girc:echo t "-!- Can't kill a connected server buffer.")
            (if (crt:children (girc:current-buffer))
                (girc:echo t "-!- Can't kill a server buffer with child buffers.")
                (girc:remove-buffer))))
       (girc:target-buffer
        (girc:remove-buffer))
       (girc:buffer
        (girc:echo t "-!- Can't kill the main buffer."))))
    ("list"
     (girc:echo t "Number" "Connection" "Target" "Current")
     (let ((n 0))
       (labels ((show (buf)
                  (typecase buf
                    (girc:target-buffer
                     (girc:echo t n (girc:name (girc:connection buf)) (girc:target buf) (girc:currentp buf)))
                    (girc:connection-buffer
                     (girc:echo t n (girc:name (girc:connection buf)) (girc:currentp buf)))
                    (girc:buffer
                     (girc:echo t n "main" (girc:currentp buf))))
                  (incf n)
                  (when (crt:children buf)
                    (dolist (i (crt:children buf))
                      (show i)))))
         (show girc:*buffers*))))
    ;; arg0 := <server>
    ;; arg1 := <target>
    ("add"
     (if arg0
         ;; server given, check for target
         (if arg1
             ;; server and target given
             (if (girc:find-buffer arg0 arg1)
                 (girc:echo t "-!- Buffer already exists:" arg0 arg1)
                 (girc:add-select-target-buffer arg0 arg1))
             ;; target not given, arg0=server
             (progn
               (if (girc:find-buffer arg0)
                   (girc:echo t "-!- Buffer already exists:" arg0)
                   (girc:add-select-server-buffer arg0))))
         ;; server not given
         (girc:echo t "-!- Server not given.")))
    ("names"
     (if (and (typep (girc:current-buffer) 'girc:target-buffer)
              (girc:target (girc:current-buffer))
              (girc:channelp (girc:target (girc:current-buffer))))
         (let ((chan (find (girc:target (girc:current-buffer))
                           (girc:channels (girc:connection (girc:current-buffer)))
                           :key #'girc:name
                           :test #'string=)))
           (girc:display t "~{~A~^ ~}" (girc:nicknames chan)))
         (girc:echo t "-!- Not in a channel buffer.")))
    ("target"
     ;; set the buffer target, or remove it by setting it to nil.
     (if (typep (girc:current-buffer) 'girc:target-buffer)
         (progn
           (setf (girc:target (girc:current-buffer)) arg0))
         (girc:echo t "-!- Not in a channel/query buffer.")))
    ("connection"
     ;; associate a buffer with an existing connection
     (if (typep (girc:current-buffer) 'girc:connection-buffer)
         (if arg0
             (let ((conn (girc:find-connection arg0)))
               (if conn
                   (if (and (girc:connection (girc:current-buffer))
                            (girc:connectedp (girc:connection (girc:current-buffer))))
                       (girc:echo t "-!- Can't set new connection while connected.")
                       (progn
                         (setf (girc:connection (girc:current-buffer)) conn)
                         (girc:echo t "*** Buffer connection set to" arg0)))
                   (progn
                     (girc:echo t "-!- Server" arg0 "not found."))))
             ;; if no connection name was given,
             ;; set the connection to nil (remove the current connection)
             (let ((conn (girc:connection (girc:current-buffer))))
               (if conn
                   (if (and (girc:connection (girc:current-buffer))
                            (girc:connectedp (girc:connection (girc:current-buffer))))
                       (girc:echo t "-!- Can't remove connection while connected.")
                       (progn
                         (setf (girc:connection (girc:current-buffer)) nil)
                         (girc:echo t "*** Buffer connection removed.")))
                   (girc:echo t "-!- Buffer not associated with a connection."))))
         (girc:echo t "-!- Not in a server buffer.")))
    (t
     (if cmd
         (girc:echo t "-!- Undefined command: /buffer" cmd)
         (girc:echo t "-!- Required command: /buffer <command>")))))

;; /info
;; /info event
(defun info (arg)
  (alexandria:switch (arg :test #'string=)
    ("event"
     (apply #'girc:echo t (loop for h in girc:*event-handlers* collect (car h))))
    (t
     (if arg
         (girc:echo t "-!- Undefined value:" arg)
         ;; if no command is given
         (girc:display-info)))))

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
         (apply #'girc:add-connection name host args)
         (girc:echo t "-!- Required arguments: /server add <name> <host>")))
    ("list"
     (girc:echo t "Name" "Host" "Nick" "Port" "SSL" "Connected" "Channels")
     (when girc:*connections*
       (dolist (c girc:*connections*)
         (girc:echo t
               (girc:name c)
               (girc:hostname c)
               (girc:nickname c)
               (girc:port c)
               (girc:sslp c)
               (girc:connectedp c)

               ;; list channels of the connection
               (when (girc:connectedp c)
                 (format nil "~{~A~^ ~}"
                         (if (girc:channels c)
                             (mapcar #'girc:name (girc:channels c))
                             nil)))))))
    (t
     (if cmd
         (girc:echo t "-!- Undefined command: /server" cmd)
         (girc:echo t "-!- Required command: /server <command>")))))

;; /connect
;; /connect <server name>
;; /connect <hostname> <nick>
(defun connect (name &optional nick)
  (if name
      (progn
        (when (girc::hostname-p name)
          (if nick
              (server "add" (girc:hostname-p name) name :nickname nick)
              (server "add" (girc:hostname-p name) name)))
        (let* ((name (if (girc:hostname-p name)
                         (girc:hostname-p name)
                         name))
               (con (girc:find-connection name)))
          (if con
              (progn
                (let ((buf (girc:find-buffer name)))
                  (if buf
                      (girc:select-buffer name)
                      (girc:add-select-server-buffer name)))
                (girc:connect con)
                ;; associate the current server buffer with the new connection
                (setf (girc:connection (girc:current-buffer)) con))
              (girc:echo t "-!- Server not found:" name))))
      ;; if a name was not given, try to use the connection associated with the buffer.
      (if (and (typep (girc:current-buffer) 'girc:connection-buffer)
               (girc:connection (girc:current-buffer)))
          (if (girc:connectedp (girc:connection (girc:current-buffer)))
              (girc:echo t "-!- Server already connected: " (girc:name (girc:connection (girc:current-buffer))))
              (progn
                (girc:connect (girc:connection (girc:current-buffer)))))
          (girc:echo t "-!- Buffer not associated with a connection."))))

;; /exit
(defun exit ()
  (crt:exit-event-loop (girc:input-field girc:*ui*) nil))

;; /join #channel
;; /join #chan1,#chan2

;;; TODO 231001 /join #c1,#c2 #k1,#k2

(defun join (channel)
  "Send a request to join the channel on the current server.

The channel is joined when a join event returned by the server is
handled.

This has to be done by the event handler in case we can't join the
channel and receive an error message, for example 474."
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if channel
         (irc:join t channel)
         (girc:echo t "-!- Required argument: /join <channel>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /msg target text
;; <nick> hello channel
;; *nick* hello nick
;; <nick@#chan> hello other channel
;; *nick@nick2* hello other nick
(defun msg (target &rest text)
  (typecase (girc:current-buffer)
    (girc:connection-buffer
     (if (girc:connection (girc:current-buffer))
         (if (girc:connectedp (girc:connection (girc:current-buffer)))
             (progn
               (if (girc:channelp target)
                   (girc:display t "<~A> ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text))
                   (girc:display t "*~A* ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text)))
               (irc:privmsg t target (car text)))
             (girc:echo t "-!- Server not connected:" (girc:name (girc:connection (girc:current-buffer)))))
         (girc:echo t "-!- Buffer not associated with a connection.")))
    (girc:target-buffer
     (if (string-equal target (girc:target (girc:current-buffer)))
         (progn
           (if (girc:channelp target)
               (girc:display t "<~A> ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text))
               (girc:display t "*~A* ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text)))
           (irc:privmsg t target (car text)))
         (progn
           (if (girc:channelp target)
               (girc:display t "<~A@~A> ~A" (girc:nickname (girc:connection (girc:current-buffer))) target (car text))
               (girc:display t "*~A@~A* ~A" (girc:nickname (girc:connection (girc:current-buffer))) target (car text)))
           (irc:privmsg t target (car text)))))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /query
;; /query <nick>
(defun query (nick)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if (girc:connection (girc:current-buffer))
         (if nick
             (if (string-equal nick
                               (girc:nickname (girc:connection (girc:current-buffer))))
                 (girc:display t "-!- ~A is your own nickname." nick)
                 (if (and nick
                          (girc:find-buffer (girc:name (girc:connection (girc:current-buffer))) nick))
                     (girc:echo t "-!- Buffer already exists:" (girc:name (girc:connection (girc:current-buffer))) nick)
                     (typecase (girc:current-buffer)
                       (girc:target-buffer
                        (if (null (girc:target (girc:current-buffer)))
                            (setf (girc:target (girc:current-buffer)) nick)
                            (progn
                              (girc:add-select-target-buffer (girc:name (girc:connection (girc:current-buffer))) nick)))
                        (girc:echo t "*** Starting a query with" nick))
                       (girc:connection-buffer
                        (girc:add-select-target-buffer (girc:name (girc:connection (girc:current-buffer))) nick)
                        (girc:echo t "*** Starting a query with" nick)))))
             (if (typep (girc:current-buffer) 'girc:target-buffer)
                 (if (and (girc:target (girc:current-buffer))
                          (not (girc:channelp (girc:target (girc:current-buffer)))))
                     (let* ((tgt (girc:target (girc:current-buffer)))
                            (buf (girc:find-buffer (girc:name (girc:connection (girc:current-buffer))) tgt)))
                       (girc:echo buf "*** Ending the query with" tgt)
                       (setf (girc:target (girc:current-buffer)) nil))
                     (girc:echo t "-!- Current target is not a query."))
                 (girc:echo t "-!- Server buffer does not have a target.")))
         (girc:echo t "-!- Buffer not associated with a connection.")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /ctcp #testus ACTION tests this command.
;; * haoms tests this command.
(defun ctcp (target command &rest args)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (irc:ctcp t target command (car args)))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

(defun action (target &rest text)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if text
         (progn
           (irc:ctcp t target "ACTION" (car text))
           (girc:display t "* ~A ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text)))
         (girc:display t "-!- CTCP ACTION requires a text argument.")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

(defun me (&rest text)
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if (girc:target (girc:current-buffer))
         (if text
             (action (girc:target (girc:current-buffer)) (car text))
             (girc:echo t "-!- Required argument: /me <text>"))
         (girc:echo t "-!- No target associated with the curent buffer.")))
    (girc:connection-buffer
     (girc:echo t "-!- No target associated with a server buffer."))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /nick new-nick
;; The changes to the client settings happen when the server replies,
;; because it is possible that we cant use the new nick.
(defun nick (new-nick)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if new-nick
         (irc:nick t new-nick)
         (girc:echo t "-!- Required argument: /nick <new-nick>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /say hello there dear john
(defun say (&rest text)
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if (girc:connection (girc:current-buffer))
         (if (girc:connectedp (girc:connection (girc:current-buffer)))
             (if (girc:target (girc:current-buffer))
                 (progn
                   (if (girc:channelp (girc:target (girc:current-buffer)))
                       (girc:display t "<~A> ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text))
                       (girc:display t "*~A* ~A" (girc:nickname (girc:connection (girc:current-buffer))) (car text)))
                   (irc:privmsg t (girc:target (girc:current-buffer)) (car text)))
                 (girc:echo t "-!- Buffer not associated with a target."))
             (girc:echo t "-!- Server not connected:" (girc:name (girc:connection (girc:current-buffer)))))
         (girc:echo t "-!- Buffer not associated with a connection.")))
    (girc:connection-buffer
     (girc:echo t "-!- Server buffer not associated with a target."))
    (girc:buffer
     (girc:echo t "-!- Main buffer not connected to a server."))))

;; /part
;; /part #channel
(defun part (&optional channel)
  "Leave the given channel or the current channel, if no channel is given."
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if channel
         (progn
           ;; remove the channel from the channel list of the connection
           (girc:remove-channel channel (girc:connection (girc:current-buffer)))
           (irc:part t channel))
         ;; if no channel was given, but the current buffer has a target
         (if (girc:target (girc:current-buffer))
             ;; recursively leave the current target
             (part (girc:target (girc:current-buffer)))
             ;; if we call part from a buffer without a target, we need an argument
             (girc:echo t "-!- Required argument: /part <channel>"))))
    (girc:connection-buffer
     (if channel
         (progn
           ;; remove the channel from the channel list of the connection
           (girc:remove-channel channel (girc:connection (girc:current-buffer)))
           (irc:part t channel))
         (girc:echo t "-!- Required argument: /part <channel>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

;; /quit
(defun quit (&rest message)
  "Quit the chat session, disconnect from the server."
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if message
         (irc:quit t (car message))
         (girc:echo t "-!- Required argument: /quit <message>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

;; /quote args*
;; /quote WHOIS McParen
(defun quote (&rest args)
  "Send a raw, unmodified irc message to the current server."
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if args
         (progn
           (girc:echo t "/quote" (car args))
           (girc:send-raw-message t (car args)))
         (girc:echo t "-!- Required argument: /quote <args>*")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

;; /whois nick
;; /whois nick nick
;; If nick is given a second time, additionally return 317 seconds idle, signon time
(defun whois (&rest args)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if args
         (irc:whois t (car args))
         (girc:echo t "-!- Required argument: /whois <nick>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

(defun show (name)
  "Show the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (girc:show-buffer-line t))
    ("buffer-column"
     (girc:show-buffer-column t))
    ("topic"
     (girc:show-topic-line t))))

(defun hide (name)
  "Hide the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (girc:show-buffer-line nil))
    ("buffer-column"
     (girc:show-buffer-column nil))
    ("topic"
     (girc:show-topic-line nil))))
