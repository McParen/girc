(in-package :de.anvi.girc.command)

;;; Implementation of user commands

(defparameter *user-commands* nil)

;; when we want a quick command that is NOT defined as a reusable lisp
;; function in the cmd package.
(defmacro define-command (command lambda-list &body body)
  "Define a user command to be called from the command line.

If the command should also be callable as a lisp function, use defcmd
instead.

define-command is the command equivalent of define-event."
  `(setf *user-commands*
         (acons (symbol-name ',command)
                (lambda ,lambda-list ,@body)
                *user-commands*)))

;; when we want to add a command but use a lisp function with another name,
;; so it is like an alias, but it is not a lisp alias, so we cant use the
;; command name as a lisp function, but have to call the original function.

;; we need this for commands which would clash with functions from the
;; cl package like list, load, open, close, warn, get, set, etc.

(defmacro bind-command (command function)
  "Bind a user command name to a handler function.

The function can be given as a function object or a symbol
representing a fbound function.

bind-command is the command equivalent of bind-event."
  `(setf *user-commands*
         (acons (symbol-name ',command)
                ,function
                *user-commands*)))

;; when we want to use the lisp function and the command of the same name.
;; for example (cmd:buffer "kill"), (server "add"), etc.
(defmacro defcmd (name lambda-list &body body)
  "Define a command that can also be reused as a lisp function.

defcmd is a thin wrapper around defun and bind-command that binds
a command of same name as the function.

The command functions can then be called, for exmaple, from the
.gircrc init file to perform actions after the client start.

Since the cmd package uses cl, if symbols from the cl have to be used,
they have to be shadowed first."
  `(progn
     (defun ,name ,lambda-list ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name 'de.anvi.girc.command))
     (when (fboundp ',name)
       (bind-command ,name (fdefinition ',name)))))

;; use equalp so that: CMD = cmd
(defun get-command-handler (cmd)
  "Take a string denoting a user command, return the handler function."
  ;; equalp or string-equal so we can test strings regardless of the case.
  (let ((cmd-pair (assoc cmd *user-commands* :test #'equalp)))
    (if cmd-pair
        (cdr cmd-pair)
        nil)))

(defun current-nickname ()
  "Nickname of the current connection."
  (girc:nickname (girc:connection (girc:current-buffer))))

(defun current-nickname-p (nick)
  (string-equal nick (girc:nickname (girc:connection (girc:current-buffer)))))

(defun current-target ()
  "Target (channel or nickname) of the current buffer."
  (girc:target (girc:current-buffer)))

(defun current-target-p (target)
  (string-equal target (current-target)))

(defun current-connection ()
  "Connection associated with the current buffer."
  (girc:connection (girc:current-buffer)))

(defun current-server-name ()
  (girc:name (current-connection)))

;; Special read-only variables to abbreviate otherwise verbose command code.
;; They return strings and correspond to epic special variables or expandos.
;; https://epicsol.org/special_vars

(define-symbol-macro $N (current-nickname))
(define-symbol-macro $S (current-server-name))
(define-symbol-macro $T (current-target))

(bind-command logo 'girc:display-logo)

;; eval the lisp form given on the command line and print the _return_ in the buffer
(define-command lisp (&rest args)
  (girc:echo t (eval (read-from-string (car args)))))

#|
/channel load
/channel load #lisp
/channel load *lisp*,>10,<20
/channel list
/channel list 20
|#
(define-command channel (cmd &rest args)
  (alexandria:switch (cmd :test #'string=)
    ("load"
     ;; load the channels from the server into a local list
     ;; args is one single comma-separated list of filters: *m*,>10,<20
     (irc:list t (car args)))
    ("list"
     ;; display the n (default 10) largest channels to the server buffer
     (if (current-connection)
         (with-slots ((chans girc:rpl-list-channels)) (current-connection)
           (if chans
               (let ((buf (girc:find-buffer $S)))
                 (loop for chan in chans
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
(defcmd buffer (cmd &optional arg0 arg1)
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
     (girc:print-buffer-list))
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
              $T
              (girc:channelp $T))
         (let ((chan (find $T
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
(define-command info (arg)
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
(defcmd server (cmd name host &rest args &key &allow-other-keys)
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
(defcmd connect (name &optional nick)
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
              (girc:echo t "-!- Server already connected: " $S)
              (progn
                (girc:connect (girc:connection (girc:current-buffer)))))
          (girc:echo t "-!- Buffer not associated with a connection."))))

;; /exit
(defcmd exit ()
  (crt:exit-event-loop (girc:input-field girc:*ui*) nil))

;; /join #channel
;; /join #chan1,#chan2

;;; TODO 231001 /join #c1,#c2 #k1,#k2

(defcmd join (channel)
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
(defun display-send-msg (target text &optional show-target-p)
  (if show-target-p
      (if (girc:channelp target)
          (girc:display t "<~A@~A> ~A" $N target (car text))
          (girc:display t "*~A@~A* ~A" $N target (car text)))
      (if (girc:channelp target)
          (girc:display t "<~A> ~A" $N (car text))
          (girc:display t "*~A* ~A" $N (car text))))
  (irc:privmsg t target (car text)))

(defcmd msg (target &rest text)
  (typecase (girc:current-buffer)
    (girc:connection-buffer
     (if (current-connection)
         (if (girc:connectedp (current-connection))
             (display-send-msg target text t)
             (girc:echo t "-!- Server not connected:" $S))
         (girc:echo t "-!- Buffer not associated with a server.")))
    (girc:target-buffer
     (display-send-msg target text (not (current-target-p target))))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

;; /query
;; /query <nick>
(defcmd query (nick)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if (current-connection)
         (if nick
             (if (current-nickname-p nick)
                 (girc:display t "-!- ~A is your own nickname." nick)
                 (if (and nick
                          (girc:find-buffer $S nick))
                     (girc:echo t "-!- Buffer already exists:" $S nick)
                     (typecase (girc:current-buffer)
                       (girc:target-buffer
                        (if (null $T)
                            (setf (girc:target (girc:current-buffer)) nick)
                            (girc:add-select-target-buffer $S nick))
                        (girc:echo t "*** Starting a query with" nick))
                       (girc:connection-buffer
                        (girc:add-select-target-buffer $S nick)
                        (girc:echo t "*** Starting a query with" nick)))))
             (if (typep (girc:current-buffer) 'girc:target-buffer)
                 (if (and $T
                          (not (girc:channelp $T)))
                     (let* ((tgt $T)
                            (buf (girc:find-buffer $S tgt)))
                       (girc:echo buf "*** Ending the query with" tgt)
                       (setf (girc:target (girc:current-buffer)) nil))
                     (girc:echo t "-!- Current target is not a query."))
                 (girc:echo t "-!- Server buffer does not have a target.")))
         (girc:echo t "-!- Buffer not associated with a connection.")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /ctcp #testus ACTION tests this command.
;; * haoms tests this command.
(defcmd ctcp (target command &rest args)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (irc:ctcp t target command (car args)))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

(defcmd action (target &rest text)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if text
         (progn
           (irc:ctcp t target "ACTION" (car text))
           (girc:display t "* ~A ~A" $N (car text)))
         (girc:display t "-!- CTCP ACTION requires a text argument.")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

(define-command me (&rest text)
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if $T
         (if text
             (action $T (car text))
             (girc:echo t "-!- Required argument: /me <text>"))
         (girc:echo t "-!- No target associated with the curent buffer.")))
    (girc:connection-buffer
     (girc:echo t "-!- No target associated with a server buffer."))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /nick new-nick
;; The changes to the client settings happen when the server replies,
;; because it is possible that we cant use the new nick.
(defcmd nick (new-nick)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if new-nick
         (irc:nick t new-nick)
         (girc:echo t "-!- Required argument: /nick <new-nick>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a connection."))))

;; /say hello there dear john
(defcmd say (&rest text)
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if (current-connection)
         (if (girc:connectedp (current-connection))
             (if $T
                 (display-send-msg $T text)
                 (girc:echo t "-!- Buffer not associated with a target."))
             (girc:echo t "-!- Server not connected:" $S))
         (girc:echo t "-!- Buffer not associated with a connection.")))
    (girc:connection-buffer
     (girc:echo t "-!- Server buffer not associated with a target."))
    (girc:buffer
     (girc:echo t "-!- Main buffer not connected to a server."))))

;; /part
;; /part #channel
(defcmd part (&optional channel)
  "Leave the given channel or the current channel, if no channel is given."
  (typecase (girc:current-buffer)
    (girc:target-buffer
     (if channel
         (progn
           ;; remove the channel from the channel list of the connection
           (girc:remove-channel channel (girc:connection (girc:current-buffer)))
           (irc:part t channel))
         ;; if no channel was given, but the current buffer has a target
         (if $T
             ;; recursively leave the current target
             (part $T)
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
(defcmd quit (&rest message)
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
(defcmd quote (&rest args)
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
(define-command whois (&rest args)
  (typecase (girc:current-buffer)
    ((or girc:connection-buffer
         girc:target-buffer)
     (if args
         (irc:whois t (car args))
         (girc:echo t "-!- Required argument: /whois <nick>")))
    (girc:buffer
     (girc:echo t "-!- Buffer not associated with a server."))))

(define-command show (name)
  "Show the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (girc:show-buffer-line t))
    ("buffer-column"
     (girc:show-buffer-column t))
    ("topic"
     (girc:show-topic-line t))))

(define-command hide (name)
  "Hide the ui element given by its name."
  (alexandria:switch (name :test #'string=)
    ("buffer-line"
     (girc:show-buffer-line nil))
    ("buffer-column"
     (girc:show-buffer-column nil))
    ("topic"
     (girc:show-topic-line nil))))
