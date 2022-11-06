(in-package :de.anvi.girc)

(defparameter *irc-commands*
  '("ERROR" "JOIN" "KICK" "MODE" "NICK" "NOTICE" "PART" "PING" "PRIVMSG" "QUIT" "TOPIC" "CAP" "AUTHENTICATE")
  "Commands a client can receive from the server.")

;; TODO: (every #'digit-char-p string)
(defun numericp (cmd)
  "If cmd is a three-digit numeric IRC reply, return the cmd as an integer, otherwise return nil."
  (let ((char-list (coerce cmd 'list)))
    (if (and (= (length char-list) 3)
             (digit-char-p (nth 0 char-list))
             (digit-char-p (nth 1 char-list))
             (digit-char-p (nth 2 char-list)))
        t
        nil)))

(defun commandp (cmd)
  "If cmd is a valid IRC command, return t."
  (if (member (string-upcase cmd) *irc-commands* :test #'string=)
      t
      nil))

(defun numeric-to-keyword (cmd)
  "Take the parsed integer of a numeric reply, lookup its keyword name.

If the keyword is unknown, return the integer."
  (let* ((numeric (values (parse-integer cmd)))
         (key (cdr (assoc numeric *numerics*))))
    (if key key numeric)))

(defun command-to-keyword (cmd)
  "Return a keyword symbol representing a valid non-numeric irc command."
  (values (intern (string-upcase cmd) "KEYWORD")))

(defun get-event-name (cmd)
  "Take a command or numeric string, return a keyword or integer representing the IRC event."
  (cond ((numericp cmd) (numeric-to-keyword cmd))
        ((commandp cmd) (command-to-keyword cmd))
        ;; TODO: is it better to signal an error or to retun nil if the command isnt valid?
        (t (error "identify-event: event ~A not a valid irc numeric or command." cmd))))

;; TODO: this is a global variable at first, should be a server/connection slot later.
;; The user should be able to customize the responses for each server separately.
(defparameter *event-handlers* nil
  "Alist of events (keyword or integer) and handler functions.")

;; (with-msg (command text) msg 'body) =>
;; (LET ((#:G649 MSG))
;;   (SYMBOL-MACROLET ((COMMAND (COMMAND #:G649)) (TEXT (TEXT #:G649)))
;;     'BODY))
(defmacro with-msg (accessors message &body body)
  "Define symbol-macros for given message accessors."
  (let ((msg (gensym)))
    `(let ((,msg ,message))
       (symbol-macrolet ,(mapcar (lambda (acc) `(,acc (,acc ,msg))) accessors)
         ,@body))))

(defmacro define-event (event (msg . slots) &body body)
  "Define a handler function, then bind it to the event.

The function takes the parsed message as a mandatory argument.

If given as additional arguments, slot names are provided in the body
as symbol macros."
  `(setf *event-handlers*
         (acons (if (eq ',event t)
                    t
                    ;; save the symbol as a keyword
                    (intern (symbol-name ',event) "KEYWORD"))
                (lambda (,msg) (with-msg ,slots ,msg ,@body))
                *event-handlers*)))

(defmacro bind-event (event handler-function)
  "Add an irc event and an associated handler function.

An event is a keyword for an irc command, or an integer for the numeric.

The handler function takes four arguments:

a parsed message object, the ui object and the connection object."
  `(setf *event-handlers*
         (acons (if (eq ',event t)
                    t
                    ;; save the symbol as a keyword
                    (intern (symbol-name ',event) "KEYWORD"))
                ,handler-function
                *event-handlers*)))

(defun get-event-handler (event)
  "Take an irc event (keyword or integer), return the associated handler function."
  (let ((event-pair (assoc event *event-handlers*)))
    (if event-pair
        (cdr event-pair)
        nil)))

;; called only from connection.lisp/handle-server-input
(defun handle-message (rawmsg connection)
  (let* ((irc-message (parse-raw-message rawmsg connection))
         (event (get-event-name (command irc-message))))
    (when event
      (let ((handler (get-event-handler event)))
        (if handler
            (funcall handler irc-message)
            ;; default action (print the raw message) when no handler has been defined.
            (let ((default-handler (get-event-handler t)))
              (if default-handler
                  (funcall default-handler irc-message)
                  (error "handle-message: no default handler defined."))))))))

;; here we define the event handler functions.
;; those do not deal with the current connection, but the connection pointer given in the message.

(defun default-event-handler (msg)
  "The default event handler will handle every valid irc event for which no handler has been specified.

For now, the raw irc message will simply be displayed in the output window."
  (echo (buffer msg) (rawmsg msg)))

;; then we add the pre-defined handlers to events.

(bind-event t 'default-event-handler)

;; Syntax: CAP <user> <subcommand> :<args>
;; Syntax: CAP * LS :<caps>
;; Syntax: CAP * ACK :<caps>
;; Comment: Response to CAP LS
;; Example: :calcium.libera.chat CAP * LS :account-notify away-notify chghost extended-join multi-prefix sasl tls ...
;; Example: :osmium.libera.chat CAP haom ACK :sasl
;; Example: :calcium.libera.chat CAP * ACK :sasl
(define-event cap (msg buffer connection params text)
  (destructuring-bind (user subcommand) params
    (alexandria:switch (subcommand :test #'string=)
      ("LS"
       (let ((available (split text)))
         (if (and available
                  (member "sasl" available :test #'string=))
             ;; SASL capability available, proceed with request.
             ;; If enabled, replied with CAP ACK.
             (cap connection "REQ" "sasl")
             (echo buffer "-!- SASL capability not supported by server"))))
      ("ACK"
       (let ((enabled (split text)))
         (if (and enabled
                  (member "sasl" enabled :test #'string=))
             ;; SASL capability enabled, proceed with authentication request.
             ;; If successful, replied with a AUTHENTICATE + prompt for the base64 auth token.
             (authenticate connection "PLAIN")
             (echo buffer "-!- Requested SASL capability not enabled.")))))))

;; Client: AUTHENTICATE PLAIN
;; Server: AUTHENTICATE +
;; The event handler of AUTHENTICATE sends the base64-encoded sasl plain token
;; Client: AUTHENTICATE Kj3TjdlfkjsljLKJWI109u31==
(define-event authenticate (msg buffer connection params)
  (destructuring-bind (arg) params
    ;; The AUTHENTICATE + event prompts for the base64-encoded sasl auth token.
    (when (string= arg "+")
      (with-slots ((nick nickserv-account)
                   (pass nickserv-password)) connection
        (authenticate connection (base64-encode-string (sasl-plain-token nick nick pass)))))))

;; Commend: Response to a nickServ login, whether by msg, pass or sasl.
;; Syntax:  :server 900 <nick> <nick>!<ident>@<host> <account> :You are now logged in as <user>
;; Example: :osmium.libera.chat 900 haom haom!myuser@user/mcparen BigusNickus :You are now logged in as BigusNickus
(define-event rpl-loggedin (msg buffer text)
  (echo buffer "***" text))

;; Comment: If SASL auth is successful, server replies with rpl-saslsuccess (603).
;;          The handler must close the capability negotiation with CAP END.
;; Example: :sodium.libera.chat 903 BigusNickus :SASL authentication successful
(define-event rpl-saslsuccess (msg prefix-nick buffer connection params text)
  (destructuring-bind (nick) params
    (echo buffer "***" text)
    (cap connection "END")))

;; Event:   JOIN
;; Syntax:  :<prefix> JOIN :<channel>
;; Syntax:  :<prefix> JOIN <channel>
;; Comment: The target channel is on some servers param 0, sometimes the text.
;; Example: :haom!~myuser@78-2-83-238.adsl.net.com.com JOIN :#testus
;; Example: :haom!~myuser@78-2-83-238.adsl.net.com.com JOIN #testus
(define-event join (msg connection prefix-nick command params text)
  (let ((channel (cond (text text)
                       (params (nth 0 params)))))
    (let ((buffer (find-buffer channel connection))
          (chan (find-channel channel connection)))
      (add-nick prefix-nick chan)
      (echo buffer "***" prefix-nick "joined" channel))))

;; Syntax: :<prefix> NOTICE <target> :<text>
;; Examples:
;; :kornbluth.freenode.net NOTICE * :*** Looking up your hostname...
;; :kornbluth.freenode.net NOTICE * :*** Checking Ident
;; :kornbluth.freenode.net NOTICE * :*** Found your hostname
(define-event notice (msg prefix buffer text)
  (display buffer "-~A- ~A" prefix text))

;; Syntax: :<prefix> PART <channel> :<reason>
;; Syntax: :<prefix> PART :<chan>
;; Examples:
;; :haom!~myuser@freenode-4bt.6qi.vs9qrf.IP PART :#linux
;; :another!~another@freenode-bn0om7.k2om.k054.fah2pm.IP PART #linux :"So we must part ways"
(define-event part (msg connection prefix-nick command params text)
  (let* ((channel (if params (nth 0 params) text))
         (chan (find-channel channel connection))
         (reason (if (and params text) text nil))
         ;; if there is a buffer for the channel, send part to that buffer
         (buffer (find-buffer channel connection)))
    (unless (string= prefix-nick (nickname connection))
      (remove-nick prefix-nick chan))
    (if reason
        (display buffer "*** ~A left ~A (~A)" prefix-nick channel reason)
        (echo buffer "***" prefix-nick "left" channel))))

;; Event:   NICK
;; Syntax:  :<prefix> NICK <old-nick> :<new-nick>
;; Example: :_vanessa_!~farawayas@user/farawayastronaut NICK :vaness
;; Example: :ohokthen!~igloo@172.58.165.154 NICK :FarMoreSinister
;; Example: :haom!~myuser@ip-22-111.un55.pool.myip.com NICK :haoms
(define-event nick (msg buffer prefix-nick prefix-user connection text)
  ;; if it is your own nick
  (when (string= prefix-nick (nickname connection))
    ;; update the client
    (setf (nickname connection) text)
    ;; display the change to the server buffer
    (display buffer "*** You are now known as ~A" text)
    ;; update the status line
    (update-status))
  ;; display the change to chans where the nick is present
  (dolist (buffer (crt:items *buffers*))
    (when (and (eq connection (connection buffer))
               (target buffer)
               (member prefix-nick
                       (nicknames (find-channel (target buffer) connection))
                       :test #'string=))
      (remove-nick prefix-nick (find-channel (target buffer) connection))
      (add-nick text (find-channel (target buffer) connection))
      (if text
          (display buffer "*** ~A is now known as ~A" prefix-nick text)
          (echo buffer "***" prefix-nick "is now known as ...")))))

;; Event:   QUIT
;; Syntax:  :<prefix> QUIT :<reason>
;; Comment: Generates an ERROR reply.
;; Example: :haom!~myuser@ip-088-152-010-043.um26.pools.vodafone-ip.de QUIT :Client Quit
(define-event quit (msg prefix-nick connection text)
  (dolist (buffer (crt:items *buffers*))
    (when (and (eq connection (connection buffer))
               (target buffer)
               (member prefix-nick
                       (nicknames (find-channel (target buffer) connection))
                       :test #'string=))
      (remove-nick prefix-nick (find-channel (target buffer) connection))
      (if text
          (display buffer "*** ~A quit (~A)" prefix-nick text)
          (echo buffer "***" prefix-nick "quit")))))

(define-event ping (msg buffer rawmsg connection text)
  (when conf:show-server-ping
    (echo buffer rawmsg)
    (display buffer "PONG :~A" text))
  ;; return a PONG to the server which sent the PING.
  (pong connection text))

;; Syntax:
;; :<prefix> PRIVMSG <target> :<text>
;;
;; Examples:
;; :IdleOne!~idleone@ubuntu/member/idleone PRIVMSG #ubuntu :The_BROS: not at this time.
;; :leo!~leo@host-205-241-38-153.acelerate.net PRIVMSG #ubuntu :im a newbie
;; :moah!~gnu@dslb.host-ip.net PRIVMSG arrk13 :test back
;; :haom!~myuser@93-142-151-146.adsl.net.com.de PRIVMSG haom :hello there
(define-event privmsg (msg prefix-nick params connection text)
  (destructuring-bind (target) params
    (let ((buffer (find-buffer target connection)))
      ;; have different handlers if the msg target is a channel or nick
      (if (channelp target)
          (display buffer "<~A> ~A" prefix-nick text)
          (display buffer "*~A* ~A" prefix-nick text)))))

(defun display-event-text (msg)
  "Basic event handler to simply display the message text."
  (echo (buffer msg) (text msg)))



;;; Replies 001 to 004 are sent upon a successful registration

;; Number:  001
;; Event:   RPL_WELCOME
;; Syntax:  :<prefix> 001 <nick> :<text>
;; Example: :calcium.libera.chat 001 haom :Welcome to the Libera.Chat Internet Relay Chat Network haom
(bind-event rpl-welcome 'display-event-text)

;; Number:  002
;; Event:   RPL_YOURHOST
;; Syntax:  :<prefix> 002 <nick> :<text>
;; Example: :calcium.libera.chat 002 haom :Your host is calcium.libera.chat[64.86.243.186/6667], running version solanum-1.0-dev
(bind-event rpl-yourhost 'display-event-text)

;; Number:  003
;; Event:   RPL_CREATED
;; Syntax:  :<prefix> 003 <nick> :<text>
;; Example: :calcium.libera.chat 003 haom :This server was created Wed Aug 24 2022 at 00:18:16 UTC
(bind-event rpl-created 'display-event-text)

;; Number:  004
;; Event:   RPL_MYINFO
;; Syntax:  :<prefix> 004 <nick> <server name> <version> <user modes> <channel modes> <channel modes>
;; Example: :calcium.libera.chat 004 haom calcium.libera.chat solanum-1.0-dev DGMQRSZaghilopsuwz CFILMPQSTbcefgijklmnopqrstuvz bkloveqjfI
(define-event rpl-myinfo (msg)
  nil)

;; Number:  005
;; Event:   RPL_ISUPPORT
;; Syntax:  :<prefix> 005 <nick> <params> :<text>
;; Comment: Here the server communicates features to the client
;; Example: :calcium.libera.chat 005 haom ETRACE SAFELIST ELIST=CMNTU MONITOR=100 CALLERID=g FNC WHOX KNOCK CHANTYPES=# EXCEPTS INVEX CHANMODES=eIbq,k,flj,CFLMPQSTcgimnprstuz :are supported by this server
;; Example: :calcium.libera.chat 005 haom CHANLIMIT=#:250 PREFIX=(ov)@+ MAXLIST=bqeI:100 MODES=4 NETWORK=Libera.Chat STATUSMSG=@+ CASEMAPPING=rfc1459 NICKLEN=16 MAXNICKLEN=16 CHANNELLEN=50 TOPICLEN=390 DEAF=D :are supported by this server
;; Example: :calcium.libera.chat 005 haom TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,PRIVMSG:4,NOTICE:4,ACCEPT:,MONITOR: EXTBAN=$,ajrxz :are supported by this server
(define-event rpl-isupport (msg)
  nil)


;;; Replies sent upon a successful login on a server

;; Number:  251
;; Event:   RPL_LUSERCLIENT
;; Syntax:  :<prefix> 251 <nick> :<text>
;; Text:    :There are <int> users and <int> invisible on <int> servers
;; Example: :tantalum.libera.chat 251 haom :There are 71 users and 44327 invisible on 27 servers
(bind-event rpl-luserclient 'display-event-text)

;; Number:  252
;; Event:   RPL_LUSEROP
;; Syntax:  :<prefix> 252 <int> :<text>
;; Comment: Number of IRC operators online
;; Example: :tantalum.libera.chat 252 haom 36 :IRC Operators online
(define-event rpl-luserop (msg)
  nil)

;; Number:  253
;; Event:   RPL_LUSERUNKNOWN
;; Syntax:  :<prefix> 253 <nick> <int> :<text>
;; Comment: Number of connections in an unknown or unregistered state
;; Example: :tantalum.libera.chat 253 haom 14 :unknown connection(s)
(define-event rpl-luserunknown (msg)
  nil)

;; Number:  254
;; Event:   RPL_LUSERCHANNELS
;; Example: :tantalum.libera.chat 254 haom 22210 :channels formed
(define-event rpl-luserchannels (msg)
  nil)

;; Number:  255
;; Event:   RPL_LUSERME
;; Example: :tantalum.libera.chat 255 haom :I have 2800 clients and 1 servers
(define-event rpl-luserme (msg)
  nil)

;; Number:  265
;; Event:   RPL_LOCALUSERS
;; Example: :tantalum.libera.chat 265 haom 2800 4413 :Current local users 2800, max 4413
(define-event rpl-localusers (msg)
  nil)

;; Number:  266
;; Event:   RPL_GLOBALUSERS
;; Example: :tantalum.libera.chat 266 haom 44398 52334 :Current global users 44398, max 52334
(bind-event rpl-globalusers 'display-event-text)

;; Number:  250
;; Event:   RPL_STATSCONN
;; Example: :tantalum.libera.chat 250 haom :Highest connection count: 4414 (4413 clients) (289303 connections received)
(bind-event rpl-statsconn 'display-event-text)


;;; Replies sent as response to a command

;; Number:  375
;; Event:   RPL_MOTDSTART
;; Syntax:  :<prefix> 375 <nick> :<text>
;; Comment: Start of a MOTD response
;; Example: :erbium.libera.chat 375 haom :- erbium.libera.chat Message of the Day -
(define-event rpl-motdstart (msg buffer text)
  (echo buffer "--- Start of /MOTD.")
  (echo buffer text))

;; Number:  372
;; Event:   RPL_MOTD
;; Syntax:  :<prefix> 372 <nick> :<text>
;; Comment: MOTD file displayed line by line
;; Example: :kornbluth.freenode.net 372 haom :- Thank you for using freenode!
(define-event rpl-motd (msg buffer text)
  (echo buffer text))

;; Number:  376
;; Event:   RPL_ENDOFMOTD
;; Syntax:  :<prefix> 376 <nick> :<text>
;; Comment: End of a MOTD response
;; Example: :kornbluth.freenode.net 376 haom :End of /MOTD command.
(define-event rpl-endofmotd (msg buffer text)
  (echo buffer "---" text))

;; Number:
;; Event:
;; Syntax:  
;; Comment: 
;; Example: 

;;; ERROR

;; Comment: Reply to QUIT
;; Example: ERROR :Closing Link: 78-2-83-238.adsl.net.com.com (Quit: haom)

(define-event error (msg buffer connection text)
  (display buffer "ERROR: ~A" text)
  (disconnect connection)
  (update-status))


;;; LIST

;; list #irc30

;; :irc.efnet.nl 321 haom Channel :Users  Name
;; :irc.efnet.nl 322 haom #IRC30 258 :Call your Mom :)
;; :irc.efnet.nl 323 haom :End of /LIST


;;; NAMES

;; :irc.efnet.nl 353 haom = #test :@haom
;; :irc.efnet.nl 366 haom #test :End of /NAMES list.

;; Number:  353
;; Event:   RPL_NAMREPLY
;; Syntax:  :<prefix> 353 <nick> = <channel> :<nicks>
;; Example: :irc.efnet.nl 353 haom = #test :@haom
(define-event rpl-namreply (msg connection params text)
  (destructuring-bind (nick _ channel) params
    (when text
      (let ((chan (find-channel channel connection)))
        (when chan
          (with-slots (nicknames rpl-namreply-started-p) chan
            (unless rpl-namreply-started-p
              ;; set the start flag to t, will be set to finished in 366
              (setf rpl-namreply-started-p t
                    nicknames nil))
            ;; push the nicks to the nicknames slot
            (dolist (nick (split text))
              (add-nick nick chan))))))))

;; Number:  366
;; Event:   RPL_ENDOFNAMES
;; Syntax:  :<prefix> 366 <nick> <channel> :<info>
;; Example: :irc.efnet.nl 366 haom #test :End of /NAMES list.
(define-event rpl-endofnames (msg connection params)
  (destructuring-bind (nick channel) params
    (let ((chan (find-channel channel connection)))
      (when chan
        ;; set the flag back to finished, was set to started in 353
        (setf (slot-value chan 'rpl-namreply-started-p) nil)))))

;;; MODE

;; :haom MODE haom :+Ziw
;; :irc.efnet.nl MODE #test +nt
;; :irc.efnet.nl 324 haom #test +tn
;; :irc.efnet.nl 329 haom #test 1598613944


;;; TOPIC

;; Number:   332
;; Event:    RPL_TOPIC
;; Reply to: JOIN, TOPIC
;; Syntax:   :<prefix> 332 <client> <channel> :<topic>
;; Example:  :kornbluth.freenode.net 332 McParen #ubuntu :Official Ubuntu Support Channel
(define-event rpl-topic (msg connection params text)
  (destructuring-bind (client channel) params
    (let ((buffer (find-buffer channel connection)))
      (when text
        (display buffer "TOPIC for ~A: ~A" channel text)
        (let ((chan (find-channel channel connection)))
          (when chan
            (setf (slot-value chan 'topic) text)
            (update-topic)))))))

;; Number:   333
;; Event:    RPL_TOPICWHOTIME
;; Reply to: JOIN, TOPIC
;;
;; Syntax1:  :<prefix> 333 <client> <channel> <nick> <setat>
;; Examples:
;; :kornbluth.freenode.net 333 McParen #ubuntu el!~el@ubuntu/member/el 1595009905
;; :karatkievich.freenode.net 333 McParen #kde Mamarok 1603383031
;;
;; Syntax2:  :<prefix> 333 <client> <channel> <nick> :<setat>
;; Examples:
;; :lux.freenode.net 333 haom #linux oxek :1623786160
(define-event rpl-topicwhotime (msg connection params text)
  (cond ((= (length params) 4)
         (destructuring-bind (client channel nick setat) params
           (let ((buffer (find-buffer channel connection)))
             (display buffer "TOPIC set by ~A on ~A." nick setat))))
        ((= (length params) 3)
         (destructuring-bind (client channel nick) params
           (let ((buffer (find-buffer channel connection)))
             (display buffer "TOPIC set by ~A on ~A." nick text))))
        (t
         (echo (buffer msg) (rawmsg msg)))))

;;; WHOIS

;; :irc.efnet.nl 311 haom haom ~myuser hostname.de * :Realname
;; :irc.efnet.nl 312 haom haom irc.efnet.nl :>> Hax Pax Max Deus Adimax <<
;; :irc.efnet.nl 313 haom tau :is een Bediener der IRC (IRC Operator)
;; :irc.efnet.nl 317 haom haom 72 1589142682 :seconds idle, signon time
;; :irc.efnet.nl 318 haom haom :End of /WHOIS list.
;; :irc.efnet.nl 319 haom ninex :#irc.awknet.ca @#proxybl @#irccloud #try2hack #port80 @#netadmins @+#eris.Berkeley.EDU #efnet @+#asciiart +#IRC30 #2600
;; :verne.freenode.net 330 haom Joa Joa :is logged in as
;; :irc.efnet.nl 338 haom haom 52.71.6.45 :actually using host
;; :weber.freenode.net 378 haom haom :is connecting from *@93-137-20-95.adsl.net.com.com 155.17.21.5
;; :irc.mzima.net 671 haom neonjesus :is using a secure connection

;; Number:  311
;; Event:   RPL_WHOISUSER
;; Syntax:  :<prefix> 311 <nick> <target nick> <user> <host> * :<real name>
;; Comment: Reply to WHOIS - Information about the user
;; Example: :calvino.freenode.net 311 arrk23 arrk23 ~arrakis24 hostname-ip.net * :McLachlan
(define-event rpl-whoisuser (msg buffer params text)
  (destructuring-bind (nick target user host star) params
    (echo buffer "--- Start of /WHOIS list.")
    (echo buffer "Nick:" target)
    (echo buffer "User:" user)
    (echo buffer "Host:" host)
    (echo buffer "Real:" text)))

;; Number:  312
;; Event:   RPL_WHOISSERVER
;; Syntax:  :<prefix> 312 <nick> <target nick> <server> :<server location>
;; Comment: Reply to WHOIS - What server the user is on
;; Example: :calvino.freenode.net 312 arrk23 arrk23 calvino.freenode.net :Milan, IT
(define-event rpl-whoisserver (msg buffer params text)
  (destructuring-bind (nick target server) params
    (echo buffer "Server:" server)
    (echo buffer "Location:" text)))

;; Number:  317
;; Event:   RPL_WHOISIDLE
;; Syntax:  :<prefix> 317 <nick> <target nick> <idle time in seconds> <signon time in seconds> :<text>
;; Comment: Reply to WHOIS - Idle information
;; Example: :sendak.freenode.net 317 arrk23 arrk23 32 1316203528 :seconds idle, signon time
;; Example: :irc.efnet.nl 317 haom haom 72 1589142682 :seconds idle, signon time
(define-event rpl-whoisidle (msg buffer params text)
  (destructuring-bind (nick target seconds-idle signon-time) params
    (display buffer "~A: ~A ~A" text seconds-idle signon-time)))

;; Number:  318
;; Event:   RPL_ENDOFWHOIS
;; Syntax:  :<prefix> 318 <nick> <target nick> :<info>
;; Comment: Reply to WHOIS - End of list
;; Example: :calvino.freenode.net 318 arrk23 arrk23 :End of /WHOIS list.
(define-event rpl-endofwhois (msg buffer text)
  (echo buffer "---" text))

;; Number:  319
;; Event:   RPL_WHOISCHANNELS
;; Syntax:  :<prefix> 318 <nick> <target nick> :{[@|+]<channel><space>}
;; Comment: Reply to WHOIS - Channel list for user
;; Example: :verne.freenode.net 319 McParen McParen :#python
(define-event rpl-whoischannels (msg buffer text)
  (echo buffer "Channels: " text))

;; Number:  330
;; Event:   RPL_WHOISACCOUNT
;; Syntax:  :<prefix> 330 <nick> <target nick> <target authname> :<info>
;; Example: :verne.freenode.net 330 haom Joa Joa :is logged in as
(define-event rpl-whoisaccount (msg buffer params text)
  (destructuring-bind (nick target authname) params
    (echo buffer target text authname)))

;; Number:  338
;; Event:   RPL_WHOISACTUALLY
;; Syntax:  :<prefix> 338 <nick> <target nick> <host ip> :<text>
;; Example: :irc.efnet.nl 338 haom haom 92.73.73.213 :actually using host
(define-event rpl-whoisactually (msg buffer params text)
  (destructuring-bind (nick target host-ip) params
    (display buffer "~A: ~A" text host-ip)))

;; Number:  378
;; Event:   RPL_WHOISHOST
;; Syntax:  :<prefix> 378 <nick> <target> :<info>
;; Example: :weber.freenode.net 378 haom haom :is connecting from *@93-137-20-95.adsl.net.com.com 92.111.22.5
(define-event rpl-whoishost (msg buffer text)
  (echo buffer text))

;; Number:  671
;; Event:   RPL_WHOISSECURE
;; Syntax:  :<prefix> 671 <client> <nick> :is using a secure connection
;; Example: :irc.mzima.net 671 haom neonjesus :is using a secure connection
(define-event rpl-whoissecure (msg buffer params text)
  (destructuring-bind (client nick) params
    (echo buffer nick text)))
