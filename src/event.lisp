(in-package :de.anvi.girc)

(defparameter *irc-commands*
  '("ERROR" "JOIN" "KICK" "MODE" "NICK" "NOTICE" "PART" "PING" "PRIVMSG" "QUIT" "TOPIC")
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

;; Syntax:
;; :<prefix> JOIN :<channel>
;; :<prefix> JOIN <channel>
;; Comment: The target channel is on some servers param 0, sometimes the text.
;; Examples:
;; :haom!~myuser@78-2-83-238.adsl.net.com.com JOIN :#testus
;; :haom!~myuser@78-2-83-238.adsl.net.com.com JOIN #testus
(define-event join (msg buffer prefix-nick command params text)
  (let ((channel (cond (text text)
                       (params (nth 0 params)))))

    ;; when your nick joins the channel, make the channel the target of the buffer.
    ;;(when (string= prefix-nick
    ;;               (connection-nickname (buffer-connection *current-buffer*)))
    ;;  (setf (buffer-target *current-buffer*) channel)
    ;;  (update-status))

    (echo buffer command prefix-nick channel)))

;; Syntax: :<prefix> NOTICE <target> :<text>
;; Examples:
;; :kornbluth.freenode.net NOTICE * :*** Looking up your hostname...
;; :kornbluth.freenode.net NOTICE * :*** Checking Ident
;; :kornbluth.freenode.net NOTICE * :*** Found your hostname
(define-event notice (msg buffer command text)
  (display buffer "~A: ~A" command text))

;; Syntax: :<prefix> PART <channel> :<reason>
;; Syntax: :<prefix> PART :<chan>
;; Examples:
;; :haom!~myuser@freenode-4bt.6qi.vs9qrf.IP PART :#linux
;; :another!~another@freenode-bn0om7.k2om.k054.fah2pm.IP PART #linux :"So we must part ways"
(define-event part (msg buffer prefix-nick command params text)
  (let ((channel (if params (nth 0 params) text))
        (reason (if (and params text) text nil)))

    ;; when your nick parts the channel and the channel is the current buffers target, remove it from the target.
    ;;(when (and (string= prefix-nick
    ;;                    (connection-nickname (buffer-connection *current-buffer*)))
    ;;           (string= channel
    ;;                    (buffer-target *current-buffer*)))
    ;;  (setf (buffer-target *current-buffer*) nil)
    ;;  (update-status))

    (if reason
        (echo buffer command prefix-nick channel reason)
        (echo buffer command prefix-nick channel))))

(define-event quit (msg buffer prefix-nick command text)
  (echo buffer command prefix-nick text))

(define-event ping (msg buffer rawmsg connection text)
  (when *show-server-ping*
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
    (let ((buffer (find-buffer connection target)))
      ;; have different handlers if the msg target is a channel or nick
      (if (channelp target)
          (display buffer "<~A> ~A" prefix-nick text)
          (display buffer "*~A* ~A" prefix-nick text)))))

;; :kornbluth.freenode.net 372 haom :- Thank you for using freenode!
;; :kornbluth.freenode.net 376 haom :End of /MOTD command.
(define-event rpl-motd (msg buffer text)
  (echo buffer text))

(define-event rpl-endofmotd (msg buffer text)
  (echo buffer text))

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

;;; JOIN

;; :haom!~myuser@78-2-83-238.adsl.net.com.com JOIN :#test

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
(define-event rpl-namreply (msg)
  nil)

;; Number:  366
;; Event:   RPL_ENDOFNAMES
;; Syntax:  :<prefix> 366 <nick> <channel> :<info>
;; Example: :irc.efnet.nl 366 haom #test :End of /NAMES list.
(define-event rpl-endofnames (msg)
  nil)

;;; MODE

;; :irc.efnet.nl MODE #test +nt
;; :irc.efnet.nl 324 haom #test +tn
;; :irc.efnet.nl 329 haom #test 1598613944

;;; TOPIC

;; Number:   332
;; Event:    RPL_TOPIC
;; Reply to: JOIN, TOPIC
;; Syntax:   :<prefix> 332 <client> <channel> :<topic>
;; Example:  :kornbluth.freenode.net 332 McParen #ubuntu :Official Ubuntu Support Channel
(define-event rpl-topic (msg buffer params text)
  (destructuring-bind (client channel) params
    (display buffer "TOPIC for ~A: ~A" channel text)))

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
(define-event rpl-topicwhotime (msg buffer params text)
  (cond ((= (length params) 4)
         (destructuring-bind (client channel nick setat) params
           (display buffer "TOPIC set by ~A on ~A." nick setat)))
        ((= (length params) 3)
         (destructuring-bind (client channel nick) params
           (display buffer "TOPIC set by ~A on ~A." nick text)))
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
    (display buffer "-- Start of /WHOIS list.")
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
  (display buffer "-- ~A" text))

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
