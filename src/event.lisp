
(in-package :de.anvi.girc)

;; TODO: what about user commands like USER, WHOIS?
(defparameter *valid-irc-commands*
  '("ERROR" "JOIN" "KICK" "MODE" "NICK" "NOTICE" "PART" "PING" "PRIVMSG" "QUIT" "TOPIC"))

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
  (if (member (string-upcase cmd) *valid-irc-commands* :test #'string=)
      t
      nil))

(defun numeric-to-integer (cmd)
  ;; force only one return value.
  (values (parse-integer cmd)))

(defun command-to-keyword (cmd)
  "Return a keyword symbol representing a valid non-numeric irc command."
  (values (intern (string-upcase cmd) "KEYWORD")))

;; TODO: we want users to be able to add their own event handlers using both numerics or :RPL_ keywords
;; translate :RPL_ to a numeric before adding the event handler.
(defun validate (cmd)
  "Take a command or numeric string, return a keyword or integer representing the IRC event."
  (cond ((numericp cmd) (numeric-to-integer cmd))
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
(defmacro with-msg (slots message &body body)
  "Macro like with-slots but for accessors."
  (let ((msg (gensym)))
    `(let ((,msg ,message))
       (symbol-macrolet ,(mapcar (lambda (slot) `(,slot (,slot ,msg))) slots)
         ,@body))))

(defmacro define-event (event (msg . slots) &body body)
  "Define a handler function, then bind it to the event.

The function takes the parsed message as a mandatory argument.

If given as additional arguments, slot names are provided in the body
as symbol macros."
  `(setf *event-handlers*
         (acons ,event
                (lambda (,msg) (with-msg ,slots ,msg ,@body))
                *event-handlers*)))

;; TODO 200328 allow event to be a list of events.
(defmacro bind-event (event handler-function)
  "Add an irc event and an associated handler function.

An event is a keyword for an irc command, or an integer for the numeric.

The handler function takes four arguments:

a parsed message object, the ui object and the connection object."
  `(setf *event-handlers*
         (acons ,event ,handler-function *event-handlers*)))

(defun get-event-handler (event)
  "Take an irc event (keyword or integer), return the associated handler function."
  (let ((event-pair (assoc event *event-handlers*)))
    (if event-pair
        (cdr event-pair)
        nil)))

;; called only from girc.lisp/process-server-input
(defun handle-message (rawmsg connection)
  (let* ((irc-message (parse-raw-message rawmsg connection))
         (event (validate (command irc-message)))) ; return keyword or integer
    ;; TODO: does validate ever return nil?
    ;; if not, we dont have to check, an irc message without an event/command cant exist
    (when event
      (let ((handler (get-event-handler event)))
        (if handler
            (funcall handler irc-message)
            ;; default action (simply print event) when no event handler has been defined.
            ;; that means that all validated events will be handled.
            (funcall (get-event-handler t) irc-message))))))

;; here we define the event handler functions.
;; those do not deal with the current connection, but the connection pointer given in the message.

(defun default-event-handler (msg)
  "The default event handler will handle every valid irc event for which no handler has been specified.

For now, the raw irc message will simply be displayed in the output window."
  (display "~A~%" (raw-message msg)))

;; then we add the pre-defined handlers to events.

(bind-event t 'default-event-handler)

;;;

(define-event :ping (msg raw-message connection text)
  (display "~A~%" raw-message)
  (display "PONG :~A~%" text)
  ;; return a PONG to the server which sent the PING.
  (pong connection text))

;; Syntax:
;; :<prefix> PRIVMSG <target> :<text>
;;
;; Examples:
;; :IdleOne!~idleone@ubuntu/member/idleone PRIVMSG #ubuntu :The_BROS: not at this time.
;; :leo!~leo@host-205-241-38-153.acelerate.net PRIVMSG #ubuntu :im a newbie
;; :moah!~gnu@dslb.host-ip.net PRIVMSG arrk13 :test back
;; :haom!~myuser@93-142-151-146.adsl.net.t-com.de PRIVMSG haom :hello there
(define-event :privmsg (msg prefix-nick params text)
  (display "~A @ ~A: ~A~%" prefix-nick (nth 0 params) text))

;; Syntax:
;; :<prefix> NOTICE <target> :<text>
;;
;; Examples:
;; :kornbluth.freenode.net NOTICE * :*** Looking up your hostname...
;; :kornbluth.freenode.net NOTICE * :*** Checking Ident
;; :kornbluth.freenode.net NOTICE * :*** Found your hostname
(define-event :notice (msg text)
  (display "NOTICE: ~A~%" text))

;; :kornbluth.freenode.net 372 haom :- Thank you for using freenode!
;; :kornbluth.freenode.net 376 haom :End of /MOTD command.
(define-event 372 (msg text)
  (display "~A~%" text))

(define-event 376 (msg text)
  (display "~A~%" text))

;;; LIST

;; list #irc30

;; :irc.efnet.nl 321 haom Channel :Users  Name
;; :irc.efnet.nl 322 haom #IRC30 258 :Call your Mom :)
;; :irc.efnet.nl 323 haom :End of /LIST

;;; NAMES

;;; WHOIS

;; :irc.efnet.nl 311 haom haom ~myuser hostname.de * :Realname
;; :irc.efnet.nl 319 haom ninex :#irc.awknet.ca @#proxybl @#irccloud #try2hack #port80 @#netadmins @+#eris.Berkeley.EDU #efnet @+#asciiart +#IRC30 #2600
;; :irc.efnet.nl 312 haom haom irc.efnet.nl :>> Hax Pax Max Deus Adimax <<
;; :irc.efnet.nl 313 haom tau :is een Bediener der IRC (IRC Operator)
;; :irc.efnet.nl 338 haom haom 52.71.6.45 :actually using host
;; :irc.efnet.nl 317 haom haom 72 1589142682 :seconds idle, signon time
;; :irc.efnet.nl 318 haom haom :End of /WHOIS list.
;; :irc.mzima.net 671 haom neonjesus :is using a secure connection

;; Number:  311
;; Event:   RPL_WHOISUSER
;; Syntax:  311 <nick> <target nick> <user> <host> * :<real name>
;; Comment: Reply to WHOIS - Information about the user 
;; Example: :calvino.freenode.net 311 arrk23 arrk23 ~arrakis24 hostname-ip.net * :McLachlan
(define-event 311 (msg params text)
  (destructuring-bind (nick target user host star) params
    (display "~%-- Start of /WHOIS list.~%")
    (echo "Nick: " nick)
    (echo "User: " user)
    (echo "Host: " host)
    (echo "Real: " text)))

;; Number:  312
;; Event:   RPL_WHOISSERVER
;; Syntax:  312 <nick> <target nick> <server> :<server location>
;; Comment: Reply to WHOIS - What server the user is on
;; Example: :calvino.freenode.net 312 arrk23 arrk23 calvino.freenode.net :Milan, IT
(define-event 312 (msg params text)
  (echo "Server: " (nth 2 params))
  (echo "Message: " text))

;; Number:  338
;; Event:   RPL_WHOISACTUALLY 
;; Example: :irc.efnet.nl 338 haom haom 92.73.73.213 :actually using host
(define-event 338 (msg params text)
  (display "~A: ~A~%" text (nth 2 params)))

;; Number:  317
;; Event:   RPL_WHOISIDLE
;; Syntax:  317 <nick> <target nick> <idle time in seconds> <signon time in seconds> :<text>
;; Comment: Reply to WHOIS - Idle information
;; Example: :sendak.freenode.net 317 arrk23 arrk23 32 1316203528 :seconds idle, signon time
;; Example: :irc.efnet.nl 317 haom haom 72 1589142682 :seconds idle, signon time
(define-event 317 (msg params text)
  (destructuring-bind (nick target seconds-idle signon-time) params
    (display "~A: ~A ~A~%" text seconds-idle signon-time)))

;; Number:  318
;; Event:   RPL_ENDOFWHOIS
;; Syntax:  318 <nick> <target nick> :<info>
;; Comment: Reply to WHOIS - End of list
;; Example: :calvino.freenode.net 318 arrk23 arrk23 :End of /WHOIS list.
(define-event 318 (msg text)
  (display "-- ~A~%~%" text))
