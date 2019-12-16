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
  "If cmd is a valid IRC command, return a keyword representing it, otherwise return nil."
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

(defmacro define-handler (event handler-function)
  "Add an irc event and an associated handler function.

An event is a keyword for an irc command, or an integer for the numeric.

The handler function takes four arguments:

a parsed message object, the ui object and the connection object."
  `(setf *event-handlers*
         (acons ,event ,handler-function *event-handlers*)))

(defun get-handler (event)
  "Take an irc event (keyword or integer), return the associated handler function."
  (let ((event-pair (assoc event *event-handlers*)))
    (if event-pair
        (cdr event-pair)
        nil)))

(defun handle-irc-message (ircmsg ui con)
  (let* ((message (parse ircmsg))
         (event (validate (command message)))) ; return key or integer
    ;; TODO: does validate ever return nil?
    ;; if not, we dont have to check, an irc message without an event/command cant exist
    (when event
      (let ((handler (get-handler event)))
        (if handler
            (funcall handler message ui con)
            ;; default action (simply print event) when no event handler has been defined.
            ;; that means that all validated events will be handled.
            (funcall (get-handler t) message ui con))))))

;; here we first define several event handler functions.

(defun default-event-handler (msg ui con)
  "The default event handler will handle every valid irc event for which no handler has been specified.

For now, the raw irc message will simply be displayed in the output window."
  (declare (ignore con))
  (format (output-window ui) "~A~%" (ircmsg msg)))

(defun ping-handler (msg ui con)
  (format (output-window ui) "~A~%" (ircmsg msg))
  (format (output-window ui) "PONG :~A~%" (text msg))
  (send con "PONG :~A" (text msg)))

;; Syntax: :<prefix> PRIVMSG <target> :<text>
;; Examples:
;; :IdleOne!~idleone@ubuntu/member/idleone PRIVMSG #ubuntu :The_BROS: not at this time.
;; :leo!~leo@host-205-241-38-153.acelerate.net PRIVMSG #ubuntu :im a newbie
;; :moah!~gnu@dslb-092-073-066-073.pools.arcor-ip.net PRIVMSG arrk13 :test back
;; :haom!~myuser@93-142-151-146.adsl.net.t-com.hr PRIVMSG haom :hello there

(defun privmsg-handler (msg ui con)
  (declare (ignore con))
  (format (output-window ui)
          "~A ~A: ~A~%"
          (prefix-nick msg)
          (nth 0 (params msg))
          (text msg)))

;; then we add the handlers to events.

(define-handler t 'default-event-handler)
(define-handler :ping 'ping-handler)
(define-handler :privmsg 'privmsg-handler)