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
        (t (error "identify-event: event ~A not a valid irc numeric or command." cmd))))

;; TODO: this is a global variable at first, should be a server/connection slot later.
;; The user should be able to customize the responses for each server separately.
(defparameter *event-handlers* nil
  "Alist of events (keyword or integer) and handler functions.")

;; TODO: we want more than one event handler per event.
;; the handler should be like an emacs hook, a list of handler functions.
;; so add-event-handler should not overwrite an entry, but add to it.
;; but then, how do we remove a handler from the list???
;; do we have to add sub-ID's to handlers, like epic does with numbers?
;; TODO: define a define-command function to deal with user commands.
;; irc messages received from a server are "events", user input is a "command".
(defmacro define-handler (event handler-function)
  "Add an irc event and an associated handler function.

An event is a keyword for an irc command, or an integer for the numeric.

The handler function takes four arguments:

- a window for output
- a parsed message object
- the connected server stream"
  `(setf *event-handlers*
         (acons ,event ,handler-function *event-handlers*)))

(defun get-handler (event)
  "Take an irc event (keyword or integer), return the associated handler function."
  (let ((event-pair (assoc event *event-handlers*)))
    (if event-pair
        (cdr event-pair)
        nil)))

(defun handle-irc-message (ircmsg wout stream)
  (let* ((message (parse ircmsg))
         (event (validate (command message)))) ; return key or integer
    ;; TODO: does validate ever return nil?
    ;; if not, we dont have to check
    ;; an irc message without an event/command cant exist
    (when event
      (let ((handler (get-handler event)))
        (if handler
            (funcall handler wout message stream)
            ;; default action (simply print event) when no event handler has been defined.
            ;; that means that all validated events will be handled.
            (funcall (get-handler t) wout message stream))))))

;; here we first define several event handler functions.

(defun default-event-handler (wout message stream)
  "The default event handler will handle every valid irc event for which no handler has been specified.

For now, the irc message will simply be displayed in the output window."
  (declare (ignore stream))
  (format wout "~A~%" (ircmsg message)))

(defun ping-handler (wout message stream)
  ;;(declare (ignore))
  (format wout "~A~%" (ircmsg message))
  (format wout "PONG :~A~%" (text message))
  (send stream "PONG :~A" (text message)))

(defun privmsg-handler (wout message stream)
  (declare (ignore stream))
  (format wout "~A ~A: ~A~%"
               (car (get-nick-user-host (prefix message))) ; this should be (nick (prefix message))
               (nth 0 (params message))                    ; this should be (p0 message)
               (text message)))

;; then we add the handlers to events.

(define-handler t 'default-event-handler)
(define-handler :ping 'ping-handler)
(define-handler :privmsg 'privmsg-handler)
