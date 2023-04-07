(in-package :de.anvi.girc.irc)

;;; Implementation of IRC protocol commands and supporting functions.

;; AUTHENTICATE PLAIN
;; AUTHENTICATE <base64-token
;; AUTHENTICATE *
(defun authenticate (connection arg)
  (send connection :authenticate (cl:list arg)))

;; CAP LS
;; CAP REQ :sasl
;; CAP END
(defun cap (connection subcommand &optional text)
  (send connection :cap (cl:list subcommand) text))

(defun ctcp (connection target command &optional args)
  (send connection
        :privmsg
        (cl:list target)
        (if args
            (make-ctcp-message (join-args command args))
            (make-ctcp-message command))))

(defun join (connection channel)
  (send connection :join (cl:list channel)))

;; Replies:
;; rpl-liststart (321)
;; rpl-list (322)
;; rpl-listend (323)
(defun list (connection &optional mask args)
  (send connection :list (remove nil (cl:list mask args))))

(defun nick (connection nickname)
  "Give the user a new nickname during registration or change the existing one."
  (send connection :nick (cl:list nickname)))

(defun part (connection channel)
  (send connection :part (cl:list channel)))

(defun pass (connection password)
  (send connection :pass (cl:list password)))

(defun pong (connection text)
  "Send a PONG command with the appropriate response text."
  (send connection :pong nil text))

(defun privmsg (connection target text)
  (send connection :privmsg (cl:list target) text))

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (connection &optional message)
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send connection :quit nil message))

;; register is not an irc command, but pass, user and nick are.
(defun register (connection)
  "Register a connection to an irc server with a nickname and a username.

This is the first command that should be sent after a connection is established.

Upon success, the server will reply with a 001 RPL_WELCOME message."
  (with-slots (nickname username realname server-password) connection
    (let ((mode 0))
      (when server-password
        (pass connection server-password))
      (nick connection nickname)
      (user connection username mode realname))))

;; "USER ~A 0 0 :~A"
(defun user (connection username mode realname)
  "Specify the username, mode and realname of a new user when registering a connection.

Not used directly, but together with nick during registration with the server."
  (send connection :user (cl:list username mode "*") realname))

(defun whois (connection nickname)
  "Query information about one or more users given by a nickname or mask."
  (send connection :whois (cl:list nickname)))
