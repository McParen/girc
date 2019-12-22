(in-package :de.anvi.girc)

;;; Implementation of IRC protocol commands and supporting functions.

(defun nick (connection nickname)
  "Give the user a new nickname during registration or change the existing one."
  (send-irc-message connection :nick (list nickname) nil))

(defun pong (connection text)
  "Send a PONG command with the appropriate response text."
  (send-irc-message connection :pong nil text))

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (connection &optional (quit-message "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send-irc-message connection :quit nil quit-message))

(defun register (connection nickname mode username realname)
  "Register a connection to an irc server with a nickname and a username.

This is the first command that should be sent after a connection is established.

Upon success, the server will reply with a 001 RPL_WELCOME message."
  (nick connection nickname)
  (user connection username mode realname))

;; "USER ~A 0 0 :~A"
(defun user (connection username mode realname)
  "Specify the username, mode and realname of a new user when registering a connection.

Not used directly, but together with nick during registration with the server."
  (send-irc-message connection :user (list username mode "*") realname))

(defun whois (connection nickname)
  "Query information about one or more users given by a nickname or mask."
  (send-irc-message connection :whois (list nickname) nil))
