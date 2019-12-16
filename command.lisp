(in-package :de.anvi.girc)

;; used in register
(defun nick (connection nickname)
  "Give the user a new nickname during registration or change the existing one."
  (send-irc-message connection :nick (list nickname) nil))

;; "USER ~A 0 0 :~A"
(defun user (connection username mode realname)
  "Specify the username, mode and realname of a new user when registering a connection."
  (send-irc-message connection :user (list username mode "*") realname))

(defun register (connection nickname mode username realname)
  "Register a connection to an irc server with a nickname and a username.

This is the first command that should be sent after a connection is established.

Upon success, the server will reply with a 001 RPL_WELCOME message."
  (nick connection nickname)
  (user connection username mode realname))

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (connection &optional (message "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send-irc-message connection :quit nil message))

;; nickname can be a mask
(defun whois (connection nickname)
  (send-irc-message connection :whois (list nickname nickname) nil))
