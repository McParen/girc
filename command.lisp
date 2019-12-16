(in-package :de.anvi.girc)

;; used in register
(defun nick (stream nickname)
  "Give the user a new nickname during registration or change the existing one."
  (send-irc-message stream :nick (list nickname) nil))

;; "USER ~A 0 0 :~A"
(defun user (stream username mode realname)
  "Specify the username, mode and realname of a new user when registering a connection."
  (send-irc-message stream :user (list username mode "*") realname))

(defun register (stream nickname mode username realname)
  "Register a connection to an irc server with a nickname and a username.

This is the first command that should be sent after a connection is established.

Upon success, the server will reply with a 001 RPL_WELCOME message."
  (nick stream nickname)
  (user stream username mode realname))

;; QUIT :Gone to have lunch
;; :syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch
;; ERROR :Closing Link: 5.146.114.134 (Client Quit)
(defun quit (stream &optional (message "Bye"))
  "Cleanly QUIT an IRC connection and send a message to the joined channels.

The server acknowledges this by sending an ERROR message to the client."
  (send-irc-message stream :quit nil message))

;; nickname can be a mask
(defun whois (stream nickname)
  (send-irc-message stream :whois (list nickname nickname) nil))
