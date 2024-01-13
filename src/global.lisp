(in-package :de.anvi.girc)

(defparameter *girc-logo*
  (uiop:read-file-string (asdf:system-relative-pathname "girc" "img/girc.asc"))
  "Contains the girc ASCII logo.")

(in-package :de.anvi.girc.conf)

(defparameter nickname nil
  "Default nickname to be used when a server specific nick is not provided.")

(defparameter username "myuser"
  "Default username to be used when a server specific user is not provided.")

(defparameter realname "Realname"
  "Default realname to be used when a server specific name is not provided.")

(defparameter show-server-ping nil
  "Set to t to show the server PING event and the PONG response.")

(defparameter show-buffer-list nil
  "Set to t to show the list of buffers in a side window.")

(defparameter show-topic-line t
  "Set to t to show the topic line at the top of the screen.")
