(in-package :de.anvi.girc)

(defparameter *girc-logo*
  (uiop:read-file-string (asdf:system-relative-pathname "girc" "img/girc.asc"))
  "Contains the girc ASCII logo.")

(defparameter *show-server-ping* nil
  "Set to t to show the server PING event and the PONG response.")
