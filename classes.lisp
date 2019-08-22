(in-package :de.anvi.girc)

;; irc-message, ircmsg = irc protocol message string
;; message, msg = parsed girc message object

(defclass message ()
  ((ircmsg
    :initarg       :ircmsg
    :initform      nil
    :accessor      ircmsg
    :type          (or null string)
    :documentation "As-received IRC protocol message, without the CRLF ending. (Kept for debugging purposes.)")

   (prefix
    :initarg       :prefix
    :initform      nil
    :accessor      prefix
    :type          (or null string)
    :documentation "Origin of the message.")

   (command
    :initarg       :command
    :initform      nil
    :accessor      command
    :type          (or null string)
    :documentation "Three-digit numeric or text command.")

   (params
    :initarg       :params
    :initform      nil
    :accessor      params
    :type          (or null cons)
    :documentation "List of strings denoting the parameters.")

   (text
    :initarg       :text
    :initform      nil
    :accessor      text
    :type          (or null string)
    :documentation "Last parameter after the colon, usually denoting the body of the message."))

  (:documentation "Object representing a parsed IRC protocol message."))

;; print the parsed contents of ircmsg in the repl.
;; http://stackoverflow.com/questions/7382122/lisp-how-to-override-default-string-representation-for-clos-class
;; http://clhs.lisp.se/Body/f_pr_obj.htm
;; http://clhs.lisp.se/Body/m_pr_unr.htm
(defmethod print-object ((obj message) out)
  ;; unreadable objects are printed as <# xyz >
  (print-unreadable-object (obj out :type t)
    (format out "~S / ~S / ~S / ~S" (prefix obj) (command obj) (params obj) (text obj))))
