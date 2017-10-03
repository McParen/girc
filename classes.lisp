(in-package :de.anvi.girc)

(defclass ircmsg ()
  ((server
    :initarg :server
    :initform nil
    :accessor .server
    :type (or null string)
    :documentation "Name of the IRC server which sent the message.")

   (prefix
    :initarg :prefix
    :initform nil
    :accessor .prefix
    :type (or null string)
    :documentation "Origin of the message.")

   (command
    :initarg :command
    :initform nil
    :accessor .command
    :type (or null string)
    :documentation "")

   (params
    :initarg :params
    :initform nil
    :accessor .params
    :type (or null cons)
    :documentation "List of strings denoting the parameters.")

   (text
    :initarg :text
    :initform nil
    :accessor .text
    :type (or null string)
    :documentation ""))

  (:documentation "IRC message received from an IRC server and parsed."))

;; print the parsed contents of ircmsg in the repl.
;; http://stackoverflow.com/questions/7382122/lisp-how-to-override-default-string-representation-for-clos-class
;; http://clhs.lisp.se/Body/f_pr_obj.htm
;; http://clhs.lisp.se/Body/m_pr_unr.htm
(defmethod print-object ((obj ircmsg) out)
  ;; unreadable objects are printed as <# xyz >
  (print-unreadable-object (obj out :type t)
    (format out "~S / ~S / ~S / ~S" (.prefix obj) (.command obj) (.params obj) (.text obj))))
