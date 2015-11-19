(in-package :de.anvi.girc)

(defclass ircmsg ()
  ((server
    :initarg :server
    :initform nil
    :accessor .server
    :type string
    :documentation "Name of the IRC server which sent the message.")

   (prefix
    :initarg :prefix
    :initform nil
    :accessor .prefix
    :type string
    :documentation "Origin of the message.")

   (command
    :initarg :command
    :initform nil
    :accessor .command
    :type string
    :documentation "")

   (params
    :initarg :params
    :initform nil
    :accessor .params
    :type cons
    :documentation "List of strings denoting the parameters.")

   (text
    :initarg :text
    :initform nil
    :accessor .text
    :type string
    :documentation ""))

  (:documentation "IRC message received from an IRC server and parsed."))
