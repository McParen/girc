(in-package :de.anvi.girc)

;; irc-message, ircmsg = irc protocol message string
;; message, msg = parsed girc message object

(defclass interface ()
  ((main-screen
    :initarg       :screen
    :initform      nil
    :accessor      main-screen
    :type          (or null crt:screen)
    :documentation "croatoan/ncurses main screen.")

   (output-window
    :initarg       :output-window
    :initform      nil
    :accessor      output-window
    :type          (or null crt:window)
    :documentation "croatoan/ncurses window for the display of server and command output.")

   (input-window
    :initarg       :input-window
    :initform      nil
    :accessor      input-window
    :type          (or null crt:window)
    :documentation "croatoan/ncurses window for the user command input.")

   (input-field
    :initarg       :input-field
    :initform      nil
    :accessor      input-field
    :type          (or null crt:field)
    :documentation "croatoan/ncurses field for the user command input."))

  (:documentation "Elements of the user input and output interface based on croatoan/ncurses."))

(defmethod initialize-instance :after ((ui interface) &key)
  "Initialize the window and field objects that are part of the user interface."
  (with-slots (main-screen input-window output-window input-field) ui
    (setf main-screen   (make-instance 'crt:screen
                                       :input-echoing nil
                                       :input-buffering nil
                                       :process-control-chars t
                                       :cursor-visible t
                                       :enable-colors nil
                                       :enable-function-keys t)
          output-window (make-instance 'crt:window
                                       :height (1- (crt:height main-screen))
                                       :width (crt:width main-screen)
                                       :location '(0 0)
                                       :enable-scrolling t)
          input-window  (make-instance 'crt:window
                                       :height 1
                                       :width (crt:width main-screen)
                                       :location (list (1- (crt:height main-screen)) 0)
                                       :enable-function-keys t
                                       ;; note that when this is nil, we plan to perform work during the nil event.
                                       :input-blocking nil)
          input-field   (make-instance 'crt:field :location (list 0 0) :width (crt:width main-screen) :window input-window
                                       :style (list :foreground nil :background nil) :keymap 'girc-input-map))))

(defun finalize-interface (ui)
  "Cleanly free ncurses object memory."
  (with-slots (input-window output-window) ui
    (close input-window)
    (close output-window)
    (crt::end-screen)))

(defclass connection ()
  ((name
    :initarg       :name
    :initform      nil
    :accessor      connection-name
    :type          (or null string)
    :documentation "Name of the server to which the connection is established.")

   (hostname
    :initarg       :hostname
    :initform      nil
    :accessor      connection-server
    :type          (or null string)
    :documentation "Hostname of the IRC server to which the connection is established.")

   (port
    :initarg       :port
    :initform      6667
    :accessor      connection-port
    :type          integer
    :documentation "Port to which the server connection is established.")

   (stream
    :initarg       :stream
    :initform      nil
    :accessor      connection-stream
    :type          (or null string)
    :documentation "Hostname of the IRC server to which the connection is established.")

   (nickname
    :initarg       :nickname
    :initform      nil
    :accessor      connection-nickname
    :type          (or null string)
    :documentation "Nickname of the user to be registered with the connected server.")

   (username
    :initarg       :username
    :initform      "myuser"
    :accessor      connection-username
    :type          (or null string)
    :documentation "Username of the user to be registered with the connected server.")

   (realname
    :initarg       :realname
    :initform      "Realname"
    :accessor      connection-realname
    :type          (or null string)
    :documentation "Realname of the user to be registered with the connected server."))

  (:documentation "Parameters necessary to establish a connection to an IRC server."))

(defmethod initialize-instance :after ((con connection) &key)
  "Initialize the window and field objects that are part of the user interface."
  (with-slots (stream hostname port nickname username realname) con
    (setf stream (connect hostname port))
    (register con nickname 0 username realname)))

(defclass message ()
  ((connection
    :initarg       :connection
    :initform      nil
    :accessor      connection
    :type          (or null connection)
    :documentation "Connection from which the message was received.")

   (ircmsg
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

