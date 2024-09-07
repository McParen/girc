(cl:defpackage :de.anvi.girc
  (:documentation
   "General client functions, connection, buffer, event and command handling.")
  (:use :common-lisp)
  (:nicknames #:girc)

  ;; shadow the stream type so we can use stream as an accessor
  ;; shadow eval so we can use that to evaluate user commands
  (:shadow
   stream
   eval)

  (:export
   ;; external interface for starting the client
   main
   make

   ;; functions that have to be exported from the core package
   ;; so they can be used within the command package:

   ;; girc
   display-logo
   display-info
   bind
   hostname-p

   ;; parse
   join-args
   string-join
   channelp

   ;; buffer
   buffer
   currentp
   connection-buffer
   target-buffer
   find-buffer
   get-buffer
   connection
   target
   *buffers*
   current-buffer
   display
   echo
   remove-buffer
   select-buffer
   changedp
   redraw
   add-select-server-buffer
   add-select-target-buffer

   ;; connection
   *connections*
   name
   nickname
   username
   realname
   server-password
   hostname
   port
   sslp
   connectedp
   rpl-list-channels
   channels
   channel
   nicknames
   connect
   send
   send-raw-message
   add-connection
   find-connection
   add-channel
   remove-channel
   find-channel
   add-nick
   remove-nick

   ;; ctcp
   make-ctcp-message

   ;; event
   *event-handlers*

   ;; ui
   *ui*
   layout
   output-window
   input-field
   show-buffer-column
   show-buffer-line
   show-topic-line))

(cl:defpackage :de.anvi.girc.irc
  (:documentation
   "IRC protocol commands and supporting functions.")
  (:use :common-lisp)

  (:import-from #:girc
   send
   nickname
   username
   realname
   server-password
   join-args
   make-ctcp-message)

  (:shadow
   list)

  (:export
   authenticate
   cap
   ctcp
   join
   list
   nick
   part
   pass
   pong
   privmsg
   quit
   register
   user
   whois))

(cl:defpackage :de.anvi.girc.conf
  (:documentation
   "Global variables to be set from the user init file.")
  (:use :common-lisp)
  (:export
   nickname
   username
   realname
   show-server-ping
   show-buffer-line
   show-buffer-list
   show-topic-line))

(cl:defpackage :de.anvi.girc.command
  (:documentation
   "Functions that can be called from the command line.")
  (:use :common-lisp)

  ;; Import some often used functions from the core package so we can
  ;; use them without the package prefix, which also means we cant use
  ;; them as commands at the same time.
  ;; To add a new commend with one of these names, remove it from the
  ;; import list first.
  ;;(:import-from #:girc
  ;; echo)

  ;; shadow command names that collide with standard cl functions
  (:shadow
   quote)

  ;; To be recognized as /commands, symbols have to be exported from
  ;; the command package, either here or by a separate call export function
  ;; for example from gircrc.
  (:export
   ctcp
   action
   me
   logo
   show
   hide
   lisp
   channel
   buffer
   info
   server
   connect
   exit
   join
   msg
   nick
   say
   part
   query
   quit
   quote
   whois))

;; we cant add the local nickname within defpackage girc,
;; we have to define all the packages first, then add the nicknames.
(add-package-local-nickname "CMD"  :de.anvi.girc.command :de.anvi.girc)
(add-package-local-nickname "CONF" :de.anvi.girc.conf    :de.anvi.girc)
(add-package-local-nickname "IRC"  :de.anvi.girc.irc     :de.anvi.girc)

;; these abbrevs are now visible in the de.anvi.girc package, but we also need
;; the same abbrevs to be usable from every other package
(add-package-local-nickname "CMD"  :de.anvi.girc.command :de.anvi.girc.command)
(add-package-local-nickname "CONF" :de.anvi.girc.conf    :de.anvi.girc.command)
(add-package-local-nickname "GIRC" :de.anvi.girc         :de.anvi.girc.command)
(add-package-local-nickname "IRC"  :de.anvi.girc.irc     :de.anvi.girc.command)

(add-package-local-nickname "CMD"  :de.anvi.girc.command :de.anvi.girc.conf)
(add-package-local-nickname "CONF" :de.anvi.girc.conf    :de.anvi.girc.conf)
(add-package-local-nickname "GIRC" :de.anvi.girc         :de.anvi.girc.conf)
(add-package-local-nickname "IRC"  :de.anvi.girc.irc     :de.anvi.girc.conf)
