(cl:defpackage :de.anvi.girc
  (:documentation
   "The girc package exports symbols for buffer and connection handling,
mainly to be imported to the command package.")
  (:use :common-lisp)
  (:nicknames #:girc)

  ;; shadow the stream type so we can use stream as an accessor
  ;; shadow eval so we can use that to evaluate user commands
  (:shadow #:stream #:eval)

  (:export
   ;; external interface for starting the client
   main
   build

   ;; functions that have to be exported from the core package
   ;; so they can be used within the command package:

   ;; girc
   display-logo
   display-info

   ;; parse
   join-args

   ;; buffer
   buffer
   connection
   target
   *buffers*
   current-buffer
   display
   echo
   remove-buffer
   select-last-buffer
   changedp
   update-status
   append-buffer

   ;; connection
   *connections*
   name
   nickname
   hostname
   port
   sslp
   connectedp
   channels
   channel
   nicknames
   connect
   send
   send-raw
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
   input-field))

(cl:defpackage :de.anvi.girc.conf
  (:documentation
   "The conf package contains global variables to be set from the user init file.")
  (:use :common-lisp)
  (:export
   nickname
   username
   realname
   show-server-ping))

(cl:defpackage :de.anvi.girc.command
  (:documentation
   "The cmd package contains functions to be called from the command line.")
  (:use :common-lisp)

  ;; Import functions from the core package so we can use them without
  ;; the package prefix, which means we cant use them as commands at the
  ;; same time.

  ;; For a new command to be added, remove it from the import list first.
  (:import-from #:girc
   display-logo
   display-info
   join-args
   connection
   target
   *buffers*
   current-buffer
   display
   echo
   remove-buffer
   select-last-buffer
   changedp
   update-status
   append-buffer
   *connections*
   name
   nickname
   hostname
   port
   sslp
   connectedp
   channels
   channel
   nicknames
   send
   send-raw
   add-connection
   find-connection
   add-channel
   remove-channel
   find-channel
   add-nick
   remove-nick
   make-ctcp-message
   *event-handlers*
   *ui*
   input-field)

  ;; shadow command names that collide with cl functions
  (:shadow #:quote)

  ;; To be recognized as /commands, symbols have to be exported from
  ;; the command package, either here or by the separate export function.
  (:export
   ctcp
   action
   me
   lisp
   logo
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
   quit
   quote
   whois))

;; we cant add the local nickname within defpackage girc,
;; we have to define all the packages first, then add the nicknames.
(add-package-local-nickname "CMD"  :de.anvi.girc.command :de.anvi.girc)
(add-package-local-nickname "CONF" :de.anvi.girc.conf    :de.anvi.girc)

;; these abbrevs are now visible in the de.anvi.girc package, but we also need
;; the same abbrevs to be usable from every other package
(add-package-local-nickname "GIRC" :de.anvi.girc         :de.anvi.girc.command)
(add-package-local-nickname "CONF" :de.anvi.girc.conf    :de.anvi.girc.command)
(add-package-local-nickname "GIRC" :de.anvi.girc         :de.anvi.girc.conf)
(add-package-local-nickname "CMD"  :de.anvi.girc.command :de.anvi.girc.conf)
