(cl:defpackage :de.anvi.girc
  (:use :common-lisp :split-sequence)
  (:nicknames #:girc)

  ;; shadow the stream type so we can use stream as an accessor
  (:shadow #:stream #:eval)

  (:export

   ;; girc
   run
   build

   ;; functions to use in commands
   display
   display-logo
   echo
   *buffers*
   remove-buffer
   select-last-buffer
   changedp
   current-buffer
   update-status
   connection
   connectedp
   target
   arglen
   append-buffer
   buffer
   find-connection
   ntharg
   *event-handlers*
   name
   nickname
   nicknames
   hostname
   display-info
   *connections*
   channel
   channels
   connect
   input-field
   *ui*
   send
   send-raw))

(cl:defpackage :de.anvi.girc.conf
  (:use :common-lisp)
  (:export show-server-ping))

(cl:defpackage :de.anvi.girc.command
  (:use :common-lisp)

  (:import-from #:girc
   display
   display-logo
   echo
   *buffers*
   remove-buffer
   select-last-buffer
   changedp
   current-buffer
   update-status
   connection
   connectedp
   target
   arglen
   append-buffer
   ;; buffer
   find-connection
   ntharg
   *event-handlers*
   name
   nickname
   nicknames
   hostname
   display-info
   *connections*
   channel
   channels
   ;; connect
   input-field
   *ui*
   send
   send-raw)

  ;; shadow cl functions that collide with commands
  (:shadow #:eval #:quote)

  (:export

   ;; command
   lisp
   logo
   buffer
   info
   server
   connect
   exit
   join
   msg
   say
   part
   quit
   quote
   whois))

;; we cant add the local nickname within defpackage girc, we have to define the packages first.
(add-package-local-nickname "CMD" :de.anvi.girc.command :de.anvi.girc)
(add-package-local-nickname "CONF" :de.anvi.girc.conf :de.anvi.girc)
