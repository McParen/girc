(cl:defpackage :de.anvi.girc
  (:use :common-lisp :split-sequence)
  (:nicknames #:girc)
  ;; shadow the stream type so we can use stream as an accessor
  (:shadow #:stream)
  (:export

   ;; girc.lisp
   run
   build))
