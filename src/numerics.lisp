(in-package :de.anvi.girc)

;;; Numeric reply and error codes

;;; Source: https://defs.ircdocs.horse/defs/numerics.html

(defparameter *numerics*
  '((  1 . :rpl-welcome)
    (  2 . :rpl-yourhost)
    (311 . :rpl-whoisuser)
    (312 . :rpl-whoisserver)
    (317 . :rpl-whoisidle)
    (318 . :rpl-endofwhois)
    (330 . :rpl-whoisaccount)
    (338 . :rpl-whoisactually)
    (353 . :rpl-namreply)
    (366 . :rpl-endofnames)
    (372 . :rpl-motd)
    (375 . :rpl-motdstart)
    (376 . :rpl-endofmotd)
    (378 . :rpl-whoishost)
    (671 . :rpl-whoissecure)))
