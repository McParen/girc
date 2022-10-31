(in-package :de.anvi.girc)

;;; Numeric reply and error codes

;;; Source: https://defs.ircdocs.horse/defs/numerics.html

(defparameter *numerics*
  '((  1 . :rpl-welcome)
    (  2 . :rpl-yourhost)
    (  3 . :rpl-created)
    (  4 . :rpl-myinfo)
    (  5 . :rpl-isupport)
    (250 . :rpl-statsconn)
    (251 . :rpl-luserclient)
    (252 . :rpl-luserop)
    (253 . :rpl-luserunknown)
    (254 . :rpl-luserchannels)
    (255 . :rpl-luserme)
    (265 . :rpl-localusers)
    (266 . :rpl-globalusers)
    (311 . :rpl-whoisuser)
    (312 . :rpl-whoisserver)
    (317 . :rpl-whoisidle)
    (318 . :rpl-endofwhois)
    (319 . :rpl-whoischannels)
    (330 . :rpl-whoisaccount)
    (332 . :rpl-topic)
    (333 . :rpl-topicwhotime)
    (338 . :rpl-whoisactually)
    (353 . :rpl-namreply)
    (366 . :rpl-endofnames)
    (372 . :rpl-motd)
    (375 . :rpl-motdstart)
    (376 . :rpl-endofmotd)
    (378 . :rpl-whoishost)
    (671 . :rpl-whoissecure)
    (900 . :rpl-loggedin)
    (903 . :rpl-saslsuccess)))
