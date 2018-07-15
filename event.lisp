(in-package :de.anvi.girc)

(defparameter *valid-irc-commands*
  '("ERROR" "JOIN" "KICK" "MODE" "NICK" "NOTICE" "PART" "PING" "PRIVMSG" "QUIT" "TOPIC"))

(defun irc-numeric-p (cmd)
  "If cmd is a three-digit numeric IRC reply, return the cmd as an integer, otherwise return nil."
  (let ((char-list (coerce cmd 'list)))
    (if (and (= (length char-list) 3)
             (digit-char-p (nth 0 char-list))
             (digit-char-p (nth 1 char-list))
             (digit-char-p (nth 2 char-list)))
        (values (parse-integer cmd))
        nil)))

(defun irc-command-p (cmd)
  "If cmd is a valid IRC command, return a keyword representing it, otherwise return nil."
  (if (member (string-upcase cmd) *valid-irc-commands* :test #'string=)
      (values (intern (string-upcase cmd) "KEYWORD"))
      nil))

;; TODO: wout sollte eigentlich kein parameter sein, spaeter irgendwie entfernen
;; TODO: stream sollte auch nicht explicit übergeben werden, sondern wie wout als slot in einem connection- oder server-objekt.
(defun evaluate-message (raw-irc-message wout stream)
  (let* ((msg (parse raw-irc-message))

         ;; instead of making separate let-assignments, it would be better to use with-accessors here:
         (cmd (.command msg))
         (params (.params msg))
         (txt (.text msg)))

    ;; in old girc.scm, at this point we passed msg to evaluate-message
    ;; we will always upcase commands before comparing them.
    (cond

#|
#<DE.ANVI.GIRC::IRCMSG NIL / ERROR / NIL / Closing Link: dslb-178-006-222-043.178.006.pools.vodafone-ip.de (Quit: Bye)>
|#
      ((string= cmd "ERROR")
       ;; ERROR :txt
       (format wout "~A :~A~%" cmd txt))
#|
#<DE.ANVI.GIRC::IRCMSG ghost-287!~ghost-287@154.242.11.218 / JOIN / ("#python-unregistered") / NIL>
#<DE.ANVI.GIRC::IRCMSG ghost-287!~ghost-287@unaffiliated/ghost-287 / JOIN / ("#python-unregistered") / NIL>
#<DE.ANVI.GIRC::IRCMSG tomonori__!~ayum@45.32.67.191 / JOIN / ("#python-unregistered") / NIL>
#<DE.ANVI.GIRC::IRCMSG diek!~diek@134.41.34.242 / JOIN / ("#python-unregistered") / NIL>
|#
      ((string= cmd "JOIN")
       ;; JOIN nick #channel
       (format wout "~A ~A ~A~%"
               cmd
               (car (get-nick-user-host (.prefix msg)))
               (car (.params msg))))

#|
#<DE.ANVI.GIRC::IRCMSG ghost-287!~ghost-287@154.242.11.218 / QUIT / NIL / Changing host>
#<DE.ANVI.GIRC::IRCMSG diek!~diek@134.41.34.242 / QUIT / NIL / Remote host closed the connection>
#<DE.ANVI.GIRC::IRCMSG nehaljwani!~nehaljwan@unaffiliated/new-student/x-0672275 / QUIT / NIL / Quit: leaving>
|#
      ((string= cmd "QUIT")
       ;; QUIT nick :reason
       (format wout "~A ~A :~A~%"
               cmd
               (car (get-nick-user-host (.prefix msg)))
               txt))

#|
#<DE.ANVI.GIRC::IRCMSG ghost-287!~ghost-287@unaffiliated/ghost-287 / PART / ("#python-unregistered") / NIL>
#<DE.ANVI.GIRC::IRCMSG diek!~diek@134.41.34.242 / JOIN / ("#python-unregistered") / NIL>
|#
      ((string= cmd "PART")
       ;; PART nick #channel
       (format wout "~A ~A ~A~%"
               cmd
               (car (get-nick-user-host (.prefix msg)))
               (car (.params msg))))

#|
#<DE.ANVI.GIRC::IRCMSG wrengr_away!~wrengr@104.132.1.65 / NICK / NIL / wrengr>
|#
      ((string= cmd "NICK")
       ;; NICK old new
       (format wout "~A ~A ~A~%"
               cmd
               (car (get-nick-user-host (.prefix msg)))
               txt))

      ((string= cmd "NOTICE")
       ;; NOTICE :txt
       (format wout "~A :~A~%" cmd txt))

      ((string= cmd "PING")
       ;; TODO: before we send anything, we craft a proper irc message, then we send a message object,
       ;; and before it gets sent it is serialized into a string.
       (format wout "PONG :~A~%" (.text msg))
       (send stream "PONG :~A" (.text msg)))

      ((string= cmd "PRIVMSG")
       (format wout "~A ~A: ~A~%"
               (car (get-nick-user-host (.prefix msg)))
               (nth 0 params)
               txt))


;;; NUMERICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See: http://www.alien.net.au/irc/irc2numerics.html

;;; Replies 001 to 004 follow a successful login.

;; Number:  001
;; Event:   RPL_WELCOME
;; Syntax:  001 <target nick> :<text>
;; Example: :barjavel.freenode.net 001 arrk13 :Welcome to the freenode Internet Relay Chat Network arrk13
       
;; Number:  002
;; Event:   RPL_YOURHOST
;; Syntax:  002 <target nick> :<text>
;; Example: :barjavel.freenode.net 002 arrk13 :Your host is barjavel.freenode.net[78.40.125.4/6667], running version ircd-seven-1.1.0

;; Number:  003
;; Event:   RPL_CREATED
;; Example: :barjavel.freenode.net 003 arrk13 :This server was created Thu Sep 8 2011 at 15:12:03 CEST

;; Number:  004
;; Event:   RPL_MYINFO
;; Example: :barjavel.freenode.net 004 arrk13 barjavel.freenode.net ircd-seven-1.1.0 DOQRSZaghilopswz CFILMPQbcefgijklmnopqrstvz bkloveqjfI
      
      ((string= cmd "001") nil)
      ((string= cmd "002") nil)
      ((string= cmd "003") nil)
      ((string= cmd "004") nil)

;;; Those replies also follow a successful login.
      
;; Example: :barjavel.freenode.net 005 arrk13 CHANTYPES=# EXCEPTS INVEX CHANMODES=eIbq,k,flj,CFLMPQcgimnpstz CHANLIMIT=#:120 PREFIX=(ov)@+ MAXLIST=bqeI:100 MODES=4 NETWORK=freenode KNOCK STATUSMSG=@+ CALLERID=g :are supported by this server
;; Example: :barjavel.freenode.net 005 arrk13 CASEMAPPING=rfc1459 CHARSET=ascii NICKLEN=16 CHANNELLEN=50 TOPICLEN=390 ETRACE CPRIVMSG CNOTICE DEAF=D MONITOR=100 FNC TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,PRIVMSG:4,NOTICE:4,ACCEPT:,MONITOR: :are supported by this server
;; Example: :barjavel.freenode.net 005 arrk13 EXTBAN=$,arx WHOX CLIENTVER=3.0 SAFELIST ELIST=CTU :are supported by this server

      ((string= cmd "005") nil)

;; Example: :barjavel.freenode.net 250 arrk13 :Highest connection count: 10322 (10321 clients) (548594 connections received)
      
      ((string= cmd "250") nil)

;; Example: :barjavel.freenode.net 251 arrk13 :There are 278 users and 64960 invisible on 28 servers
      
      ((string= cmd "251") nil)

;; Example: :barjavel.freenode.net 252 arrk13 35 :IRC Operators online      
      
      ((string= cmd "252") nil)

;; Example: :barjavel.freenode.net 252 arrk13 35 :IRC Operators online
      
      ((string= cmd "253") nil)

;; Example: :barjavel.freenode.net 254 arrk13 44981 :channels formed

      ((string= cmd "254") nil)

;; Example: :barjavel.freenode.net 255 arrk13 :I have 4169 clients and 1 servers

      ((string= cmd "255") nil)

;; Example: :barjavel.freenode.net 265 arrk13 4169 10321 :Current local users 4169, max 10321

      ((string= cmd "265") nil)

;; Example: :barjavel.freenode.net 266 arrk13 65238 74286 :Current global users 65238, max 74286

      ((string= cmd "266") nil)
      
      ((string= cmd "353") nil) ; nicklist
      ((string= cmd "372") nil) ; motd
      
;; Number:  375
;; Event:   RPL_MOTDSTART
;; Syntax:  :- <server> Message of the day - 
;; Example: :manitu.iZ-smart.net 375 arrakis23 :- manitu.iZ-smart.net Message of the Day -

      ((string= cmd "375") nil)

;; Number:  376
;; Event:   RPL_ENDOFMOTD
;; Syntax:  :End of /MOTD command
;; Example: :adams.freenode.net 376 arrk23 :End of /MOTD command.

      ((string= cmd "376") nil)

;;; Response to WHOIS
      
#|
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 311 / ("haom" "haom" "~haom" "dslb-178-006-222-043.178.006.pools.vodafone-ip.de" "*") / haom>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 319 / ("haom" "haom") / #debian >
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 312 / ("haom" "haom" "verne.freenode.net") / Amsterdam, NL, EU>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 378 / ("haom" "haom") / is connecting from *@dslb-178-006-222-043.178.006.pools.vodafone-ip.de 178.6.222.43>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 317 / ("haom" "haom" "1754" "1507052537") / seconds idle, signon time>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 318 / ("haom" "haom") / End of /WHOIS list.>
|#

;; :leguin.freenode.net 311 arrk13 arrk13 ~arrakis24 dslb-188-109-148-125.pools.arcor-ip.net * :Kyle McLachlan
;; :leguin.freenode.net 319 arrk13 arrk13 :@#tesfff #ubuntu
;; :leguin.freenode.net 312 arrk13 arrk13 leguin.freenode.net :Ume?, SE, EU
;; :leguin.freenode.net 378 arrk13 arrk13 :is connecting from *@dslb-188-109-148-125.pools.arcor-ip.net 188.109.148.125
;; :leguin.freenode.net 317 arrk13 arrk13 5395 1348471347 :seconds idle, signon time
;; :leguin.freenode.net 301 arrk13 Xach :zzz
;; :leguin.freenode.net 330 arrk13 Xach Xach :is logged in as
;; :leguin.freenode.net 318 arrk13 arrk13 :End of /WHOIS list.
      
;; Number:  311
;; Event:   RPL_WHOISUSER
;; Syntax:  311 <target nick> <nick> <user> <host> * :<real name>
;; Example: :calvino.freenode.net 311 arrk23 arrk23 ~arrakis24 dslb-092-073-066-073.pools.arcor-ip.net * :McLachlan

;; Number:  319
;; Event:   RPL_WHOISCHANNELS
;; Syntax:  319 <target nick> <nick> :<channels>
;; Example: :leguin.freenode.net 319 arrk13 arrk13 :@#tesfff #ubuntu

;; Number:  312
;; Event:   RPL_WHOISSERVER
;; Syntax:  312 <target nick> <nick> <server> :<server location>
;; Example: :calvino.freenode.net 312 arrk23 arrk23 calvino.freenode.net :Milan, IT
      
;; Number:  378
;; Event:   RPL_CONNECT_FROM
;; Example: :calvino.freenode.net 378 arrk23 arrk23 :is connecting from *@dslb-092-073-066-073.pools.arcor-ip.net 92.73.66.73

;; Number:  317
;; Event:   RPL_WHOISIDLE
;; Syntax:  317 <target nick> <nick> <idle time in seconds> <signon time in seconds> :<text>
;; Example: :sendak.freenode.net 317 arrk23 arrk23 32 1316203528 :seconds idle, signon time

;; Number:  318
;; Event:   RPL_ENDOFWHOIS
;; Example: :calvino.freenode.net 318 arrk23 arrk23 :End of /WHOIS list.

      
      ;; TODO: add terpri at the end of every event handler except the ignored events
      ;; print all other messages verbatim for now
      (t (format wout "~S~%" msg)))))

#|
=> join #python

#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 470 / ("haom" "#python" "#python-unregistered") / Forwarding to another channel>
#<DE.ANVI.GIRC::IRCMSG haom!~haom@dslb-178-006-222-043.178.006.pools.vodafone-ip.de / JOIN / ("#python-unregistered") / NIL>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 332 / ("haom" "#python-unregistered") / Welcome to #python-unregistered! You've been put here because #python requires you to register your nickname with Freenode. For more information about registering on Freenode, ask #freenode. For more information about #pytho
n,#python-* moderation, #python-ops.>
#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 333 / ("haom" "#python-unregistered" "lvh!~lvh@python/psf/lvh" "1309509859") / NIL>

hier fehlen wahrscheinlich die ganzen nicks

#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 366 / ("haom" "#python-unregistered") / End of /NAMES list.>
|#


#|
;; CTCP something in the text parameter
;; hackagebot: ^AACTION git-annex 6.20171003 - manage files with git, without checking their contents into git - https://hackage.haskell.org/package/git-annex^A


mode line upon logging in

#<DE.ANVI.GIRC::IRCMSG haom / MODE / ("haom") / +i>




EMIYA haom: ^AVERSION^A


#<DE.ANVI.GIRC::IRCMSG verne.freenode.net / 470 / ("haom" "#archlinux" "#archlinux-unregistered") / Forwarding to another channel>

|#
