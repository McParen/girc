(in-package :de.anvi.girc)

;; raw-message, rawmsg = raw irc protocol message string
;; irc-message, ircmsg = parsed girc message object

(defclass irc-message ()
  ((connection
    :initarg       :connection
    :initform      nil
    :accessor      connection
    :type          (or null connection)
    :documentation "Connection from which the message was received.")

   (rawmsg
    :initarg       :rawmsg
    :initform      nil
    :accessor      rawmsg
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

;; print the parsed contents of raw-message in the repl.
;; http://stackoverflow.com/questions/7382122/lisp-how-to-override-default-string-representation-for-clos-class
;; http://clhs.lisp.se/Body/f_pr_obj.htm
;; http://clhs.lisp.se/Body/m_pr_unr.htm
(defmethod print-object ((obj irc-message) out)
  ;; unreadable objects are printed as <# xyz >
  (print-unreadable-object (obj out :type t)
    (format out "~S / ~S / ~S / ~S" (prefix obj) (command obj) (params obj) (text obj))))

;; put every read char into an array of max 512 length.

;; check if ircmsg is max 512 chars.

;; check if irc-message ends in crlf.

;; if ircmsg starts with a colon, then cut to the second colon
;; if ircmsg doesnt start with a colon, then cut to the first colon.

;; TODO: ":a b c g :d e f" => (":a" "b" "c" "g" ":d" "e" "f") => message object

;; ":prefix command p1 p2 p3 :text1 text2"
;; ":prefix command p1 p2 p3"
;; ":prefix command :text1 text2"
;; "command p1 p2 p3 :text1 text2"
;; "command p1 p2 p3"
;; "command :text1 text2"

(defun strlst (str)
  "Take a string, return a list of characters."
  (coerce str 'list))

(defun lststr (lst)
  "Take a list of characters, return a string."
  (coerce lst 'string))

;; To be used in ltrim.
;; Example: (ltrimlst '(#\  #\  #\1 #\2 #\3))
(defun ltrimlst (lst)
  "Take a list of characters, return a list with spaces on the left side removed."
  (if (equal (car lst) #\space)
      (ltrimlst (cdr lst))
      lst))

(defun ltrim (str)
  "Take a string, return a string with spaces on the left side removed."
  (let ((trmd (ltrimlst (strlst str))))
    (when trmd
      (lststr trmd))))

(defun token (str)
  "Take a string, return the a list with the first word and the rest of the string."
  (let* ((str (ltrim str))
         (pos (position #\space str)))
    (when str
      (if pos
          (list (subseq str 0 pos)
                  (ltrim (subseq str pos)))
          (list str nil)))))

(defun colonp (str)
  "Return t if #\: is the first char in the string, otherwise nil."
  (equal #\: (car (strlst str))))

;; (get-prefix-and-command ":prefix command p1 p2 p3 :text1 text2") => ("prefix" "command" "p1 p2 p3 :text1 text2")
;; (get-prefix-and-command "command p1 p2 p3 :text1 text2") => (NIL "command" "p1 p2 p3 :text1 text2")
;; the result list is fed to get-params-and-text.
(defun get-prefix-and-command (str)
  "Take a string containing an irc message, return a list with the prefix command and rest of the message."
  (let ((t1 (car (token str)))
        (r1 (cadr (token str))))
    (if (colonp t1)
        (let ((t2 (car (token r1)))
              (r2 (cadr (token r1))))
          (list (subseq t1 1) t2 r2))
        (list nil t1 r1))))

(defun string-any (str1 str2)
  "Return t if any char from str1 is in str2, nil otherwise."
  (if (some #'(lambda (i) (find i str2)) str1) t nil))

;; (string-tokenize "!@" "n!u@h") => ("n" "u" "h")
(defun string-tokenize (str1 str2)
  "Return a list of substrings of str2 split on chars from str1."
  (values (split-sequence:split-sequence-if #'(lambda (i) (position i str1)) str2 :remove-empty-subseqs t)))

;; type of prefix can be nick-user-host or just a host.
;; (get-nick-user-host "nick!user@host") => ("nick" "user" "host")
;; (get-nick-user-host "leo!~leo@host-205-241-38-153.acelerate.net") => ("leo" "~leo" "host-205-241-38-153.acelerate.net")
(defun get-nick-user-host (prefix)
  (if (string-any "!@" prefix)
      (string-tokenize "!@" prefix)
      ;; if the prefix isnt in the form nick!user@host it is just a server hostname
      ;; :efnet.portlane.se 366 haom #EFNetNews :End of /NAMES list.
      (list "" "" prefix)))

;; (get-params-and-text '("prefix" "command" "p1 p2 p3 :text1 text2")) => (("p1" "p2" "p3") "text1 text2")
;; (get-params-and-text '(NIL "command" "p1 p2 p3 :text1 text2")) => (("p1" "p2" "p3") "text1 text2")
(defun get-params-and-text (lst)
  "Take a list returned by get-prefix-and-command, return a list with a list of parameters and the text."
  (let* ((str (third lst))
         (pos (position #\: str)))
    (if pos
        (list (remove "" (split-sequence:split-sequence #\space (subseq str 0 pos)) :test #'equal)
              (subseq str (1+ pos)))
        (list (remove "" (split-sequence:split-sequence #\space (subseq str 0 pos)) :test #'equal)
              nil))))

;; (parse ":prefix command p1 p2 p3 :text1 text2") => ("prefix" "command" ("p1" "p2" "p3") "text1 text2")
(defun parse2 (rawmsg)
  "Take a string containing an irc message, return a list with parsed components."
  (let* ((lst1 (get-prefix-and-command rawmsg))
         (lst2 (get-params-and-text lst1)))
    (append (list (car lst1) (cadr lst1)) lst2)))

;; position #\: str
;; count #\: str
;; subseq str 0 5
;; coerce str 'list, coerce lst 'string

(defun parse-raw-message (rawmsg connection)
  "Take a string containing an irc message, return a irc message object."
  (let* ((lst1 (get-prefix-and-command rawmsg))
         (lst2 (get-params-and-text lst1)))
    (make-instance 'irc-message :connection connection :rawmsg rawmsg :prefix (car lst1)
                   :command (cadr lst1) :params (car lst2) :text (cadr lst2))))

(defun prefix-nick (irc-msg)
  (nth 0 (get-nick-user-host (prefix irc-msg))))

(defun prefix-user (irc-msg)
  (nth 1 (get-nick-user-host (prefix irc-msg))))

(defun prefix-host (irc-msg)
  (nth 2 (get-nick-user-host (prefix irc-msg))))

;; Examples: (user-input-parse "/hello there dear john") => ("hello" "there dear john")
;;           (user-input-parse "hello there dear john") => ("" "hello there dear john")
;;           (user-input-parse "/hello") => ("hello" "")
;; (parse-user-input "/hello there dear john") => ("Hello" "there dear john")"
(defun parse-user-input (str)
  "Split a user input string into a command, if the line beginns with /, and a string of arguments.

Return a pair consisting of a command string and an args string, (cmd . args).

Called from handle-user-command."
  (if (char= #\/ (char str 0))
      ;; if we have a command
      (let ((pos (position #\space str)))
        (if pos
            ;; if we have a command + args
            (cons (subseq str 1 pos) (subseq str (1+ pos)))
            ;; if we only have a command
            (cons (subseq str 1) nil)))
      ;; if we have no command, only the arguments.
      (cons nil str)))

(defun string-car (str)
  "1 2 3 => 1"
  (let ((pos (position #\space str)))
    (if pos
        (subseq str 0 pos)
        (subseq str 0))))

(defun string-cdr (str)
  "1 2 3 => 2 3"
  (let* ((str (string-trim " " str))
         (pos (position #\space str)))
    (if pos
        ;; remove any additional spaces
        ;; 1  2  3 = 2  3
        (string-left-trim " " (subseq str (1+ pos)))
        nil)))

(defun string-cadr (str)
  "1 2 3 => 2"
  (string-car (string-cdr str)))

;; Example: (string-nth 2 "a b c d e f") => "c"
(defun string-nth (n str)
  (nth n (split-sequence:split-sequence #\space str :remove-empty-subseqs t)))

(defun ntharg (n args)
  (string-nth n args))

;; Example: (string-nthcdr 2 "a b c d e f") => "c d e f"
(defun string-nthcdr (n str)
  "Take a string, return the rest that would be returned by calling string-cdr n times."
  (let ((str (string-trim " " str)))
    (cond ((string= "" str)
           nil)
          ((= n 0)
           str)
          ((= n 1)
           (string-cdr str))
          (t
           (string-nthcdr (1- n) (string-cdr str))))))

(defun nthargs (n str)
  "Return str without the first n space-separated tokens."
  (string-nthcdr n str))

;; Example: (string-nthcdr 2 "a b c d e f") => ("c" "d" "e" "f")
(defun string-nthcdr- (n str)
  (nthcdr n (split-sequence:split-sequence #\space str :remove-empty-subseqs t)))

(defun split-args (args-string)
  "Take a string, return a list of substrings split on space as the delimiter."
  (split-sequence:split-sequence #\space args-string))

(defun join-args (args-list)
  "Take a list of strings, join them with space as the delimiter."
  (format nil "~{~A~^ ~}" args-list))

;; Example: (string-nth-list '(0 2) "a b c d e f") => ("a" "c")
(defun string-nth-list (positions str)
  "Take a string with args and a list of arg positions to return."
  (loop for i in positions
        collect (string-nth i str)))

;; Example: (get-args "a bord c irc.com d e f" '(nick host) '(1 3))
;; ((HOST . "irc.com") (NICK . "bord"))
(defun get-named-args (args names positions)
  (pairlis names (string-nth-list positions args)))

;; https://stackoverflow.com/questions/25897254/lisp-creating-property-list-from-strings
;; https://stackoverflow.com/questions/8830888/whats-the-canonical-way-to-join-strings-in-a-list

;; from the cl-cookbook

;; Example: (replace-all "a $B c d B e f b" "$B" "nick") => "a nick c d B e f b"
(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurrences of the part is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string :start2 old-pos :test test)
            do (write-string string out :start old-pos :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

;; we want to be able to take a string with special variables
;; and return a string with all those variables replaced with values

;; "a b ${nickname} d e" => "a b hello d e"
