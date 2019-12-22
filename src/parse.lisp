(in-package :de.anvi.girc)

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
(defun parse2 (ircmsg)
  "Take a string containing an irc message, return a list with parsed components."
  (let* ((lst1 (get-prefix-and-command ircmsg))
         (lst2 (get-params-and-text lst1)))
    (append (list (car lst1) (cadr lst1)) lst2)))

;; position #\: str
;; count #\: str
;; subseq str 0 5
;; coerce str 'list, coerce lst 'string

(defun parse-raw-message (ircmsg connection)
  "Take a string containing an irc message, return a irc message object."
  (let* ((lst1 (get-prefix-and-command ircmsg))
         (lst2 (get-params-and-text lst1)))
    (make-instance 'message :connection connection :ircmsg ircmsg :prefix (car lst1)
                   :command (cadr lst1) :params (car lst2) :text (cadr lst2))))

(defun prefix-nick (message)
  (nth 0 (get-nick-user-host (prefix message))))

(defun prefix-user (message)
  (nth 1 (get-nick-user-host (prefix message))))

(defun prefix-host (message)
  (nth 2 (get-nick-user-host (prefix message))))

;; Split a user line into a command, if the line beginns with /, and the text.
;; Examples: (user-input-parse "/hello there dear john") => ("hello" "there dear john")
;;           (user-input-parse "hello there dear john") => ("" "hello there dear john")
;;           (user-input-parse "/hello") => ("hello" "")
;; (parse-user-input "/hello there dear john") => ("Hello" "there dear john")
(defun parse-user-input (str)
  (if (char= #\/ (char str 0))
      ;; if we have a command
      (let ((pos (position #\space str)))
        (if pos
            ;; if we have a command + args
            (cons (subseq str 1 pos) (subseq str (1+ pos)))
            ;; if we only have a command
            (cons (subseq str 1) nil)))
      ;; if we have no command, only text
      (cons nil str)))

(defun string-car (str)
  "1 2 3 => 1"
  (let ((pos (position #\space str)))
    (if pos
        (subseq str 0 pos)
        (subseq str 0))))

(defun string-cdr (str)
  "1 2 3 => 2 3"
  (let ((pos (position #\space str)))
    (if pos
        (subseq str (1+ pos))
        nil)))

(defun string-cadr (str)
  "1 2 3 => 2"
  (string-car (string-cdr str)))

(defun split-args (args-string)
  "Take a string, return a list of substrings split on space as the delimiter."
  (split-sequence:split-sequence #\space args-string))

(defun merge-args (args-list)
  "Take a list of strings, merge them with space as the delimiter."
  (format nil "~{~A~^ ~}" args-list))
