(in-package :de.anvi.girc)

;; put every read char into an array of max 512 length.

;; check if msg is max 512 chars.

;; check if message ends in crlf.

;; if msg starts with a colon, then cut to the second colon
;; if msg doesnt start with a colon, then cut to the first colon.

;; TODO: ":a b c g :d e f" => (":a" "b" "c" "g" ":d" "e" "f") => ircmsg object

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

;; type of prefix: nick-user-host or just a host

(defun get-nick-user-host (prefix)
  (if (string-any "!@" prefix)
      (string-tokenize "!@" prefix)
      (list prefix "" "")))

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
(defun parse2 (ircstr)
  "Take a string containing an irc message, return a list with parsed components."
  (let* ((lst1 (get-prefix-and-command ircstr))
         (lst2 (get-params-and-text lst1)))
    (append (list (car lst1) (cadr lst1)) lst2)))

;; position #\: str
;; count #\: str
;; subseq str 0 5
;; coerce str 'list, coerce lst 'string

(defun parse (ircstr)
  "Take a string containing an irc message, return a irc message object."
  (let* ((lst1 (get-prefix-and-command ircstr))
         (lst2 (get-params-and-text lst1)))
    (make-instance 'ircmsg :prefix (car lst1) :command (cadr lst1) :params (car lst2) :text (cadr lst2))))
