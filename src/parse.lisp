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
    :documentation "Source of the message, can be a host or a nick!user@host.")

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
    :documentation "List of strings denoting the parameters."))

  (:documentation "Object representing a parsed IRC protocol message."))

;; print the parsed contents of raw-message in the repl.
;; http://stackoverflow.com/questions/7382122/lisp-how-to-override-default-string-representation-for-clos-class
;; http://clhs.lisp.se/Body/f_pr_obj.htm
;; http://clhs.lisp.se/Body/m_pr_unr.htm
(defmethod print-object ((obj irc-message) out)
  ;; unreadable objects are printed as <# xyz >
  (print-unreadable-object (obj out :type t)
    (format out "~S / ~S / ~S" (prefix obj) (command obj) (params obj))))

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
  "Take a string, return a list with the first word and the rest of the string."
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

;; (get-prefix-and-command ":prefix command p1 p2 p3 :text1 text2")
;; => ("prefix" "command" "p1 p2 p3 :text1 text2")
;; (get-prefix-and-command "command p1 p2 p3 :text1 text2")
;; => (NIL "command" "p1 p2 p3 :text1 text2")
;; the result list is fed to get-params
(defun get-prefix-and-command (str)
  "Take a string containing a raw irc message, return a list with the prefix, command and a string of parameters."
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

(defun split (str &optional (ch #\space))
  "Split the string on single and multiple spaces, trim all empty strings."
  (split-sequence:split-sequence ch str :remove-empty-subseqs t))

;; type of prefix can be nick-user-host or just a host.
;; (get-nick-user-host "nick!user@host") => ("nick" "user" "host")
;; (get-nick-user-host "leo!~leo@host-205-241-38-153.acelerate.net") => ("leo" "~leo" "host-205-241-38-153.acelerate.net")

(defun get-nick-user-host (prefix)
  (if (string-any "!@" prefix)
      (string-tokenize "!@" prefix)
      ;; if the prefix isnt in the form nick!user@host it is just a server hostname
      ;; :efnet.portlane.se 366 haom #EFNetNews :End of /NAMES list.
      (list nil nil prefix)))

(defun prefix-nick (irc-msg)
  (nth 0 (get-nick-user-host (prefix irc-msg))))

(defun prefix-user (irc-msg)
  (nth 1 (get-nick-user-host (prefix irc-msg))))

(defun prefix-host (irc-msg)
  (nth 2 (get-nick-user-host (prefix irc-msg))))

#|

CL-USER> (get-params '("prefix" "command" "p1 p2 p3 :text1 text2"))
("p1" "p2" "p3" "text1 text2")

CL-USER> (get-params '(NIL "command" "p1 p2 p3 :text1 text2"))
("p1" "p2" "p3" "text1 text2")

No prefix and no params:

CL-USER> (get-prefix-and-command "COMMAND :text")
(NIL "COMMAND" ":text")
CL-USER> (get-params *)
("text")

CL-USER> (get-prefix-and-command "ERROR :Closing Link: 78-2-83-238.adsl.net.com.com (Quit: haom)")
(NIL "ERROR" ":Closing Link: 78-2-83-238.adsl.net.com.com (Quit: haom)")
CL-USER> (get-params *)
("Closing Link: 78-2-83-238.adsl.net.com.com (Quit: haom)")

Params containing a colon:

;; instead of ":" as the start of text, we have to look for " :"
;; :tungsten.libera.chat 322 haoms ##:D 17 ::D  The Utopian Nightmare of Eternal Happiness

CL-USER> (get-prefix-and-command ":prefix command p1 p2 p:3 :text")
("prefix" "command" "p1 p2 p:3 :text")
CL-USER> (get-params *)
("p1" "p2" "p:3" "text")

|#

(defun get-params (lst)
  "Take a list returned by get-prefix-and-command, return a list of parameters."
  (let* ((str (third lst)))
    (if (char= (char str 0) #\:)
        ;; no parameters, only text: ":prefix command :text", ("prefix" "command" ":text")
        (list (subseq str 1))
        ;; we have to use search (substring) instead of position (single char) to exclude
        ;; the case where a parameter contains the colon character, for example
        ;; ":prefix command p1 p2 #p:3 :text1 text2"
        (let* ((pos (search " :" str))
               (middle (split-sequence:split-sequence #\space (subseq str 0 pos) :remove-empty-subseqs t))
               (trailing (if pos
                             (list (subseq str (+ 2 pos)))
                             nil)))
          (append middle trailing)))))

;; unused, see parse-raw-message
;; (parse ":prefix command p1 p2 p3 :text1 text2")
;; => ("prefix" "command" ("p1" "p2" "p3" "text1 text2"))
(defun parse2 (rawmsg)
  "Take a string containing an irc message, return a list with parsed components."
  (let* ((lst1 (get-prefix-and-command rawmsg))
         (lst2 (get-params lst1)))
    (append (list (car lst1) (cadr lst1))
            lst2)))

;; position #\: str
;; count #\: str
;; subseq str 0 5
;; coerce str 'list, coerce lst 'string

(defun parse-raw-message (rawmsg connection)
  "Take a string containing an irc message, return a irc message object."
  (let* ((lst1 (get-prefix-and-command rawmsg))
         (lst2 (get-params lst1)))
    (make-instance 'irc-message :connection connection
                                :rawmsg rawmsg :prefix (car lst1)
                                :command (cadr lst1)
                                :params lst2)))

;; Examples:
;; (parse-user-input "/hello there dear john")
;; => ("hello" . "there dear john")
;; (parse-user-input "hello there dear john")
;; => (NIL . "hello there dear john")
;; (parse-user-input "/hello")
;; => ("hello")
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

#|

If there are more tokens than parameters, ignore the additional tokens.

(parse-user-arguments '(a b c) "foo bar baz qux")
=> ("foo" "bar" "baz")

To catch the additional tokens, use the &rest keyword.

(parse-user-arguments '(a b &rest c) "foo bar baz qux")
=> ("foo" "bar" "baz qux")

If there are more parameters than tokens, they are optional, so return nil

(parse-user-arguments '(a b c) "foo")
=> ("foo" NIL NIL)

(parse-user-arguments '(a b &rest c) "foo bar")
=> ("foo" "bar" "")

Neither optional nor key arguments support default values yet.
So this doesnt work: &optional (a 1) or &key (min 10).
Default values have to be manually provided within the functions.

|#

(defun test-params ()
  ;; 1. too many arguments
  ;; arguments for which no parameters exist are ignored
  (format t "1.~A "
          (equalp (parse-user-arguments '(a b c) "foo bar baz qux")
                  (list "foo" "bar" "baz")))
  ;; 2. too few arguments
  ;; all parameters are optional, and return nil if too few arguments are passed
  (format t "2.~A "
          (equalp (parse-user-arguments '(a b c) "foo")
                  (list "foo" nil nil)))
  ;; 3. this is equivalent to making b and c &optional, which is implied for all vars.
  (format t "3.~A "
          (equalp (parse-user-arguments '(a &optional b c) "foo")
                  (list "foo" nil nil)))
  ;; 3. too few arguments
  (format t "4.~A "
          (equalp (parse-user-arguments '(a) "")
                  (list nil)))
  ;; 4. rest parameter
  ;; they are collected, if the last parameter is &rest
  (format t "5.~A "
          (equalp (parse-user-arguments '(a b &rest c) "foo bar baz qux")
                  (list "foo" "bar" "baz qux")))
  ;; 5. rest parameter but no rest arguments
  ;; a missing rest argument returns nothing, so lisp will return an empty list.
  (format t "6.~A "
          (equalp (parse-user-arguments '(a b &rest c) "foo bar")
                  (list "foo" "bar")))
  ;; 6. rest parameter but no rest arguments
  (format t "7.~A "
          (equalp (parse-user-arguments '(&rest a) "")
                  nil))
  ;; 7. no arguments
  (format t "8.~A "
          (equalp (parse-user-arguments '(a b c &rest d) "")
                  (list nil nil nil)))
  ;; 8. no parameters
  ;; returns nil, or an empty list
  (format t "9.~A "
          (equalp (parse-user-arguments '() "foo bar baz")
                  nil))
  ;; 9. keyword parameters
  ;; instead of returning all rest args in a single string, when the &key
  ;; keyword is present in the lambda list, the args are parsed and returned as :k v :k v
  (format t "10.~A "
          (equalp (parse-user-arguments '(a b &rest c &key d e f) "a b :c 1 :d 2")
                  (list "a" "b" :C 1 :D 2)))
  ;; 10. the same as 9. but without they &key keyword.
  ;; here again the rest is returned in a single string.
  (format t "11.~A "
          (equalp (parse-user-arguments '(a b &rest c) "a b :c 1 :d 2")
                  (list "a" "b" ":c 1 :d 2")))
  ;; 11. the same as 9, but without the &rest keyword
  (format t "12.~A "
          (equalp (parse-user-arguments '(a b &key d e f) "a b :c 1 :d 2")
                  (list "a" "b" :C 1 :D 2)))
  ;; 12. only keyword args, no required args
  (format t "13.~A "
          (equalp (parse-user-arguments '(&key d e f) ":c 1 :d 2")
                  (list :C 1 :D 2)))
  ;; 13. keyword args with default arguments
  (format t "14.~A "
          (equalp (parse-user-arguments '(&key (a 1) b (c t)) ":b 2")
                  (list :B 2)))
  ;; 15. optional args with default arguments
  (format t "15.~A "
          (equalp (parse-user-arguments '(a &optional (b 1) (c 2)) "foo")
                  (list "foo" nil nil)))
  ;; 16. all keywords, optional args
  (format t "16.~A "
          (equalp (parse-user-arguments
                   '(a &optional (b 1) (c 2) &key (d 3))
                   "a b c :d 2 :r1 r1")
                  (list "a" "b" "c" :D 2 :R1 "r1"))))

;; (parse-keyword-args ":a 1 :b 22 :c hello :d nil :e t")
;; => (:A 1 :B 22 :C "hello" :D NIL :E T)
(defun parse-keyword-args (args)
  "Take a string with keyword arguments, parse it to a lisp list.

Recognized objects are keywords, integers, t and nil. Everyting else
is left as a string."
  (when args
    (loop for i in (split-sequence:split-sequence #\space args)
          if (char= (char i 0) #\:)
            collect (intern (subseq (string-upcase i) 1) :keyword)
          else if (or (string= i "t") (string= i "nil"))
            collect (read-from-string i)
          else if (every #'digit-char-p i)
            collect (parse-integer i)
          else
            collect i)))

(defun parse-user-arguments (lbd str)
  "Take a lambda list and a string, return a list of destructured tokens.

Only as many tokens are parsed as there are required parameters.

The remaining redundant tokens are silently ignored, unless &key or
&rest keywords are present:

If &key is present, the remaining tokens are parsed as alternating keys
and values.

If &rest is present, it does not collect all remaining arguments in a
list like it would in Lisp, but collects them in a single string, in
order to mirror the irc trailing parameter that may contain spaces.

&key and &rest can not be used at the same time.

All parameters are treated as optional whether the keyword &optional
is present or not, so if there are more parameters than arguments,
nil will be returned for each.

The list of parsed tokens is then passed to the command handler."
  (let* ((restp (member '&rest     lbd))
         (keyp  (member '&key      lbd))
         (optp  (member '&optional lbd))
         ;; remove the key keyword and all arguments.
         ;; (a b &optional c &rest d &key e f) => (a b &optional c &rest d)
         (lbd1 (if keyp
                   (subseq lbd 0 (position '&key lbd))
                   lbd))
         ;; remove &rest and the rest arg
         ;; (a b &optional c &rest d) => (a b &optional c)
         (lbd2 (if restp
                   (subseq lbd1 0 (position '&rest lbd1))
                   lbd1))
         ;; all arguments are optional anyway, remove the &optional keyword
         ;; (a b &optional c) => (a b c)
         ;; we still can mark args optional, but this only affects the
         ;; function when it is called from lisp.
         (req (if optp
                  (remove '&optional lbd2)
                  lbd2))
         ;; number of required (and optional) arguments
         (nreq (length req)))
    (multiple-value-bind (reqs pos)
        ;; tokenize the first n required (and optional) args
        ;; reqs = list of parsed required (and optional) arguments
        ;; pos = end of nth required arg, start of rest or key
        (split-sequence:split-sequence #\space str :count nreq :remove-empty-subseqs t)
      ;; if there are no args, split-sequence returns an empty string, but we want nil
      (let* ((reqs (if (and (= (length reqs) 1)        ; a list with one empty string ("")
                            (= (length (car reqs)) 0)) ; empty string = length 0
                       ;; use nil instead of one empty string
                       nil
                       reqs))
             ;; if the rest string is empty
             (rest-string (if (= 0 (length (subseq str pos)))
                              ;; return nil in its place.
                              nil
                              ;; if there is a rest, put it all in one string.
                              (subseq str pos)))
             ;; if &key was given, parse the rest into keyword args
             (rest (if keyp
                       ;; numbers, t, nil and keywords are parsed from strings.
                       (parse-keyword-args rest-string)
                       (if (null rest-string)
                           ;; if the rest string is empty, dont return (nil), but just nil.
                           nil
                           (list rest-string)))))
        (if (or restp keyp)
            ;; we have rest or key arguments.
            (if (> nreq (length reqs))
                (append reqs (make-list (- nreq (length reqs))) rest)
                (append reqs rest))
            ;; we have only required and optional arguments.
            (if (> nreq (length reqs))
                (append reqs (make-list (- nreq (length reqs))))
                reqs))))))

#|

Additional parameters are silently ignored:

CL-USER> (parse-argument-bind (a b c) "foo bar baz qux"
           (list a b c))
("foo" "bar" "baz")

The rest arg is put into a single string:

CL-USER> (parse-argument-bind (a b &rest c) "foo bar :baz 2 :d 1"
           (list a b c))
("foo" "bar" (":baz 2 :d 1"))

If there is a key in addition to rest, the rest string is parsed:

CL-USER> (parse-argument-bind (a b &rest c &key d baz) "foo bar :baz 2 :d 1"
           (list a b c d baz))
("foo" "bar" (:BAZ 2 :D 1) 1 2)

If the keyword parameters have default arguments, they are considered.

CL-USER> (parse-argument-bind (&key (a 1) b (c t)) ":b 2"
           (list a b c))
(1 2 T)

|#

(defmacro parse-argument-bind (lbd str &body body)
  "Parse space-separated words from string and bind them to the given parameters.

If the last keyword is &rest, collect all remaining words into it as a
single string.

If the last keyword is &key, parse the rest string as if it is a sequence of
keys and values :k1 v1 :k2 v2.

Note that if we have a &rest but no &key, the underlying destructuring-bind
still puts that single string into a list."
  `(destructuring-bind ,lbd
       (parse-user-arguments ',lbd ,str)
     ,@body))

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

;; Example: (split-args "a b c") => ("a" "b" "c")
(defun split-args (args)
  "Take a string, return a list of substrings split on space as the delimiter."
  (values (split-sequence:split-sequence #\space args :remove-empty-subseqs t)))

(defun string-join (delimiter &rest args)
  "Take a list of string, join them with the given delimiter (char/string)."
  (format nil
          ;;"~{~A~^ ~}"
          (concatenate 'string
                       "~{~A~^"
                       (princ-to-string delimiter)
                       "~}")
          args))

;; Example: (join-args " a " "b " " c") => "a b c"
(defun join-args (&rest args)
  "Take a list of strings, join them with the space char as delimiter.

Trim spaces around the given arguments, if there are any."
  (apply #'string-join #\space
         (mapcar #'(lambda (i)
                     (string-trim " " i))
                 args)))

;; Example: (string-nth 2 "a b c d e f") => "c"
(defun string-nth (n str)
  (nth n (split-args str)))
(defun ntharg (n args)
  (nth n (split-args args)))

;; (arglen "a b c") => 3
;; (arglen "") => 0
(defun arglen (args)
  "Return the length of the arguments list (= number of passed arguments)."
  (length (split-args args)))

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
  (nthcdr n (split-args str)))

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

;; also see cl-ppcre:regex-replace-all
;; CL-USER> (ppcre:regex-replace-all "\\$\\{a\\}" "faa baa caa ist ${a} qux" "WORLD")
;; "faa baa caa ist WORLD qux"

;; Example: (replace-all "a $B c d B e f b" "$B" "nick") => "a nick c d B e f b"
(defun replace-all (string part replacement &key (test #'char=))
  "Return a new string in which all the occurrences of the part is replaced with replacement."
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

(defun channelp (str)
  "Return t if str is a valid channel name."
  (and (not (= (length str) 0))
       (>= (length str) 2)
       (char= #\# (char str 0))))
