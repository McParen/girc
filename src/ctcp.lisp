(in-package :de.anvi.girc)

(defparameter *ctcp-commands* '("ACTION")
  "Supported CTCP commands.")

(defun ctcp-message-p (string)
  "Return t if the text is a valid CTCP message."
  (let ((len (length string)))
    (and (> len 1)
         (char= (char string        0) #\soh)
         (char= (char string (1- len)) #\soh))))

;; (make-ctcp-message "ACTION does it!") => "^AACTION does it!^A"
(defun make-ctcp-message (string)
  (format nil "~A~A~A" #\soh string #\soh))

;; (parse-ctcp-message "^AACTION does it!^A") => ("ACTION" . "does it!")
(defun parse-ctcp-message (string)
  "Take a ctcp message, return a pair with the command and the args string."
  (let* ((str (subseq string 1 (1- (length string))))
         (pos (position #\space str)))
    (if pos
        ;; if we have a space, we have a command and arguments.
        (cons (subseq str 0 pos) (subseq str (1+ pos)))
        ;; otherwise we only have a command.
        (cons (subseq str 0) nil))))

(defun ctcp-command-p (cmd)
  "Take a string with a received CTCP command, return t if supported."
  (if (member (string-upcase cmd) *ctcp-commands* :test #'string=)
      t
      nil))

(defun ctcp-command-name (cmd)
  "Take the parsed command string, return a Lisp keyword."
  (values (intern (string-upcase cmd) "KEYWORD")))
