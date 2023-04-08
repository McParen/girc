(in-package :de.anvi.girc)

;; TODO 200412 allow maps to inherit bindings from other maps.
;; new bindings overwrite new bindings.
;; passed to the field during initialization

(crt:define-keymap girc-input-map
  ;; C-a = ^A = #\soh = 1 = start of heading
  (#\soh 'crt::accept)

  ;; TODO 201121 use caret notation instead of "soh", etc.

   ;; C-x = cancel = CAN = #\can
  (#\can 'crt::cancel)

  ;; C-r = reset = DC2 = #\dc2
  (#\dc2 'crt::reset)

  (:left 'crt::move-previous-char)
  (:right 'crt::move-next-char)

  (:backspace 'crt::delete-previous-char)

  ;; TODO 201122 rename to :delete-char :insert-char

  (:dc 'crt::delete-next-char)

  (:ic (lambda (field)
         (crt:toggle-insert-mode field)))

  ;; TODO 201122 only graphic chars should be added, what about :up?
  (t 'crt::field-add-char)

  ;; connection.lisp
  ;; instead of processed during a nil event, this should be moved to a worker thread.
  ;; as soon as we connect to a server, pass the stream to a background thread
  ;; process-server-input then should just take read messages from the thread queue
  (nil 'handle-server-input)

  (:resize (lambda ()
             (crt:calculate-layout (layout *ui*))
             (setf (changedp (current-buffer)) t
                   (crt:width (input-field *ui*)) (crt:width (input-window *ui*)))
             (update-topic)
             (update-output)
             (update-status)
             (refresh *ui*)))

  ;; command.lisp
  (#\newline 'handle-user-command)

  ;; ^N (14)
  (#\so 'select-next-buffer)
  ;; ^P (16)
  (#\Dle 'select-previous-buffer)

  ;; TODO 201122 how to bind irc command directly without having to wrap them in a lambda
  ;; C-w = 23 = #\etb = End of Transmission Block
  ;; sends a quit message to the server. (replied by the server with an error message)
  (#\etb (lambda ()
           (send t :quit))))

(defun load-init-file ()
  (let ((init (merge-pathnames (user-homedir-pathname) ".gircrc")))
    (when (probe-file init)
      (let ((*package* (find-package :de.anvi.girc)))
        (load init)))))

#|

If a server hostname and nickname are passed to the girc binary as
command line arguments, that server is immediately connected:

  girc [hostname [nickname]]

For example:

  girc irc.libera.chat haoms

|#

(defun parse-posix-argv ()
  ;; only parse if called from the girc binary with 1 or 2 args.
  (when (and (search "girc" (nth 0 sb-ext:*posix-argv*))
             (or (= (length sb-ext:*posix-argv*) 2)
                 (= (length sb-ext:*posix-argv*) 3)))
    (case (length sb-ext:*posix-argv*)
      ;; girc irc.server.org
      (2 (let* ((host (nth 1 sb-ext:*posix-argv*))
                (tokens (split-sequence:split-sequence #\. host))
                (len (length tokens))
                ;; parse out the host domain as the server name
                (name (nth (- len 2) tokens)))
           (cmd:server "add" name host)
           (cmd:connect name)))
      ;; girc irc.server.org nick
      (3 (let* ((host (nth 1 sb-ext:*posix-argv*))
                (nick (nth 2 sb-ext:*posix-argv*))
                (tokens (split-sequence:split-sequence #\. host))
                (len (length tokens))
                (name (nth (- len 2) tokens)))
           (cmd:server "add" name host :nickname nick)
           (cmd:connect name))))))

(defun main ()
  "This is the main entry point. After quickloading girc, run the client with (girc:main)."
  (let ((*debugger-hook* #'(lambda (c h)
                             (declare (ignore h))
                             (finalize-user-interface *ui*)
                             (print c))))
    (setq *ui* (make-instance 'user-interface))
    (refresh *ui*)
    (load-init-file)
    (parse-posix-argv)
    (update-status)
    ;; run the main event loop on the input field
    (crt:edit (input-field *ui*))
    (finalize-user-interface *ui*)))

#+(and sbcl sb-core-compression)
(defun build ()
  "Build the girc executable.

Requires libzstd.so to be available, produces an approx. 13 MB binary vs 56 MB uncompressed.

Builds the executable in the current directory, asdf:make puts it into src/

Omitting executable t produces a core which has to be run with sbcl --core."
  (sb-ext:save-lisp-and-die "girc" :toplevel #'girc:main :executable t :compression 22))

(defun display-logo ()
  "Display the ASCII logo line by line."
  (loop for i in (split-sequence:split-sequence #\newline *girc-logo* :remove-empty-subseqs t) do
    (echo t i)))

(defun display-info ()
  (let* ((box (make-instance 'dlg:msgbox
                             :title "gIRC v0.0.1"
                             :message *girc-logo*
                             :wrap-message nil
                             :width 45
                             :center t)))
    (crt:edit box)
    (close box))

  ;; if we add the ui to crt:main-stack, the main stack is automatically refreshed when the menu window is closed.
  (crt:touch (output-window *ui*))
  (refresh *ui*))
