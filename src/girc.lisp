(in-package :de.anvi.girc)

(crt:define-keymap girc-input-map ()
  ;; C-a = ^A = #\soh = 1 = start of heading
  (#\soh 'crt:move-start-of-line)

  ;; C-x = cancel = CAN = #\can
  (#\can 'crt:cancel)

  ;; C-r = reset = DC2 = #\dc2
  (#\dc2 'crt:reset)

  (:left  'crt:move-previous-char)
  (:right 'crt:move-next-char)

  (:backspace 'crt:delete-previous-char)
  (:delete    'crt:delete-next-char)

  (:insert (lambda (field)
             (crt:toggle-insert-mode field)))

  ;; toggle the display of the topic line
  (:f3 (lambda ()
         (setf conf:show-topic-line (not conf:show-topic-line))
         (show-topic-line conf:show-topic-line)))

  ;; toggle the display of the buffer list
  (:f4
   (lambda ()
     (setf conf:show-buffer-list (not conf:show-buffer-list))
     (if conf:show-buffer-list
         (if conf:show-buffer-line
             (show-buffer-line t)
             (show-buffer-column t))
         (progn
           (show-buffer-line nil)
           (show-buffer-column nil)))))

  ;; toggle the display of the buffer list as a line (tab bar)
  ("S-<F4>"
   (lambda ()
     (setf conf:show-buffer-line (not conf:show-buffer-line))
     (when conf:show-buffer-list
       (if conf:show-buffer-line
           (progn
             (show-buffer-line t)
             (show-buffer-column nil))
           (progn
             (show-buffer-line nil)
             (show-buffer-column t))))))

  ;; part current channel
  (:f7  (lambda ()
          (cmd:part)
          (redraw)))

  ;; kill buffer
  (:f8 (lambda ()
         (cmd:buffer "kill")
         (redraw)))

  ;; exit girc
  (:f12 'cmd:exit)

  (t 'crt:field-add-char)

  ;; connection.lisp
  ;; instead of processed during a nil event, this should be moved to a worker thread.
  ;; as soon as we connect to a server, pass the stream to a background thread
  ;; process-server-input then should just take read messages from the thread queue
  (nil 'handle-server-input)

  (:resize (lambda ()
             (crt:calculate-layout (layout *ui*))
             (setf (changedp (current-buffer)) t
                   (crt:width (input-field *ui*)) (crt:width (input-window *ui*)))
             (redraw)))

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
           (send t :quit)
           (redraw))))

(defun bind (key function)
  "Bind a key to the default girc keymap."
  (crt:bind (crt:find-keymap 'girc::girc-input-map) key function))

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
  girc irc.libera.chat

Instead of a hostname, the name of a server predefined in the init
file can be passed:

  girc libera

|#

(opts:define-opts
  (:name        :help
   :short       #\h
   :long        "help"
   :description "Show a short list of command-line options.")
  (:name        :version
   :short       #\v
   :long        "version"
   :description "Show version information.")
  (:name        :server
   :short       #\s
   :long        "server"
   :meta-var    "<server>"
   :arg-parser #'identity
   :description "Connect to the server given by its name or hostname.")
  (:name        :port
   :short       #\p
   :long        "port"
   :meta-var    "<port>"
   :arg-parser  #'parse-integer
   :description "Use <port> instead of the default port number.")
  (:name        :nick
   :short       #\n
   :long        "nick"
   :meta-var    "<nick>"
   :arg-parser #'identity
   :description "Nickname to use for the <server> connection.")
  (:name        :auth
   :short       #\a
   :long        "auth"
   :meta-var    "<user>:<pass>"
   :arg-parser #'identity
   :description "Authentication credentials to login to an account.")
  (:name        :method
   :short       #\m
   :long        "method"
   :meta-var    "<method>"
   :arg-parser #'identity
   :description "Method to use to login to the account.")

  ;; long options
  (:name        :ssl
   :long        "ssl"
   :description "Enable SSL encryption for the connection."))

(defun print-help-message ()
  (opts:describe
   :prefix "girc is a basic IRC client for the terminal."
   :suffix
"Optional free args:
  <server> Connect to the server given by its name or hostname.
  <nick>   Nickname to use for the <server> connection."
   :usage-of "girc"
   :args "[<server> [<nick>]]"))

(defun hostname-p (str)
  "If str is a hostname, return the domain to use as server name.

A name is considered a hostname if it contains dots, for example
irc.libera.chat.

If str is not a hostname, return nil."
  (when (position #\. str)
    (let* ((tokens (split-sequence:split-sequence #\. str))
           (count (length tokens))
           ;; return the second to last token, i.e. the domain.
           (name (nth (- count 2) tokens)))
      name)))

;; args handled after the ui was initialized and the init file loaded.
(defun handle-command-args ()
  ;; check if the exe is girc
  (when (search "girc" (nth 0 sb-ext:*posix-argv*))
    (let (server
          nick)
      (multiple-value-bind (opts free-args) (opts:get-opts)
        ;; check the free args (server nick) first
        (when free-args
          (if (= (length free-args) 2)
              ;; (server nick)
              (setq server (nth 0 free-args)
                    nick (nth 1 free-args))
              ;; (server)
              (setq server (nth 0 free-args))))
        ;; if the opts --server and --nick are given explicitely,
        ;; they override the free args.
        (when (getf opts :server)
          (setq server (getf opts :server)))
        (when (getf opts :nick)
          (setq nick (getf opts :nick))))
      ;; if the given server is a hostname, add it to the server list first
      (when (hostname-p server)
        (if nick
            (cmd:server "add" (hostname-p server) server :nickname nick)
            (cmd:server "add" (hostname-p server) server)))
      ;; connect the server by its name
      (let ((name (if (hostname-p server)
                      (hostname-p server)
                      server)))
        ;; connect directly only if a server has been given on the command line
        (when server
          (cmd:connect name))))))

(defun main ()
  "This is the main entry point. After quickloading girc, run the client with (girc:main)."
  (let ((*debugger-hook* #'(lambda (c h)
                             (declare (ignore h))
                             (finalize-user-interface *ui*)
                             (print c))))
    (multiple-value-bind (opts free-args) (opts:get-opts)
      (if (and opts
               (or (getf opts :help)
                   (getf opts :version)))
          ;; handle some args before starting the ui and loading the init file.
          ;; if --help or --version, print a message and don't start the ui.
          (progn
            (when (getf opts :help)
              (print-help-message))
            (when (getf opts :version)
              (princ "girc v0.0.1")
              (terpri)))

          ;; start ui, load init, then handle the remaining command args.
          (progn
            (setq *ui* (make-instance 'user-interface))

            ;; initialize the buffer lists BEFORE loading the init file because we maybe join channels or add buffers during init.
            ;; the buffer lists are part of the ui and should be initialized as early as possible
            (adjust-region *buffer-column-grid*
                           ;; new visible number of rows in the buffers window.
                           (- (crt:height (main-screen *ui*))
                              2 ; status and input line
                              (if (crt:find-node :topic (slot-value *ui* 'layout))
                                  1
                                  0)))
            (adjust-region *buffer-line-grid*
                           (floor (/ (crt:width (main-screen *ui*)) 12)))

            (load-init-file)
            (handle-command-args)
            (when conf:show-buffer-list
              (if conf:show-buffer-line
                  (show-buffer-line t)
                  (show-buffer-column t)))
            (redraw)
            ;; run the main event loop on the input field
            (crt:edit (input-field *ui*))
            ;; when we exit the main loop, cleanly finalize the ui.
            (finalize-user-interface *ui*))))))

#+(and sbcl sb-core-compression)
(defun make ()
  "Build the girc executable.

Requires libzstd.so to be available, produces an approx. 13 MB binary vs 56 MB uncompressed.

Builds the executable in the current directory, asdf:make puts it into src/

Omitting executable t produces a core which has to be run with sbcl --core."
  (sb-ext:save-lisp-and-die "girc" :toplevel #'girc:main
                                   :executable t
                                   :compression 22
                                   :save-runtime-options t))

(defun display-logo ()
  "Display the ASCII logo line by line."
  (dolist (i (split-sequence:split-sequence #\newline *girc-logo* :remove-empty-subseqs t))
    (echo t i)))

(defun display-info ()
  (let* ((box (make-instance 'dlg:msgbox
                             :title "girc v0.0.1"
                             :message *girc-logo*
                             :wrap-message nil
                             :width 45
                             :center t)))
    (crt:edit box)
    (close box))

  ;; if we add the ui to crt:main-stack, the main stack is automatically refreshed when the menu window is closed.
  (crt:touch (output-window *ui*))
  (refresh *ui*))
