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
  (#\dc2 'crt::reset-field)

  (:left 'crt::move-previous-char)
  (:right 'crt::move-next-char)

  (:backspace 'crt::delete-previous-char)

  ;; TODO 201122 rename to :delete-char :insert-char

  (:dc 'crt::delete-next-char)

  (:ic (lambda (field event &rest args)
         (declare (ignore event args))
         (crt:toggle-insert-mode field)))

  ;; TODO 201122 only graphic chars should be added, what about :up?
  (t 'crt::field-add-char)

  ;; connection.lisp
  ;; instead of processed during a nil event, this should be moved to a worker thread.
  ;; as soon as we connect to a server, pass the stream to a background thread
  ;; process-server-input then should just take read messages from the thread queue
  (nil 'handle-server-input)

  ;; command.lisp
  (#\newline 'handle-user-command)

  ;; ^N (14)
  (#\so 'select-next-buffer)

  ;; TODO 201122 how to bind irc command directly without having to wrap them in a lambda
  ;; C-w = 23 = #\etb = End of Transmission Block
  ;; sends a quit message to the server. (replied by the server with an error message)
  (#\etb (lambda (field event &rest args)
           (declare (ignore field event args))
           (send t :quit))))

;; after quickloading girc, start the client with (girc:run).
(defun run ()
  (setq *ui* (make-instance 'user-interface))
  (update-status)
  ;; run the main event loop on the input field
  (crt:edit (input-field *ui*))
  (finalize-user-interface *ui*))

;; omitting executable t produces a core which has to be run with sbcl --core
(defun build ()
  "Build the girc executable."
  (sb-ext:save-lisp-and-die "girc" :toplevel #'girc:run :executable t :compression 9))

(defparameter *girc-logo*
"           _____  _____    _____ 
          |_   _||  __ \\  / ____|
     __ _   | |  | |__) || |     
    / _` |  | |  |  _  / | |     
   | (_| | _| |_ | | \\ \\ | |____ 
    \\__, ||_____||_|  \\_\\ \\_____|
  |    | |
  |\\___/ |        A very basic IRC client.
   \\____/")

(defun display-logo ()
  (loop for i in (split-sequence:split-sequence #\newline *girc-logo* :remove-empty-subseqs t) do
    (echo t i)))

(defun display-info ()
  (let* ((box (make-instance 'crt:msgbox
                             :title "gIRC v0.0.1"
                             :message *girc-logo*
                             :message-wrap nil
                             :dimensions '(15 50)
                             :center t)))
    (crt:edit box)
    (close box))

  ;; if we add the ui to crt:main-stack, the main stack is automatically refreshed when the menu window is closed.
  (crt:touch (output-window *ui*))
  (refresh *ui*))
