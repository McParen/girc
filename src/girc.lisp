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
  
  (#\newline 'handle-user-command)

  ;; TODO 201122 how to bind irc command directly without having to wrap them in a lambda
  ;; C-w = 23 = #\etb = End of Transmission Block
  ;; sends a quit message to the server. (replied by the server with an error message)
  (#\etb (lambda (field event &rest args)
           (declare (ignore field event args))
           (quit *current-connection*))))

;; after quickloading girc, start the client with (girc:run).
(defun run ()
  (setq *ui* (make-instance 'user-interface))
  (display-logo)
  (crt:edit (input-field *ui*))
  (finalize-user-interface *ui*))

;; omitting executable t produces a core which has to be run with sbcl --core
(defun build ()
  "Build the girc executable."
  (sb-ext:save-lisp-and-die "girc" :toplevel #'girc:run :executable t :compression 9))

(defun display-logo ()
  (echo "            _____  _____    _____ ")
  (echo "           |_   _||  __ \\  / ____|")
  (echo "      __ _   | |  | |__) || |     ")
  (echo "     / _` |  | |  |  _  / | |     ")
  (echo "    | (_| | _| |_ | | \\ \\ | |____ ")
  (echo "     \\__, ||_____||_|  \\_\\ \\_____|")
  (echo "   |    | |")
  (echo "   |\\___/ |")
  (echo "    \\____/")
  (echo ""))
