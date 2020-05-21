(in-package :de.anvi.girc)

;; TODO 200412 allow maps to inherit bindings from other maps.
;; new bindings overwrite new bindings.
;; passed to the field during initialization
(crt:define-keymap girc-input-map
  ;; C-a = ^A = #\soh = 1 = start of heading
  (#\soh 'crt::accept)

   ;; C-x = cancel = CAN = #\can
  (#\can 'crt::cancel)

  ;; C-r = reset = DC2 = #\dc2
  (#\dc2 'crt::reset-field)

  (:left 'crt::move-previous-char)
  (:right 'crt::move-next-char)

  (:backspace 'crt::delete-previous-char)
  (:dc 'crt::delete-next-char)
   
  (:ic (lambda (field event &rest args)
         (setf (crt:insert-mode-p (crt:window field))
               (not (crt:insert-mode-p (crt:window field))))))

  (t 'crt::field-add-char)

  (nil 'handle-server-input)
  (#\newline 'handle-user-command)
   
  ;; C-w = 23 = #\etb = End of Transmission Block
  (#\etb (lambda (field event &rest args)
           (quit *current-connection*))))

;; after quickloading girc, start the client with (girc:run).
(defun run ()
  (setq *ui* (make-instance 'user-interface))
#|
  ;; instead of processed during a nil event, this should be moved to a worker thread.
  ;; as soon as we connect to a server, pass the stream to a background thread
  ;; process-server-input then should just take read messages from the thread queue
  (crt:bind (input-field ui) nil 'process-server-input)

  ;; TODO 191216: do not bind within the run definition.
  ;; bind them on the top level and use a keymap.
  ;; then associate the keymap with the input field.
        
  ;; the user input line is processed on every ENTER press.
  (crt:bind (input-field ui) #\newline 'handle-user-command)

  ;; C-w = 23 = #\etb (End of Transmission Block)
  ;; sends a quit message to the server. (replied by the server with an error message)
  (crt:bind (input-field ui) #\etb (lambda (f e &rest a) (quit *current-connection*)))

  ;; C-a = #\soh = accept will exit the event loop.
|#
  (crt:edit (input-field *ui*))
  
  (finalize-user-interface *ui*))

;; omitting executable t produces a core which has to be run with sbcl --core
(defun build ()
  "Build the girc executable."
  (sb-ext:save-lisp-and-die "girc" :toplevel #'girc:run :executable t :compression 9))
