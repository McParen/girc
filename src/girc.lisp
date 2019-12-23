(in-package :de.anvi.girc)

(defparameter *user-commands* nil)

(defparameter *ui* nil)

;; Handler of the nil event during a non-blocking edit of the field.
;; TODO: check whether win is non-blocking before assuming it
(defun process-server-input (field event)
  ;; do not process if a connection has not been established first.
  (when *current-connection*
    (let ((raw-message (read-raw-message *current-connection*))) ; see connection.lisp
      (if raw-message
          ;; after anything is written to the output window, return the cursor to the input window.
          (crt:save-excursion (input-window *ui*)
            ;; message handline writes to the screen, so it has to happen in the main thread
            (handle-message raw-message *current-connection*)) ; see event.lisp
          (sleep 0.01)))))

(defun handle-user-command (field event &rest args1)
  (let ((input-string (crt:value field)))
    (when input-string
      (apply #'crt:reset-field field event args1)
      (destructuring-bind (cmd . args) (parse-user-input input-string)
        ;; TODO: what to do when there is no command in the input?
        (when cmd
          (let ((fun (get-command cmd))) ; see command.lisp
            (if fun
                (funcall fun cmd args)
                (funcall (get-command t) cmd args))))))))

(defun display (template &rest args)
  "Display the format template on the output window."
  (let ((wout (output-window *ui*)))
    (apply #'format wout template args)
    (crt:refresh wout)))

;; passed to the field during initialization
(crt:define-keymap 'girc-input-map
  (list

   ;; C-a = ^A = #\soh = 1 = start of heading
   #\soh      'crt::accept

   ;; C-x = cancel = CAN = #\can
   #\can      'crt::cancel

   ;; C-r = reset = DC2 = #\dc2
   #\dc2      'crt::reset-field

   :left      'crt::move-previous-char
   :right     'crt::move-next-char

   :backspace 'crt::delete-previous-char
   :dc        'crt::delete-next-char
   
   :ic        (lambda (field event &rest args)
                (setf (crt:insert-mode-p (crt:window field))
                      (not (crt:insert-mode-p (crt:window field)))))

   t          'crt::field-add-char

   nil        'process-server-input
   #\newline  'handle-user-command
   
   ;; C-w = 23 = #\etb = End of Transmission Block
   #\etb      (lambda (field event &rest args)
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
