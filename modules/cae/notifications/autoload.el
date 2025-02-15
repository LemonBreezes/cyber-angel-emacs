;;; cae/misc-applications/autoload/ednc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ednc-show-notifications ()
  (interactive)
  (require 'ednc)
  (let ((buf (get-buffer-create ednc-log-name)))
    (pop-to-buffer buf)))

;;;###autoload
(defun cae-ednc-show-notification-in-buffer (old new)
  (let ((name (format "*Notification %d*" (ednc-notification-id (or old new)))))
    (with-current-buffer (get-buffer-create name)
      (if new (let ((inhibit-read-only t))
                (if old (erase-buffer) (ednc-view-mode))
                (set-buffer-multibyte nil)
                (insert (ednc-format-notification new t))
                (display-buffer (current-buffer)))
        (kill-buffer)))))

;;;###autoload
(defun cae-ednc-dismiss-all-notifications ()
  (interactive)
  (cl-mapcar (lambda (notification)
               (ednc-dismiss-notification notification))
             (ednc-notifications)))

;;;###autoload
(defun cae-ednc-toggle-notifications ()
  (interactive)
  (require 'ednc)
  (cond ((ednc-notifications)
         (cae-ednc-dismiss-all-notifications))
        ((get-buffer-window ednc-log-name)
         (delete-window (get-buffer-window ednc-log-name)))
        (t (cae-ednc-show-notifications))))

(defun cae-ednc-call-process-advice (orig-fun program &optional infile destination display &rest args)
  "Advice function to make `call-process` run asynchronously using `start-process`."
  ;; Generate a unique process name
  (let* ((process-name (generate-new-buffer-name (concat "async-" (file-name-nondirectory program))))
         ;; Determine the buffer based on DESTINATION
         (buffer (cond
                  ((eq destination t) (current-buffer))
                  ((bufferp destination) destination)
                  ((stringp destination) (get-buffer-create destination))
                  ((and (listp destination)
                        (eq (car destination) :file))
                   ;; For DESTINATION of the form (:file FILE), create a temporary buffer
                   (generate-new-buffer (concat "*" process-name "*")))
                  (t nil))) ;; For nil or 0, output is discarded
         ;; Use a pipe for the process communication
         (process-connection-type nil)
         ;; Start the process asynchronously
         (proc (apply 'start-process process-name buffer program args)))
    ;; If INFILE is specified, send its contents to the process
    (when infile
      (with-temp-buffer
        (insert-file-contents infile)
        (process-send-region proc (point-min) (point-max)))
      (process-send-eof proc)) ;; Signal that we've finished sending input
    ;; Handle DESTINATION being (:file FILE)
    (when (and (listp destination)
               (eq (car destination) :file))
      (let ((file (cadr destination)))
        ;; Set up a sentinel to write output to FILE when the process exits
        (set-process-sentinel
         proc
         (lambda (proc event)
           (when (eq (process-status proc) 'exit)
             (with-current-buffer (process-buffer proc)
               (write-region (point-min) (point-max) file nil 'quiet))
             (kill-buffer (process-buffer proc)))))))
    ;; If DISPLAY is non-nil and a buffer is associated, display the buffer
    (when (and display buffer)
      (display-buffer buffer))
    ;; Since the process is asynchronous, we return nil
    nil))

;;;###autoload
(defun cae-ednc-wrap-async-call-process-a (oldfun &rest args)
  (advice-add #'call-process :around #'cae-ednc-call-process-advice)
  (unwind-protect (apply oldfun args)
    (advice-remove #'call-process #'cae-ednc-call-process-advice)))

;;;###autoload
(defun cae-ednc-stack-notifications (&optional hide)
  (mapconcat (lambda (notification)
               (let ((app-name (ednc-notification-app-name notification)))
                 (unless (member app-name hide)
                   (push app-name hide)
                   (ednc-format-notification notification))))
             (ednc-notifications) ""))

;;;###autoload
(defun cae-ednc-load-h ()
  (and (require 'dbus nil t)
       (not (getenv "INSIDE_EXWM"))   ; In EXWM I prefer using Dunst.
       cae-dbus-notifications-supported-p
       (ednc-mode +1)))
