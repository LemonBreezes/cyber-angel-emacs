;;; cae/notifications/autoload/alert.el -*- lexical-binding: t; -*-

;; A flag to avoid recursive/advice reentry.
(defvar cae-notifications-advice-active nil
  "Non-nil if our notifications-notify advice is already active.
This prevents our advice from recursively intercepting internal calls.")

;; A little helper to remove a property from a plist.
(defun cae-plist-delete (plist key)
  "Return a copy of PLIST with KEY (and its value) removed."
  (let (result)
    (while plist
      (if (eq (car plist) key)
          (setq plist (cddr plist))
        (push (pop plist) result)
        (push (pop plist) result)))
    (nreverse result)))

;;;###autoload
(defun cae-notifications-notify-advice (orig-fun &rest args)
  "Advice for notifications-notify to adapt its keyword arguments for alert.
This advice transforms as many keywords as possible:

  • :body        → :message
  • :app-icon    → :icon
  • :urgency     → if :severity is not set, map its value to a severity
  • :actions, :on-action, :on-close, :timeout, and :replaces-id
     are removed from the top level and merged into the :data plist.
Additionally, any keys not part of the recognized alert parameter set
are merged into :data so that none of the diagnostic or extra keys is lost.
If the advice is active (e.g. when alert internally calls notifications-notify),
the original function is used."
  (if cae-notifications-advice-active
      (apply orig-fun args)
    (let ((cae-notifications-advice-active t))
      (let* ((plist (apply #'list args))
             (info  plist))
        ;; 1. Convert :body to :message (alert expects a message).
        (when (plist-get info :body)
          (setq info (plist-put info :message (plist-get info :body)))
          (setq info (cae-plist-delete info :body)))
        ;; 2. Map :app-icon to :icon if not already provided.
        (when (plist-get info :app-icon)
          (unless (plist-get info :icon)
            (setq info (plist-put info :icon (plist-get info :app-icon))))
          (setq info (cae-plist-delete info :app-icon)))
        ;; 3. If there’s an :urgency key and no :severity, map urgency to severity.
        (when (and (plist-get info :urgency) (not (plist-get info :severity)))
          (setq info (plist-put info :severity
                                (pcase (plist-get info :urgency)
                                  ((or "critical" 'critical) 'urgent)
                                  ((or "low" 'low) 'low)
                                  ((or "normal" 'normal) 'normal)
                                  (_ 'normal)))))
        (setq info (cae-plist-delete info :urgency))
        ;; 4. Ensure a default title if none is provided.
        (unless (plist-get info :title)
          (setq info (plist-put info :title "Emacs Notification")))
        ;; 5. Ensure a default severity if still missing.
        (unless (plist-get info :severity)
          (setq info (plist-put info :severity 'normal)))
        ;; 6. For some extra keys (like :actions, :on-action, :on-close, :timeout, :replaces-id),
        ;;    remove them from the plist and merge them into the :data key.
        (dolist (key '(:actions :on-action :on-close :timeout :replaces-id))
          (when (plist-member info key)
            (let ((val (plist-get info key)))
              (setq info (cae-plist-delete info key))
              (setq info (plist-put info :data
                                    (append (plist-get info :data) (list key val))))))

          ;; 7. In case any extra keys remain (i.e. keys that are not part of alert’s parameters),
          ;;    merge them into the :data property.  The keys recognized by alert are:
          (let ((allowed-keys '(:message :title :icon :severity :category :buffer
                                :mode :persistent :never-persist :id :style :data))
                (final-info '())
                (extras '())
                (tmp info))
            (while tmp
              (let ((key (pop tmp))
                    (val (pop tmp)))
                (if (memq key allowed-keys)
                    (setq final-info (plist-put final-info key val))
                  (setq extras (plist-put extras key val)))))
            (when extras
              (setq final-info (plist-put final-info :data
                                          (append (plist-get final-info :data) extras))))
            ;; 8. Finally, call alert with the message (extracted from :message)
            ;;    and the rest of the keys as keyword arguments.
            (apply #'alert (cons (plist-get final-info :message)
                                 (cae-plist-delete final-info :message)))))))))

(defun cae-notifications-call-process-advice (orig-fun program &optional infile destination display &rest args)
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
(defun cae-notifications-wrap-async-call-process-a (oldfun &rest args)
  (advice-add #'call-process :around #'cae-ednc-call-process-advice)
  (unwind-protect (apply oldfun args)
    (advice-remove #'call-process #'cae-ednc-call-process-advice)))
