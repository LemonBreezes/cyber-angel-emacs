;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

(defun cae-claude--create-sandbox (sandbox-root task-description folder-name)
  "Create a Claude sandbox with FOLDER-NAME in SANDBOX-ROOT for TASK-DESCRIPTION."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (sandbox-dir (expand-file-name (format "%s--%s" timestamp folder-name)
                                        (expand-file-name sandbox-root)))
         (vterm-buffer-name (format "*claude:sandbox:%s*" folder-name)))
    
    (make-directory sandbox-dir t)
    (let ((default-directory sandbox-dir))
      (vterm-other-window vterm-buffer-name)
      (vterm-send-string "claude")
      (vterm-send-return)
      ;; Wait a moment for Claude to initialize
      (sit-for 0.5)
      (vterm-send-string task-description)
      (vterm-send-return))))

;;;###autoload
(defun cae-claude-code (&optional create-sandbox)
  "Open a Claude AI terminal in a vterm buffer.
If CREATE-SANDBOX is non-nil (with prefix argument), create a sandbox
environment in ~/src/claude-sandbox with a timestamped folder based on
a task description, and start Claude there.
Otherwise, open Claude for the current project."
  (interactive "P")
  (require 'vterm)
  
  (if create-sandbox
      (let* ((sandbox-root "~/src/claude-sandbox")
             (task-description (read-string "Enter task description: ")))
        
        ;; Ensure sandbox root directory exists
        (unless (file-exists-p (expand-file-name sandbox-root))
          (make-directory (expand-file-name sandbox-root) t))
        
        ;; Request folder name from GPTel and create sandbox in callback
        (require 'gptel)
        (let ((prompt (format "Summarize this task in 3-5 words, using only alphanumeric characters and hyphens. Make it suitable for a folder name. Don't use any special characters. Only respond with that one folder name and no other text whatsoever. Task: %s" task-description)))
          (gptel-request 
              prompt
            :system "You are a helpful assistant that generates concise folder names."
            :stream nil
            :callback (lambda (response info)
                        (when response
                          (let ((folder-name (replace-regexp-in-string
                                              " " "-"
                                              (string-trim response))))
                            (cae-claude--create-sandbox
                             sandbox-root task-description folder-name)))))))
    
    ;; Original project-based behavior
    (let* ((project-root (doom-project-root))
           (vterm-buffer-name (format "*claude:%s*" project-root))
           (default-directory (or project-root default-directory))
           (existing-buffer (get-buffer vterm-buffer-name)))
      (if existing-buffer
          (pop-to-buffer existing-buffer)
        (vterm-other-window vterm-buffer-name)
        (vterm-send-string "claude")
        (vterm-send-return)))))
