;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

(defun cae-claude--generate-folder-name (task-description)
  "Generate a folder name based on TASK-DESCRIPTION using GPTel."
  (require 'gptel)
  (let ((prompt (format "Summarize this task in 3-5 words, using only alphanumeric characters and hyphens. Make it suitable for a folder name. Don't use any special characters. Task: %s" task-description)))
    (replace-regexp-in-string
     " " "_"
     (string-trim
      (gptel-request prompt
                    :system "You are a helpful assistant that generates concise folder names."
                    :stream nil)))))

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
             (task-description (read-string "Enter task description: "))
             (_ (unless (file-exists-p (expand-file-name sandbox-root))
                  (make-directory (expand-file-name sandbox-root) t)))
             (folder-name (cae-claude--generate-folder-name task-description))
             (timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (sandbox-dir (expand-file-name (format "%s-%s" timestamp folder-name) 
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
          (vterm-send-return)))
    
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
