;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

(defun cae-claude--generate-folder-name (task-description)
  "Generate a folder name from TASK-DESCRIPTION using Claude.
Uses the llm package to get a concise folder name."
  (let* ((prompt (format "Summarize this task in 3-5 words, using only alphanumeric characters and hyphens. Make it suitable for a folder name. Don't use any special characters. Task: %s" task-description))
         (response (llm-chat llm-refactoring-provider
                             (llm-make-chat-prompt prompt
                                                   :context "You are a helpful assistant that generates concise folder names."
                                                   :max-tokens 50
                                                   :temperature 0.7))))
    (replace-regexp-in-string " " "-" (string-trim response))))
;;(cae-claude--generate-folder-name "write a function to create a sandbox for claude")


(defun cae-claude--create-sandbox (sandbox-root task-description folder-name)
  "Create a Claude sandbox with FOLDER-NAME in SANDBOX-ROOT for TASK-DESCRIPTION."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (sandbox-dir (expand-file-name (format "%s--%s" timestamp folder-name)
                                        (expand-file-name sandbox-root)))
         (vterm-buffer-name (format "*claude:sandbox:%s*" folder-name)))

    (make-directory sandbox-dir t)
    (let ((default-directory sandbox-dir))
      (vterm-other-window vterm-buffer-name)
      ;; Send the task description directly as a quoted argument to claude
      (vterm-send-string (format "claude \"%s\"" task-description))
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
             (task-description (read-string "Enter task description: "
                                            nil 'cae-claude--task-history)))

        ;; Ensure sandbox root directory exists
        (unless (file-exists-p (expand-file-name sandbox-root))
          (make-directory (expand-file-name sandbox-root) t))

        ;; Generate folder name and create sandbox
        (let ((folder-name (cae-claude--generate-folder-name task-description)))
          (cae-claude--create-sandbox sandbox-root task-description folder-name)))

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
