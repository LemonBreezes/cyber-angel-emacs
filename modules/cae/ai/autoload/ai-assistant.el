;;; cae/ai/autoload/ai-assistant.el -*- lexical-binding: t; -*-

(defcustom cae-ai-assistant-terminal-backend (if (and (modulep! :cae exwm)
                                                      cae-exwm-enabled-p)
                                                 'exwm
                                               'eat)
  "Backend to use for terminal operations.
Can be 'vterm, 'eat, or 'exwm."
  :type '(choice (const :tag "VTerm" vterm)
          (const :tag "Eat" eat)
          (const :tag "EXWM Terminal" exwm))
  :group 'cae-ai-assistant)

(defcustom cae-ai-assistant-app "opencode"
  "AI coding assistant application to use.
Supported values: \"claude\", \"opencode\", \"codex\", \"gemini\", \"aider\",
\"chatgpt\", \"copilot\", \"tabnine\", \"sourcegraph-cody\", \"continue\"."
  :type '(choice (const :tag "Claude" "claude")
          (const :tag "OpenCode" "opencode")
          (const :tag "Codex" "codex")
          (const :tag "Gemini" "gemini")
          (const :tag "Aider" "aider")
          (string :tag "Custom command"))
  :group 'cae-ai-assistant)

(defcustom cae-ai-assistant-sandbox-root "~/src/ai-sandbox"
  "Root directory for AI assistant sandbox environments."
  :type 'directory
  :group 'cae-ai-assistant)

(defun cae-ai-assistant--generate-folder-name (task-description)
  "Generate a folder name from TASK-DESCRIPTION using AI.
 Uses the llm package to get a concise folder name."
  (let* ((prompt (format "Summarize this task in 3-5 words, using only alphanumeric characters and hyphens. Make it suitable for a folder name. Don't use any special characters. Task: %s" task-description))
         (response (llm-chat llm-refactoring-provider
                             (llm-make-chat-prompt prompt
                                                   :context "You are a helpful assistant that generates concise folder names."
                                                   :max-tokens 50
                                                   :temperature 0.7))))
    (replace-regexp-in-string " " "-" (string-trim response))))

(defun cae-ai-assistant--create-sandbox (sandbox-root task-description folder-name &optional app-name)
  "Create an AI assistant sandbox with FOLDER-NAME in SANDBOX-ROOT for TASK-DESCRIPTION.
Optional APP-NAME specifies which AI assistant to use (defaults to `cae-ai-assistant-app')."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (app-name (or app-name cae-ai-assistant-app))
         (sandbox-dir (expand-file-name (format "%s/%s--%s" app-name timestamp folder-name)
                                        (expand-file-name sandbox-root)))
         (buffer-name (format "*%s:sandbox:%s*" app-name folder-name)))

    (make-directory sandbox-dir t)
    ;; Create an empty .projectile file to mark it as a project root
    (with-temp-buffer
      (write-file (expand-file-name ".projectile" sandbox-dir)))
    (let ((default-directory sandbox-dir))
      (cond
       ((eq cae-ai-assistant-terminal-backend 'vterm)
        (require 'vterm)
        (vterm-other-window buffer-name)
        ;; Send the task description directly as a quoted argument to the AI assistant
        (vterm-send-string (format "%s \"%s\"" app-name task-description))
        (vterm-send-return))
       ((eq cae-ai-assistant-terminal-backend 'eat)
        (require 'eat)
        (eat-other-window buffer-name)
        ;; Send the task description directly as a quoted argument to the AI assistant
        (eat--send-string (format "%s \"%s\"" app-name task-description))
        (eat--send-string "\C-m"))
       ((eq cae-ai-assistant-terminal-backend 'exwm)
        (when (modulep! :cae exwm)
          ;; Use the terminal function from exwm module
          (cae-exwm-run-terminal-in-current-workspace
           (format "%s \"%s\"" app-name task-description)))
        (unless (modulep! :cae exwm)
          (error "EXWM module is not enabled. Please enable :cae exwm in your config.")))))))

;;;###autoload
(defun cae-ai-assistant-code (&optional create-sandbox)
  "Open an AI coding assistant terminal in a vterm buffer.
If CREATE-SANDBOX is non-nil (with prefix argument), create a sandbox
environment in `cae-ai-assistant-sandbox-root' with a timestamped folder based on
a task description, and start the AI assistant there.
Otherwise, open the AI assistant for the current project."
  (interactive "P")
  (require 'llm)
  (cond
   ((eq cae-ai-assistant-terminal-backend 'vterm)
    (require 'vterm))
   ((eq cae-ai-assistant-terminal-backend 'eat)
    (require 'eat))
   ((eq cae-ai-assistant-terminal-backend 'exwm)
    (unless (and (modulep! :cae exwm)
                 cae-exwm-enabled-p)
      (error "EXWM module is not enabled. Please enable :cae exwm in your config."))))
  
  ;; First check if there's already an AI assistant buffer for this project
  (let* ((project-root (doom-project-root))
         (app-choices '("claude" "opencode" "codex" "gemini" "aider"))
         (existing-buffer nil)
         (selected-app nil)
         (terminal-class (when (eq cae-ai-assistant-terminal-backend 'exwm)
                           (file-name-nondirectory cae-exwm-terminal-command))))

    ;; Check for existing AI assistant buffers for this project
    ;; For EXWM, check for terminal buffer; for others, check app-specific buffers
    (cond
     ((eq cae-ai-assistant-terminal-backend 'exwm)
      ;; For EXWM, check for terminal buffer (e.g., *kitty:project*)
      (let ((buffer-name (format "*%s:%s*" terminal-class (or (projectile-project-name) "default"))))
        (when (get-buffer buffer-name)
          (setq existing-buffer (get-buffer buffer-name)))))
     (t
      ;; For vterm/eat, check app-specific buffers
      (dolist (app app-choices)
        (let ((buffer-name (format "*%s:%s*" app project-root)))
          (when (get-buffer buffer-name)
            (setq existing-buffer (get-buffer buffer-name))
            (setq selected-app app)
            (return))))))

    (if existing-buffer
        ;; Found existing buffer, focus it
        (if-let ((window (get-buffer-window existing-buffer)))
            ;; Buffer is visible, focus its window
            (select-window window)
          ;; Buffer exists but not visible, display it
          (switch-to-buffer-other-window existing-buffer))

      ;; No existing buffer, proceed with selection
      (setq selected-app (completing-read (format "Select AI assistant (default: %s): " cae-ai-assistant-app)
                                          app-choices
                                          nil t nil nil cae-ai-assistant-app))

      (if create-sandbox
          (let* ((sandbox-root cae-ai-assistant-sandbox-root)
                 (task-description (read-string "Enter task description: "
                                                nil 'cae-ai-assistant--task-history)))

            ;; Ensure sandbox root directory exists
            (unless (file-exists-p (expand-file-name sandbox-root))
              (make-directory (expand-file-name sandbox-root) t))

            ;; Generate folder name and create sandbox
            (let ((folder-name (cae-ai-assistant--generate-folder-name task-description)))
              (cae-ai-assistant--create-sandbox sandbox-root task-description folder-name selected-app)))))

    ;; Original project-based behavior (no existing buffer found)
    (let* ((app-name selected-app)
           (buffer-name (cond
                         ((eq cae-ai-assistant-terminal-backend 'exwm)
                          (format "*%s:%s*" terminal-class (or (projectile-project-name) "default")))
                         (t
                          (format "*%s:%s*" app-name project-root))))
           (default-directory (or project-root default-directory)))
      (cond
       ((eq cae-ai-assistant-terminal-backend 'vterm)
        (vterm-other-window buffer-name)
        (vterm-send-string app-name)
        (vterm-send-return))
       ((eq cae-ai-assistant-terminal-backend 'eat)
        (let ((eat-buffer-name buffer-name))
          (eat-other-window app-name)))
       ((eq cae-ai-assistant-terminal-backend 'exwm)
        (when (modulep! :cae exwm)
          ;; Use the terminal function from exwm module
          (cae-exwm-run-terminal-in-current-workspace app-name)))))))
