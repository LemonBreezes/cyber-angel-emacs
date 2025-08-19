;;; cae/ai/autoload/claude.el -*- lexical-binding: t; -*-

(defcustom cae-claude-terminal-backend 'eat
  "Backend to use for terminal operations.
Can be 'vterm or 'eat."
  :type '(choice (const :tag "VTerm" vterm)
                 (const :tag "Eat" eat))
  :group 'cae-claude)

(defcustom cae-claude-use-opencode t
  "Whether to use opencode instead of claude-code."
  :type 'boolean
  :group 'cae-claude)

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
         (buffer-name (format "*%s:sandbox:%s*" 
                              (if cae-claude-use-opencode "opencode" "claude")
                              folder-name)))

    (make-directory sandbox-dir t)
    ;; Create an empty .projectile file to mark it as a project root
    (with-temp-buffer
      (write-file (expand-file-name ".projectile" sandbox-dir)))
    (let ((default-directory sandbox-dir))
      (cond
       ((eq cae-claude-terminal-backend 'vterm)
        (require 'vterm)
        (vterm-other-window buffer-name)
        ;; Send the task description directly as a quoted argument to claude
        (vterm-send-string (format "%s \"%s\"" 
                                   (if cae-claude-use-opencode "opencode" "claude")
                                   task-description))
        (vterm-send-return))
       ((eq cae-claude-terminal-backend 'eat)
        (require 'eat)
        (eat-other-window buffer-name)
        ;; Send the task description directly as a quoted argument to claude
        (eat--send-string (format "%s \"%s\"" 
                                  (if cae-claude-use-opencode "opencode" "claude")
                                  task-description))
        (eat--send-string "\C-m"))))))

;;;###autoload
(defun cae-claude-code (&optional create-sandbox)
  "Open a Claude AI terminal in a vterm buffer.
If CREATE-SANDBOX is non-nil (with prefix argument), create a sandbox
environment in ~/src/claude-sandbox with a timestamped folder based on
a task description, and start Claude there.
Otherwise, open Claude for the current project."
  (interactive "P")
  (require 'llm)
  (cond
   ((eq cae-claude-terminal-backend 'vterm)
    (require 'vterm))
   ((eq cae-claude-terminal-backend 'eat)
    (require 'eat)))
  
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
           (buffer-name (format "*%s:%s*" 
                                (if cae-claude-use-opencode "opencode" "claude")
                                project-root))
           (default-directory (or project-root default-directory))
           (existing-buffer (get-buffer buffer-name)))
      (if existing-buffer
          (pop-to-buffer existing-buffer)
        (cond
         ((eq cae-claude-terminal-backend 'vterm)
          (vterm-other-window buffer-name)
          (vterm-send-string (if cae-claude-use-opencode "opencode" "claude"))
          (vterm-send-return))
         ((eq cae-claude-terminal-backend 'eat)
          (let ((eat-buffer-name buffer-name))
            (eat-other-window (if cae-claude-use-opencode "opencode" "claude")))
          ))))))
