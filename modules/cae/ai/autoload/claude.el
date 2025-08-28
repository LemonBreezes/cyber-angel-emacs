;;; cae/ai/autoload/claude.el -*- lexical-binding: t; -*-

(defcustom cae-claude-terminal-backend 'comint
  "Backend to use for terminal operations.
Can be `vterm', `eat', `comint'."
  :type '(choice (const :tag "VTerm" vterm)
                 (const :tag "Eat" eat)
                 (const :tag "Comint" comint))
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
         (eat--send-string "\C-m"))
        ((eq cae-claude-terminal-backend 'comint)
         (cae-claude-comint buffer-name task-description))))))

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
            (eat-other-window (if cae-claude-use-opencode "opencode" "claude"))))
         ((eq cae-claude-terminal-backend 'comint)
          (cae-claude-comint buffer-name)))))))

(defun cae-claude-comint (&optional buffer-name initial-task)
  "Create a comint buffer for Claude interaction.
BUFFER-NAME is the name of the buffer to create.
INITIAL-TASK is an optional task description to send immediately."
  (require 'comint)
  (let* ((buffer-name (or buffer-name (format "*%s:comint*" 
                                               (if cae-claude-use-opencode "opencode" "claude"))))
         (buffer (get-buffer-create buffer-name))
         (program (if cae-claude-use-opencode "opencode" "claude"))
         (args (when initial-task (list (shell-quote-argument initial-task)))))
    
    (with-current-buffer buffer
      (unless (comint-check-proc buffer)
        (comint-mode)
        (comint-exec buffer buffer-name program nil args)
        (setq-local comint-process-echoes t)
        (setq-local comint-scroll-to-bottom-on-output t)
        (setq-local comint-scroll-show-maximum-output t)
        
        ;; Add keybindings for better interaction
        (local-set-key (kbd "C-c C-c") 'comint-interrupt-subjob)
        (local-set-key (kbd "C-c C-z") 'comint-stop-subjob)
        (local-set-key (kbd "C-c C-d") 'comint-send-eof)
        (local-set-key (kbd "C-c C-l") 'comint-clear-buffer)
        
        ;; Add a quit function similar to other shells
        (local-set-key (kbd "C-c C-q") 'cae-claude-comint-quit)))
    
    (pop-to-buffer buffer)))

(defun cae-claude-comint-quit ()
  "Quit the Claude comint buffer or delete character.
If at the end of buffer and at process mark, kill the buffer.
Otherwise delete character forward."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
        (kill-buffer (current-buffer))
      (delete-char 1))))

(defun cae-claude-comint-send-region (start end)
  "Send the region from START to END to Claude comint process."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (comint-send-string (get-buffer-process (current-buffer)) text)
    (comint-send-string (get-buffer-process (current-buffer)) "\n")))

(defun cae-claude-comint-send-buffer ()
  "Send the entire buffer content to Claude comint process."
  (interactive)
  (cae-claude-comint-send-region (point-min) (point-max)))

(defun cae-claude-comint-send-file (filename)
  "Send the content of FILENAME to Claude comint process."
  (interactive "fSend file: ")
  (let ((content (with-temp-buffer
                   (insert-file-contents filename)
                   (buffer-string))))
    (comint-send-string (get-buffer-process (current-buffer)) content)
    (comint-send-string (get-buffer-process (current-buffer)) "\n")))

(defun cae-claude-comint-toggle ()
  "Toggle the visibility of the Claude comint buffer."
  (interactive)
  (let* ((buffer-name (format "*%s:comint*" 
                               (if cae-claude-use-opencode "opencode" "claude")))
         (buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (cae-claude-comint buffer-name))))

(provide 'cae-claude)
