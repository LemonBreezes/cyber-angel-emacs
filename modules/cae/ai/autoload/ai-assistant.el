;;; cae/ai/autoload/ai-assistant.el -*- lexical-binding: t; -*-

(defcustom cae-ai-assistant-tmux-integration 'non-sandbox
  "Whether to wrap the AI assistant command in a persistent tmux session.

When enabled, the assistant runs inside a tmux session keyed on the
target directory's basename, so the process survives the Emacs terminal
buffer and can be reattached later (e.g. with `tmux a -t <name>').  If
two projects share the same basename, they share the same tmux session.

Values:
- `disabled': Never use tmux.
- `non-sandbox': Use tmux for real projects only, not the sandbox
  invoked via the universal argument.
- `all': Use tmux for everything, including sandboxes."
  :type '(choice (const :tag "Disabled" disabled)
          (const :tag "Non-sandbox only" non-sandbox)
          (const :tag "All (including sandboxes)" all))
  :group 'cae-ai-assistant)

(defcustom cae-ai-assistant-terminal-backend
  (cond ((and (modulep! :cae exwm) cae-exwm-enabled-p)
         'exwm)
        ;; When EXWM isn't running, prefer ghostel over vterm as long as it's
        ;; usable (i.e. not in a Linux TTY, where its text is invisible) and the
        ;; module is enabled.
        ((modulep! :cae ghostel)
         'ghostel)
        (t 'vterm))
  "Backend to use for terminal operations.
Can be 'vterm, 'eat, 'exwm, or 'ghostel."
  :type '(choice (const :tag "VTerm" vterm)
          (const :tag "Eat" eat)
          (const :tag "EXWM Terminal" exwm)
          (const :tag "Ghostel" ghostel))
  :group 'cae-ai-assistant)

(defcustom cae-ai-assistant-app "claude"
  "AI coding assistant application to use.
Supported values: \"claude\", \"opencode\", \"codex\", \"gemini\", \"aider\",
\"chatgpt\", \"copilot\", \"tabnine\", \"sourcegraph-cody\", \"continue\"."
  :type '(choice (const :tag "Claude" "claude")
          (const :tag "Mistral Vibe" "vibe")
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

(defun cae-ai-assistant--tmux-enabled-p (sandbox-p)
  "Return non-nil if tmux integration should wrap the assistant command.
SANDBOX-P indicates whether the call targets a sandbox directory."
  (pcase cae-ai-assistant-tmux-integration
    ('disabled nil)
    ('non-sandbox (not sandbox-p))
    ('all t)
    (_ nil)))

(defun cae-ai-assistant--tmux-session-name (directory)
  "Compute a tmux session name from DIRECTORY's basename.
Tmux disallows `.', `:', and whitespace in session names, so any
character outside [A-Za-z0-9_-] is replaced with `-'."
  (let ((name (file-name-nondirectory
               (directory-file-name (expand-file-name directory)))))
    (replace-regexp-in-string "[^A-Za-z0-9_-]" "-"
                              (if (string-empty-p name) "ai" name))))

(defun cae-ai-assistant--env-unset-prefix (env-unset)
  "Return a shell `env -u' prefix that removes ENV-UNSET variables.
ENV-UNSET is a list of variable names.  Returns the empty string when nil."
  (if env-unset
      (concat "env "
              (mapconcat (lambda (var) (concat "-u " (shell-quote-argument var)))
                         env-unset " ")
              " ")
    ""))

(defun cae-ai-assistant--maybe-tmux-wrap (command directory sandbox-p &optional env-unset)
  "Optionally wrap COMMAND so it runs inside a persistent tmux session.
The session is keyed on DIRECTORY's basename.  SANDBOX-P controls
whether wrapping applies via `cae-ai-assistant-tmux-integration'.
The inner COMMAND is passed as a single shell-quoted argument so that
embedded quotes (e.g. a task description) survive intact.

ENV-UNSET is a list of environment variable names to strip from the
command's environment via `env -u'.  This is necessary because the tmux
pane inherits the tmux server's environment (and re-sources `.bashrc'),
so unsetting a variable in `process-environment' on the Emacs side does
not reach the process that actually runs inside the session.

Note that `tmux new-session -A' ignores the shell-command whenever it
attaches to a session that already exists, so the `env -u' prefix only
takes effect the first time the session is created.  To keep reattaches
clean we also remove ENV-UNSET from the session environment with
`set-environment -r', so every pane spawned in the session (now or
later) has them stripped.  A process that is *already* running in a
stale session keeps its original environment until it is restarted."
  (if (cae-ai-assistant--tmux-enabled-p sandbox-p)
      (let* ((session (cae-ai-assistant--tmux-session-name directory))
             (remote (file-remote-p directory))
             ;; For a TRAMP directory (e.g. a `sudo'/`su' connection on the
             ;; local host) the start directory must be a real local path, not
             ;; the TRAMP file name; otherwise tmux can't `chdir' into `-c' and
             ;; silently launches the assistant in tmux's own cwd.  We also
             ;; `cd' into it as part of the command chain, since `-c' is honoured
             ;; only when the session is first created.
             (local-dir (if remote
                            (file-local-name directory)
                          (expand-file-name directory)))
             (inner (concat (cae-ai-assistant--env-unset-prefix env-unset) command))
             (inner (if remote
                        (format "cd %s && %s" (shell-quote-argument local-dir) inner)
                      inner)))
        (concat
         (format "tmux new-session -A -s %s -c %s %s"
                 (shell-quote-argument session)
                 (shell-quote-argument local-dir)
                 (shell-quote-argument inner))
         ;; `\\;' reaches tmux as a literal `;', chaining a server-side
         ;; `set-environment -r' for each variable.  This runs on both
         ;; create and reattach, unlike the new-session shell-command.
         (mapconcat (lambda (var)
                      (format " \\; set-environment -t %s -r %s"
                              (shell-quote-argument session)
                              (shell-quote-argument var)))
                    env-unset "")))
    command))

(defun cae-ai-assistant--generate-folder-name (task-description)
  "Generate a folder name from TASK-DESCRIPTION using AI.
 Uses the llm package to get a concise folder name."
  (let* ((prompt (format "Summarize this task in 3-5 words, using only alphanumeric characters and hyphens. Make it suitable for a folder name. Don't use any special characters. Task: %s" task-description))
         (response (llm-chat llm-refactoring-provider
                             (llm-make-chat-prompt prompt
                                                   :context "You are a helpful assistant that generates concise folder names."
                                                   :max-tokens 50))))
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
    ;; Unset ANTHROPIC_API_KEY for claude code to use its own token
    (let* ((claude-p (string= app-name "claude"))
           (unset-anthropic-key (when claude-p '("ANTHROPIC_API_KEY=")))
           (process-environment (if claude-p
                                    (append process-environment unset-anthropic-key)
                                  process-environment))
           (default-directory sandbox-dir)
           (inner-command (format "%s \"%s\"" app-name task-description))
           (command (cae-ai-assistant--maybe-tmux-wrap
                     inner-command sandbox-dir t
                     (when claude-p '("ANTHROPIC_API_KEY")))))
      (cond
       ((eq cae-ai-assistant-terminal-backend 'vterm)
        (require 'vterm)
        (let ((vterm-environment (append vterm-environment unset-anthropic-key)))
          (vterm-other-window buffer-name))
        ;; Send the task description directly as a quoted argument to the AI assistant
        (vterm-send-string command)
        (vterm-send-return))
       ((eq cae-ai-assistant-terminal-backend 'eat)
        (require 'eat)
        (eat-other-window buffer-name)
        ;; Send the task description directly as a quoted argument to the AI assistant
        (eat--send-string command)
        (eat--send-string "\C-m"))
       ((eq cae-ai-assistant-terminal-backend 'ghostel)
        (require 'ghostel)
        (let ((ghostel-buffer-name buffer-name)
              (ghostel-environment (append (bound-and-true-p ghostel-environment)
                                           unset-anthropic-key))
              (display-buffer-overriding-action
               '((display-buffer-pop-up-window display-buffer-use-some-window)
                 (inhibit-same-window . t))))
          (with-current-buffer (ghostel)
            ;; Send the task description directly as a quoted argument to the AI assistant
            (ghostel-send-string command)
            (ghostel-send-string "\C-m"))))
       ((eq cae-ai-assistant-terminal-backend 'exwm)
        (when (modulep! :cae exwm)
          ;; Use the terminal function from exwm module
          (cae-exwm-run-terminal-in-current-workspace command))
        (unless (modulep! :cae exwm)
          (error "EXWM module is not enabled. Please enable :cae exwm in your config.")))))))

;;;###autoload
(cl-defun cae-ai-assistant-code (&optional create-sandbox)
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
   ((eq cae-ai-assistant-terminal-backend 'ghostel)
    (require 'ghostel))
   ((eq cae-ai-assistant-terminal-backend 'exwm)
    (unless (and (modulep! :cae exwm)
                 cae-exwm-enabled-p)
      (error "EXWM module is not enabled. Please enable :cae exwm in your config."))))

  ;; First check if there's already an AI assistant buffer for this project
  (let* ((project-root (doom-project-root))
         (app-choices '("claude" "opencode" "codex" "gemini" "aider" "vibe"))
         (existing-buffer nil)
         (selected-app nil)
         (terminal-class (when (eq cae-ai-assistant-terminal-backend 'exwm)
                           (file-name-nondirectory cae-exwm-terminal-command))))

    ;; Check for existing AI assistant buffers for this project
    ;; For EXWM, check for terminal buffer; for others, check app-specific buffers
    (cond
     ((eq cae-ai-assistant-terminal-backend 'exwm)
      ;; For EXWM, check for terminal buffer (e.g., *kitty:project*)
      (let ((buffer-name (format "*%s:%s*" terminal-class
                                 (or (and (modulep! :ui workspaces)
                                          (+workspace-current-name))
                                     (projectile-project-name) "default"))))
        (when (get-buffer buffer-name)
          (setq existing-buffer (get-buffer buffer-name)))))
     (t
      ;; For vterm/eat, check app-specific buffers
      (dolist (app app-choices)
        (let ((buffer-name (format "*%s:%s*" app project-root)))
          (when (get-buffer buffer-name)
            (setq existing-buffer (get-buffer buffer-name))
            (setq selected-app app))))))
    
    (if existing-buffer
        ;; Found existing buffer, focus it
        (progn (if-let ((window (get-buffer-window existing-buffer)))
                   ;; Buffer is visible, focus its window
                   (select-window window)
                 ;; Buffer exists but not visible, display it
                 (switch-to-buffer-other-window existing-buffer))
               (cl-return-from cae-ai-assistant-code))
      
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
           (default-directory (or project-root default-directory))
           ;; Unset ANTHROPIC_API_KEY only for claude code
           (claude-p (string= app-name "claude"))
           (unset-anthropic-key (when claude-p '("ANTHROPIC_API_KEY=")))
           (process-environment (if claude-p
                                    (append process-environment unset-anthropic-key)
                                  process-environment))
           (command (cae-ai-assistant--maybe-tmux-wrap
                     app-name default-directory nil
                     (when claude-p '("ANTHROPIC_API_KEY")))))
      (cond
       ((eq cae-ai-assistant-terminal-backend 'vterm)
        (let ((vterm-environment (append vterm-environment unset-anthropic-key)))
          (vterm-other-window buffer-name))
        (vterm-send-string command)
        (vterm-send-return))
       ((eq cae-ai-assistant-terminal-backend 'eat)
        (let ((eat-buffer-name buffer-name))
          (ignore eat-buffer-name)      ; Silence byte-compiler.
          (eat-other-window command)))
       ((eq cae-ai-assistant-terminal-backend 'ghostel)
        (let ((ghostel-buffer-name buffer-name)
              (ghostel-environment (append (bound-and-true-p ghostel-environment)
                                           unset-anthropic-key))
              (display-buffer-overriding-action
               '((display-buffer-pop-up-window display-buffer-use-some-window)
                 (inhibit-same-window . t))))
          (with-current-buffer (ghostel)
            (ghostel-send-string command)
            (ghostel-send-string "\C-m"))))
       ((eq cae-ai-assistant-terminal-backend 'exwm)
        (when (modulep! :cae exwm)
          ;; Use the terminal function from exwm module
          (cae-exwm-run-terminal-in-current-workspace command)))))))
