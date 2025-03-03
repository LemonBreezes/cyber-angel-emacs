;;; cae/ai/autoload/chatgpt-shell.el -*- lexical-binding: t; -*-

(defvar cae-ai-chatgpt-popup-display-config
  '(("^\\*chatgpt\\* "
     (+popup-buffer)
     (actions)
     (side . top)
     (size)
     (window-width . 40)
     (window-height . 0.16)
     (slot) (vslot)
     (window-parameters (ttl . 0)
                        (quit . t)
                        (select . t)
                        (modeline)
                        (autosave)
                        (transient . t)
                        (no-other-window . t))))
  "Display configuration for ChatGPT popup buffers.")

;;;###autoload
(defun cae-ai-get-terminal-buffer-content (&optional max-chars)
  "Get the content of the current terminal buffer with reasonable truncation.
Optional argument MAX-CHARS limits the number of characters to extract (default 4000)."
  (let* ((max-chars (or max-chars 4000))
         (buffer-text (buffer-substring-no-properties
                       (max (- (point-max) max-chars) (point-min))
                       (point-max))))
    ;; If we truncated from the beginning, add an indicator
    (if (> (point-max) (+ (point-min) max-chars))
        (concat "[...TRUNCATED OUTPUT...]\n" buffer-text)
      buffer-text)))

(defun cae-ai-send-to-chatgpt (query &optional context)
  "Send QUERY to ChatGPT with optional CONTEXT.
If CONTEXT is provided, it will be formatted as a code block before the query."
  (require 'chatgpt-shell)
  (let* ((chatgpt-shell-prompt-query-response-style 'other-buffer)
         (display-buffer-alist cae-ai-chatgpt-popup-display-config)
         (formatted-prompt (if context
                               (format "Here is the terminal output:\n```\n%s\n```\n\nMy question about this output: %s"
                                       context query)
                             query)))
    (chatgpt-shell-send-to-buffer formatted-prompt)))

;;;###autoload
(defun cae-send-to-chatgpt-if-comma-a (f &rest args)
  "Advice function to intercept comma-prefixed input in comint buffers.
If input starts with a comma, send it to ChatGPT, otherwise call F with ARGS."
  (require 'chatgpt-shell)
  (let ((input (comint-get-old-input-default)))
    (if (string-prefix-p "," input)
        (let ((query (substring input 1))
              (buffer-content (cae-ai-get-terminal-buffer-content)))
          (cae-ai-send-to-chatgpt query buffer-content))
      (apply f args))))

;;;###autoload
(defun cae-eshell-send-to-chatgpt-if-comma ()
  "Send eshell input to ChatGPT if it starts with a comma.
This function is meant to be used in `eshell-input-filter-functions'."
  (let ((input (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)))
    (when (string-prefix-p "," input)
      (let ((query (substring input 1))
            (buffer-content (cae-ai-get-terminal-buffer-content)))
        (cae-ai-send-to-chatgpt query buffer-content)
        ;; Delete the command from the input to prevent it from being executed
        (delete-region eshell-last-input-start eshell-last-input-end)
        ;; Insert a blank line to maintain the prompt appearance
        (goto-char eshell-last-input-start)
        (insert "\n")
        ;; Return nil to indicate we don't want to continue processing
        nil))))

;;;###autoload
(defun cae-ai-toggle-shell-buffer (get-buffer-fn &optional create-fn)
  "Toggle visibility of a shell buffer.
GET-BUFFER-FN should return the buffer to toggle.
CREATE-FN is called when the buffer doesn't exist or isn't visible."
  (if-let* ((buf (funcall get-buffer-fn))
            (win (get-buffer-window buf)))
      (delete-window win)
    (when create-fn (funcall create-fn))))

;;;###autoload
(defun cae-ai-toggle-chatgpt-shell ()
  "Toggle the visibility of the ChatGPT shell buffer."
  (interactive)
  (require 'chatgpt-shell)
  (cae-ai-toggle-shell-buffer 
   #'chatgpt-shell--primary-buffer
   (lambda () 
     (let ((chatgpt-shell-display-function #'pop-to-buffer))
       (chatgpt-shell)))))

;;;###autoload
(defun cae-ai-toggle-dall-e-shell ()
  "Toggle the visibility of the DALL-E shell buffer."
  (interactive)
  (require 'dall-e-shell)
  (cae-ai-toggle-shell-buffer 
   (lambda () 
     (when (dall-e-shell--shell-buffers)
       (car (dall-e-shell--shell-buffers))))
   (lambda () 
     (let ((dall-e-shell-display-function #'pop-to-buffer))
       (dall-e-shell)))))

;;;###autoload
(defun cae-ai-chatgpt-quit-or-delete-char (arg)
  "Delete ARG characters or quit if at process mark.
If at the end of buffer and at process mark, kill the buffer.
Otherwise delete ARG characters forward."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
        (kill-buffer (current-buffer))
      (delete-char arg))))

;;;###autoload
(defun cae-ai-open-chatgpt-workspace ()
  "Open ChatGPT in a dedicated workspace."
  (interactive)
  (+workspace-switch cae-ai-chatgpt-shell-workspace-name t)
  (chatgpt-shell)
  (persp-add-buffer (current-buffer)))

;;;###autoload
(defun cae-ai-open-dall-e-workspace ()
  "Open DALL-E in a dedicated workspace."
  (interactive)
  (+workspace-switch cae-ai-dall-e-shell-workspace-name t)
  (dall-e-shell)
  (persp-add-buffer (current-buffer)))
