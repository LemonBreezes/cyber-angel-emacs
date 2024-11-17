;;; cae/ai/+ollama.el -*- lexical-binding: t; -*-

;;(after! llm
;;  (cl-defmethod llm-provider-chat-url ((provider llm-openai))
;;    (llm-openai--url provider "chat")) ; chat/completions
;;  (setq llm-refactoring-provider
;;        (make-llm-openai-compatible :key (cae-secrets-get-ollama-api-key)
;;                                    :url (format "http://%s:3000/ollama/api/"
;;                                                 (or (bound-and-true-p cae-ip-address)
;;                                                     "127.0.0.1"))
;;                                    :chat-model "zephyr:latest")
;;        magit-gptcommit-llm-provider llm-refactoring-provider))

(after! chatgpt-shell
  ;; your ollama endpoint
  (setq chatgpt-shell-api-url-base (format "http://%s:3000"
                                           (or (bound-and-true-p cae-ip-address)
                                               "127.0.0.1"))
        chatgpt-shell-api-url-path "/ollama/api/chat")

  ;; models you have pulled for use with ollama
  (setq chatgpt-shell-model-versions
        '("zephyr:latest"
          "dolphin-mixtral:latest"
          "llava:latest"
          "llama3:latest"
          "gemma2:27b"
          "deepseek-coder-v2:latest"))
  (setq chatgpt-shell-model-version 0)

  (defvar chatgpt-shell-model-settings
    (list (cons "llama3:latest" '((max-tokens . 8192)))
          (cons "llava:13b" '((max-tokens . 8192)))
          (cons "gemma2:27b" '((max-tokens . 8192)))
          (cons "dolphin-mixtral:latest" '((max-tokens . 8192)))
          (cons "deepseek-coder-v2:latest" '((max-tokens . 8192)))
          (cons "zephyr:latest" '((max-tokens . 8192)))))

  ;; Adapt the above function to our `chatgpt-shell-model-settings'
  (defun chatgpt-shell--approximate-context-length (model messages)
    "Approximate the context length using MODEL and MESSAGES."
    (let* ((tokens-per-message 4)
           (max-tokens)
           (original-length (floor (/ (length messages) 2)))
           (context-length original-length))
      (let ((settings (alist-get model chatgpt-shell-model-settings)))
        (setq max-tokens (alist-get 'max-tokens settings 4096)))
      (while (> (chatgpt-shell--num-tokens-from-messages
                 tokens-per-message messages)
                max-tokens)
        (setq messages (cdr messages)))
      (setq context-length (floor (/ (length messages) 2)))
      (unless (eq original-length context-length)
        (message "Warning: chatgpt-shell context clipped"))
      context-length))

  (defun chatgpt-shell--extract-chatgpt-response (json)
    "Extract ChatGPT response from JSON."
    (if (eq (type-of json) 'cons)
        (let-alist json ;; already parsed
          (or (or .delta.content
                  .message.content)
              .error.message
              ""))
      (if-let* (parsed (shell-maker--json-parse-string json))
          (string-trim
           (let-alist parsed
             .message.content))
        (if-let* (parsed-error (shell-maker--json-parse-string-filtering
                               json "^curl:.*\n?"))
            (let-alist parsed-error
              .error.message)))))

  (defun chatgpt-shell--prompt-pair ()
    "Return a pair with prompt and prompt-regexp."
    (cons
     (format "Ollama(%s)> " (chatgpt-shell--shell-info))
     (rx (seq bol "Ollama" (one-or-more (not (any "\n"))) ">" (or space "\n")))))

  (eval '(setf (shell-maker-config-prompt chatgpt-shell--config)
               (car (chatgpt-shell--prompt-pair))))
  (eval '(setf (shell-maker-config-prompt-regexp chatgpt-shell--config)
               (cdr (chatgpt-shell--prompt-pair)))))
