;;; cae/ai/autoload/minuet.el -*- lexical-binding: t; -*-

(defun cae-ai-minuet-toggle-quality ()
  "Toggle the minuet FIM model between the small (fit-alongside-chat)
and large (best-quality) qwen2.5-coder variants."
  (interactive)
  (require 'minuet)
  (let* ((current (plist-get minuet-openai-fim-compatible-options :model))
         (next (if (equal current cae-minuet-fim-model-small)
                   cae-minuet-fim-model-large
                 cae-minuet-fim-model-small)))
    (plist-put minuet-openai-fim-compatible-options :model next)
    (message "Minuet FIM model -> %s" next)))
