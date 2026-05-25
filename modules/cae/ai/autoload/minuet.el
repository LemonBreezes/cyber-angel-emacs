;;; cae/ai/autoload/minuet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ai-minuet-toggle-model ()
  "Toggle the minuet FIM model between the general coding model
\(`cae-coding-fim-model', Codestral) and the Lean-native one
\(`cae-lean-fim-model', Leanstral)."
  (interactive)
  (require 'minuet)
  (let* ((current (plist-get minuet-openai-fim-compatible-options :model))
         (next (if (equal current cae-lean-fim-model)
                   cae-coding-fim-model
                 cae-lean-fim-model)))
    (plist-put minuet-openai-fim-compatible-options :model next)
    (message "Minuet FIM model -> %s" next)))
