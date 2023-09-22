;;; private/ai/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-evil-org-ai-on-region "private/ai/autoload/evil" nil t)
(evil-define-operator cae-evil-org-ai-on-region (beg end)
  "Evaluate selection or sends it to the open REPL, if available."
  :move-point nil
  (interactive "<r>")
  ;; use beg and end as the interactively selected region then call org-ai-on-region interactively
  (evil-with-active-region beg end
    (call-interactively #'org-ai-on-region)))
