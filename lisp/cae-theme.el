;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(setq doom-theme 'ef-trio-light)

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

(defun cae-theme-customize-faces-h (theme)
  (when (modulep! :lang org)
    (after! org
      (set-face-attribute 'org-ellipsis nil
                          :inherit '(shadow default)
                          :weight 'normal)
      (set-face-attribute 'org-document-title nil
                          :height 1.2)))
  ;; For `esh-autosuggest'.
  (after! company
    (set-face-attribute 'company-preview-common nil
                        :inherit 'shadow
                        :background 'unspecified)
    (set-face-attribute 'company-preview nil
                        :inherit 'shadow
                        :background 'unspecified))
  (when (and (modulep! :tools lsp)
             (not (modulep! :tools lsp +eglot)))
    (after! lsp-mode
      (set-face-attribute 'markdown-code-face nil
                          :background 'unspecified)))
  (after! highlight-context-line
    (set-face-attribute 'highlight-context-line-face nil
                        :extend t))
  ;; Remove bold constructs.
  (dolist (face '(font-lock-keyword-face
                  font-lock-type-face
                  font-lock-builtin-face
                  font-lock-constant-face
                  font-lock-variable-name-face
                  font-lock-function-name-face
                  font-lock-string-face
                  font-lock-comment-face
                  font-lock-doc-face))
    (set-face-attribute face nil :weight 'normal)))

(after! modus-themes
  (let ((modus-themes-custom-auto-reload nil))
    (setopt modus-themes-org-blocks 'gray-background
           modus-themes-slanted-constructs t
           modus-themes-bold-constructs nil
           modus-themes-variable-pitch-ui t
           modus-themes-mixed-fonts t
           modus-themes-prompts '(italic semibold))))
(after! ef-themes
  (setopt ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t
          ef-themes-to-toggle '(ef-trio-light ef-trio-dark)))

(map! :leader
      :desc "Toggle theme" "t T" #'modus-themes-toggle)

;; Set theme based on time
(when (display-graphic-p)
  (advice-add #'doom-init-theme-h :override #'ignore)
  (use-package! circadian
    :config
    (setq circadian-themes '((:sunrise . modus-operandi)
                             (:sunset  . modus-vivendi)))
    (circadian-setup)))
