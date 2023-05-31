;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(setq doom-theme 'wheatgrass)

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

(defun cae-theme-customize-faces-h (_)
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
  (after! goggles
    (set-face-attribute 'goggles-added nil
                        :background (face-attribute 'lazy-highlight :background)))
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

(if (display-graphic-p)
    (progn (advice-add #'doom-init-theme-h :override #'ignore)
           (use-package! circadian
             :config
             (setq circadian-themes '((:sunrise . modus-operandi)
                                      (:sunset  . modus-vivendi)))
             (if (and calendar-latitude calendar-longitude)
                 (circadian-setup)
               (setq calendar-latitude 0
                     calendar-longitude 0)
               (message "ERROR: Calendar latitude and longitude are not set.")
               (setq doom-theme (or (cdr-safe (cl-find-if (lambda (x) (eq (car x) :sunset))
                                                          circadian-themes))
                                    doom-theme)))))
  (setq doom-theme 'modus-vivendi))

(use-package! theme-magic
  :defer t
  :init
  (theme-magic-export-theme-mode +1))

(unless (display-graphic-p)
  (setq +ligatures-in-modes nil
        +ligatures-extras-in-modes nil))
