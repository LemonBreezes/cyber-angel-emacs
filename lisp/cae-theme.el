;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(setq doom-theme 'modus-operandi)

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

(defun cae-theme-customize-faces-h (theme)
  (after! org
    (set-face-attribute 'org-ellipsis nil
                        :inherit '(shadow default)
                        :weight 'normal)
    (set-face-attribute 'org-document-title nil
                        :height 1.2))
  (after! company
    (set-face-attribute 'company-preview-common nil
                        :inherit 'shadow
                        :background 'unspecified)
    (set-face-attribute 'company-preview nil
                        :inherit 'shadow
                        :background 'unspecified))
  (after! markdown-mode
    (set-face-attribute 'markdown-code-face nil
                        :background 'unspecified)))

(after! modus-themes
  (let ((modus-themes-custom-auto-reload nil))
    (setq! modus-themes-org-blocks 'gray-background
           modus-themes-slanted-constructs t
           modus-themes-bold-constructs nil
           modus-themes-variable-pitch-ui t
           modus-themes-mixed-fonts t
           modus-themes-prompts '(italic semibold))))

;; Set theme based on time
(when (display-graphic-p)
  (advice-add #'doom-init-theme-h :override #'ignore)
  (use-package! circadian
    :config
    (cond ((memq doom-theme '(modus-operandi modus-vivendi))
           (setq! circadian-themes '(("7:15" . modus-operandi)
                                     ("19:30" . modus-vivendi))))
          (t (setq! circadian-themes `(("0:00" . ,doom-theme)))))
    (circadian-setup)
    (after! exwm-randr
      (add-hook 'doom-load-theme-hook #'exwm-randr-refresh))))

(map! "<f5>" #'modus-themes-toggle)
