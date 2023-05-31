;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

(defun cae-theme-customize-faces-h (_)
  (when (modulep! :lang org)
    (after! org
      ;; This is how I like my ellipsis to look. Subtle.
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
    ;; For `lsp-ui'. Fixes the background color of the doc popup.
    (after! lsp-mode
      (set-face-attribute 'markdown-code-face nil
                          :background 'unspecified)))
  (after! goggles
    ;; Make the goggles more subtle.
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
    (setq! modus-themes-org-blocks 'gray-background
           modus-themes-slanted-constructs t
           modus-themes-bold-constructs nil
           modus-themes-variable-pitch-ui t
           modus-themes-mixed-fonts t
           modus-themes-prompts '(italic semibold))))
(after! ef-themes
  (setq! ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t
          ef-themes-to-toggle '(ef-trio-light ef-trio-dark)))

(map! :leader
      :desc "Toggle theme" "t T" #'modus-themes-toggle)

;;; Set theme based on time of day.

(use-package! circadian
  :defer-incrementally t
  :config
  (setq circadian-themes
        '((:sunrise . modus-operandi-tinted)
          (:sunset  . modus-vivendi-tinted)))
  (if (and calendar-latitude calendar-longitude)
      (let ((hook (if (daemonp)
                      'server-after-make-frame-hook
                    'after-init-hook)))
        (remove-hook hook #'doom-init-theme-h)
        (add-hook hook #'circadian-setup -90))
    (setq calendar-latitude 0
          calendar-longitude 0)
    (message "ERROR: Calendar latitude and longitude are not set.")))

(add-hook 'kill-emacs-hook
          (cae-defun cae-theme-store-cicadian-times-h ()
            (let* ((themes (circadian-themes-parse))
                   (now (circadian-now-time))
                   (past-themes (circadian-filter-inactivate-themes themes now))
                   (entry (car (last (or past-themes themes))))
                   (theme (cdr entry))
                   (next-entry (or (cadr (member entry themes))
                                   (if (circadian-a-earlier-b-p (circadian-now-time) (cl-first entry))
                                       (car themes))))
                   (next-time (if next-entry
                                  (circadian--encode-time
                                   (cl-first (cl-first next-entry))
                                   (cl-second (cl-first next-entry)))
                                (+ (* (+ (- 23 (cl-first now)) (cl-first (cl-first (cl-first themes)))) 60 60)
                                   (* (+ (- 60 (cl-second now)) (cl-second (cl-first (cl-first themes)))) 60)))))
              (+log themes now past-themes entry theme next-entry next-time)
              )))
