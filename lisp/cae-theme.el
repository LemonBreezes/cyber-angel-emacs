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
    ;; Make the `goggles' more subtle.
    (set-face-attribute 'goggles-added nil
                        :background (face-attribute 'lazy-highlight :background nil t)))
  (after! beacon
    ;; Make the `beacon' more subtle.
    (setq beacon-color (face-attribute 'lazy-highlight :background nil t)))

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
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(italic semibold)
        modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)))
(after! ef-themes
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-trio-dark ef-trio-light)))

(setq doom-theme 'modus-vivendi-deuteranopia)

(map! :leader :desc "Toggle theme" "tT" #'modus-themes-toggle)

;;; Set theme based on time of day.

(use-package! circadian
  :defer-incrementally t
  :defer t :config
  (setq circadian-themes
        '((:sunrise . modus-operandi-deuteranopia)
          (:sunset  . modus-vivendi-deuteranopia)))
  (if (and calendar-latitude calendar-longitude)
      (if doom-init-time
          (circadian-setup)
        (let ((hook (if (daemonp)
                        'server-after-make-frame-hook
                      'after-init-hook)))
          (remove-hook hook #'doom-init-theme-h)
          (add-hook hook #'circadian-setup -90)))
    (setq calendar-latitude 0
          calendar-longitude 0)
    (message "ERROR: Calendar latitude and longitude are not set.")
    (doom-store-put 'circadian-themes (circadian-themes-parse))
    (doom-store-put 'circadian-sunset (circadian-sunset))
    (doom-store-put 'circadian-sunrise (circadian-sunrise))))

;; Cache the theme times so that we can set the theme on startup without loading
;; the circadian package.
(add-hook 'kill-emacs-hook
          (cae-defun cae-theme-store-circadian-times-h ()
            (when (require 'circadian nil t)
              (doom-store-put 'circadian-themes (circadian-themes-parse))
              (doom-store-put 'circadian-sunset (circadian-sunset))
              (doom-store-put 'circadian-sunrise (circadian-sunrise)))))

;; Set the theme on startup.
(when (and (doom-store-get 'circadian-themes)
           (not (symbolp (caar (doom-store-get 'circadian-themes))))
           (not cae-config-finished-loading))
  (let* ((themes (doom-store-get 'circadian-themes))
         (now (reverse (cl-subseq (decode-time) 0 3)))
         (past-themes
          (cl-remove-if (lambda (entry)
                          (let ((theme-time (cl-first entry)))
                            (not (or (and (= (cl-first theme-time) (cl-first now))
                                          (<= (cl-second theme-time) (cl-second now)))
                                     (< (cl-first theme-time) (cl-first now))))))
                        themes))
         (entry (car (last (or past-themes themes))))
         (theme (cdr entry)))
    (setq doom-theme theme)))

(use-package! ewal
  :defer-incrementally t
  :defer t :init
  ;; Use all 16 colors from our palette, not just the primary 8.
  (setq ewal-ansi-color-name-symbols '(black red green yellow blue magenta cyan white
                                       brightblack brightred brightgreen brightyellow
                                       brightblue brightmagenta brightcyan brightwhite)))
(use-package! theme-magic
  :defer t :defer-incrementally t)

(after! (:all ewal theme-magic)
  (add-hook 'doom-load-theme-hook #'cae-theme-export-using-pywal :append)
  (cae-theme-export-using-pywal))
