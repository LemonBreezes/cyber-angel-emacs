;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(defvar cae-theme-enable-modeline-bell t)
(defvar cae-theme-extend-heading-faces t)
(defvar cae-theme-export-theme-with-pywal t)
(defvar cae-theme-enable-day-night-theme-switching t)

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

(defadvice! cae-run-theme-hook-h (theme)
  :after #'consult-theme
  (run-hooks 'doom-load-theme-hook))

;; Make Org headlines respect the heading backgrounds.
(when cae-theme-extend-heading-faces
  (after! org-modern
    ;; These features interfere with the heading backgrounds.
    (setq org-modern-tag nil
          org-modern-todo nil))

  (use-package! backline
    :when cae-theme-extend-heading-faces
    :defer t :init
    (advice-add 'outline-flag-region :after 'backline-update)
    (after! org
      (setq org-fontify-whole-heading-line t))
    (after! markdown-mode
      (setq markdown-fontify-whole-heading-line t))))

(defun cae-theme-customize-faces-h (_)
  ;; Add a 3D mode-line effect if we're using a theme from `ef-themes'.
  ;; This only looks good for some of the Ef themes sadly.
  (when (and (eq (length custom-enabled-themes) 1)
             (string-prefix-p "ef-" (symbol-name (car custom-enabled-themes))))
    (set-face-attribute 'mode-line nil :box (face-foreground 'ef-themes-heading-0 nil t))
    (set-face-attribute 'mode-line-inactive nil :box (face-foreground 'shadow nil t)))

  (after! org
    ;; This is how I like my ellipsis to look. Subtle.
    (unless cae-theme-extend-heading-faces
      (set-face-attribute 'org-ellipsis nil
                          :inherit '(shadow default)
                          :weight 'normal))
    (unless (face-attribute 'org-document-title :height nil t)
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
                          :background 'unspecified)
      (set-face-attribute 'lsp-inlay-hint-face nil
                          :inherit 'shadow
                          :height 0.8)))
  ;; Use a more subtle background color for some packages.
  (let ((subtle-bg-color (face-attribute 'lazy-highlight :background nil t)))
    (after! goggles
      (set-face-attribute 'goggles-added nil :background subtle-bg-color))
    (after! beacon
      (setq beacon-color subtle-bg-color))
    (after! scrollkeeper
      (set-face-attribute 'scrollkeeper-guideline-highlight nil
                          :background subtle-bg-color)))

  (when cae-theme-extend-heading-faces
    (after! org
      (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                      org-level-6 org-level-7 org-level-8))
        (set-face-attribute face nil :extend t)))
    (after! outline
      (dolist (face '(outline-1 outline-2 outline-3 outline-4 outline-5
                      outline-6 outline-7 outline-8))
        (set-face-attribute face nil :extend t)))
    (after! markdown-mode
      (dolist (face '(markdown-header-face-1 markdown-header-face-2
                      markdown-header-face-3 markdown-header-face-4
                      markdown-header-face-5 markdown-header-face-6))
        (set-face-attribute face nil :extend t)))
    (after! shr
      (dolist (face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6))
        (set-face-attribute face nil :extend t)))
    (after! helpful
      (set-face-attribute 'helpful-heading nil :extend t)))

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
        modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted)
        modus-themes-common-palette-overrides modus-themes-preset-overrides-intense))
(after! ef-themes
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-trio-dark ef-trio-light)))

(setq doom-theme 'modus-vivendi-tinted)

(map! :leader :desc "Toggle theme" "tT" #'modus-themes-toggle)

;;; Set theme based on time of day.

(when cae-theme-enable-day-night-theme-switching
  (use-package! circadian
    :defer-incrementally t
    :defer t :config
    (setq circadian-themes
          '(("7:50" . modus-operandi-tinted)
            (:sunset  . modus-vivendi-tinted)))
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
      (doom-store-put 'circadian-themes (circadian-themes-parse))))

  ;; Cache the theme times so that we can set the theme on startup without loading
  ;; the circadian package.
  (add-hook! 'kill-emacs-hook
    (defun cae-theme-store-circadian-times-h ()
      (when (require 'circadian nil t)
        (doom-store-put 'circadian-themes (circadian-themes-parse)))))

  ;; Set the theme on startup.
  (if (and (doom-store-get 'circadian-themes)
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
        (setq doom-theme theme))
    ;; If we don't have `circadian-themes' cached, load `circadian' eagerly so
    ;; that we always have the correct theme on startup.
    (require 'circadian)))

(when cae-theme-export-theme-with-pywal
  (when cae-config-finished-loading
    (require 'ewal)
    (require 'theme-magic))

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
    (cae-theme-export-using-pywal)))

(after! org
  (add-hook 'doom-load-theme-hook #'cae-theme-refresh-latex-images-previews-h))
(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

(when cae-theme-enable-modeline-bell
  (defface cae-modeline-bell-face
    '((t (:inherit mode-line-highlight)))
    "Face used for the modeline beep.")
  (setq visible-bell t
        ring-bell-function #'cae-theme-ring-bell-function))
