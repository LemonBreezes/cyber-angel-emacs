;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-theme-enable-modeline-bell t)
(defvar cae-theme-extend-heading-faces t)
(defvar cae-theme-export-theme-with-pywal (and (not (eq (cae-terminal-type) 0))
                                               (not (cae-running-in-ssh-p))))
(defvar cae-theme-enable-day-night-theme-switching (and (not (eq (cae-terminal-type) 0))
                                                        (not (cae-running-in-ssh-p))))
(defvar cae-theme-disable-outline-headings t)
(defvar cae-theme-enable-mixed-pitch-fonts (cae-display-graphic-p))

(defvar cae-modus-day-theme 'modus-operandi-tinted)
(defvar cae-modus-night-theme (if (cae-display-graphic-p)
                                  'modus-vivendi-tinted
                                ;; A little bit more legible in the Windows
                                ;; Terminal by default.
                                'modus-vivendi-tritanopia))
(defvar cae-ef-day-theme 'ef-trio-light)
(defvar cae-ef-night-theme 'ef-trio-dark)

(defvar cae-day-theme cae-modus-day-theme)
(defvar cae-night-theme cae-modus-night-theme)
(setq doom-theme (cond
                  ((cae-running-in-ssh-p) 'ef-melissa-dark)
                  ((eq (cae-terminal-type) 0) nil)
                  (t cae-night-theme)))

(when cae-theme-enable-modeline-bell
  (defface cae-modeline-bell-face
    '((t (:inherit mode-line-highlight)))
    "Face used for the modeline beep.")
  (setq visible-bell t
        ring-bell-function #'cae-theme-ring-bell-function))

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

;; Set up mixed pitch mode.
(when cae-theme-enable-mixed-pitch-fonts
  (defvar cae-theme-mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
    "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
  (defun cae-theme-init-mixed-pitch-h ()
    "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
    (when (memq major-mode cae-theme-mixed-pitch-modes)
      (mixed-pitch-mode 1))
    (dolist (hook cae-theme-mixed-pitch-modes)
      (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))

  (add-hook 'doom-init-ui-hook #'cae-theme-init-mixed-pitch-h))

;; Disable Outline highlighting
(when cae-theme-disable-outline-headings
  (cae-advice-add #'outline-minor-mode-highlight-buffer :override #'ignore)
  (after! outline
    (setq outline-font-lock-keywords nil)))

;; I can PR a fix to Doom once we drop support for Emacs 28.
(cae-defadvice! cae-run-theme-hook-h (_)
  :after #'consult-theme
  (run-hooks 'doom-load-theme-hook))
(add-hook! 'circadian-after-load-theme-hook
  (defun cae-run-theme-hook-h (_)
    (run-hooks 'doom-load-theme-hook)))

;; Make Org headlines respect the heading backgrounds.
(when cae-theme-extend-heading-faces
  ;;(after! org-modern
  ;;  ;; These features interfere with the heading backgrounds.
  ;;  (setq org-modern-tag nil
  ;;        org-modern-todo nil))

  (after! org
    (setq org-fontify-whole-heading-line t))
  (after! markdown-mode
    (setq markdown-fontify-whole-heading-line t))

  (use-package! backline
    :when (and cae-theme-extend-heading-faces
               (not cae-theme-disable-outline-headings))
    :defer t :init
    (cae-advice-add #'outline-flag-region :after #'backline-update)))

(defun cae-theme-customize-faces-h (_)
  ;; Make the mouse cursor more visible.
  (set-face-attribute 'mouse nil :background (face-foreground 'default nil t))

  ;; Remove redundant underline in `describe-mode'.
  (set-face-attribute 'separator-line nil :underline nil)

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
    (after! org-faces
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
        modus-themes-italic-constructs (cae-display-graphic-p)
        modus-themes-mixed-fonts t
        modus-themes-prompts `(,@(when (cae-display-graphic-p)
                                   '(italic))
                               semibold)
        modus-themes-to-toggle `(,cae-modus-day-theme ,cae-modus-night-theme)
        modus-themes-common-palette-overrides modus-themes-preset-overrides-intense))
(after! ef-themes
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-toggle `(,cae-ef-day-theme ,cae-ef-night-theme)))

(map! :leader :desc "Toggle theme" "tT" #'cae-theme-toggle)

;;; Set theme based on time of day.

(when cae-theme-enable-day-night-theme-switching
  (use-package! circadian
    :defer-incrementally t
    :defer t :config
    (setq circadian-verbose t)
    
    ;; Define themes based on fixed times or sunrise/sunset if location is available
    (defun cae-theme-configure-circadian-themes ()
      "Configure circadian themes based on location availability.
Uses sunrise/sunset when location is available, otherwise falls back to fixed times."
      (setq circadian-themes
            (if (and calendar-latitude calendar-longitude 
                     (not (= calendar-latitude 0))
                     (not (= calendar-longitude 0)))
                `((:sunrise . ,cae-modus-day-theme)
                  (:sunset . ,cae-modus-night-theme))
              `(("7:30" . ,cae-modus-day-theme)
                ("19:30" . ,cae-modus-night-theme)))))
    
    (cae-theme-configure-circadian-themes)
    
    ;; Add hook to update circadian when location changes
    (add-hook 'cae-geolocation-update-hook #'cae-theme-configure-circadian-themes -1)
    (add-hook 'cae-geolocation-update-hook #'circadian-setup)
    
    (if (and calendar-latitude calendar-longitude
             (not (= calendar-latitude 0))
             (not (= calendar-longitude 0)))
        (if doom-init-time
            (circadian-activate-current)
          (let ((hook (if (daemonp)
                          'server-after-make-frame-hook
                        'after-init-hook)))
            (remove-hook hook #'doom-init-theme-h)
            (add-hook hook #'circadian-setup -90)))
      (setq calendar-latitude 0
            calendar-longitude 0)
      (message "NOTICE: Calendar latitude and longitude are not set. Using fixed times for theme switching.")
      (doom-store-put 'circadian-themes (circadian-themes-parse))))

  ;; Cache the theme times so that we can set the theme on startup without loading
  ;; the circadian package.
  (add-hook! 'kill-emacs-hook
    (defun cae-theme-store-circadian-times-h ()
      (when (require 'circadian nil t)
        (doom-store-put 'circadian-themes (circadian-themes-parse)))))

  ;; Initialize location from cache if needed
  (unless (and calendar-latitude calendar-longitude
               (not (= 0 calendar-latitude))
               (not (= 0 calendar-longitude)))
    (let ((lat (doom-store-get 'calendar-latitude))
          (lng (doom-store-get 'calendar-longitude)))
      (when (and lat lng)
        (setq calendar-latitude lat
              calendar-longitude lng))))
              
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
    :defer t :defer t :defer-incrementally t)

  (after! (:all ewal theme-magic)
    (add-hook 'doom-load-theme-hook #'cae-theme-export-using-pywal :append)
    (cae-theme-export-using-pywal)))

(after! org
  (add-hook 'doom-load-theme-hook #'cae-theme-refresh-latex-images-previews-h))
(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

;;Local Variables:
;;eval: (unless (eq (cae-terminal-type) 0))
;;End:
