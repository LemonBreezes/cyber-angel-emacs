;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(defvar cae-theme-enable-modeline-bell t)
(defvar cae-theme-extend-heading-faces t)

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

;; Make Org headlines respect the heading backgrounds.
(when cae-theme-extend-heading-faces
  (advice-add #'org-fold-core-region :around #'cae-org-fold-region-a)
  (defun cae-org-fold-region-a (oldfun from to flag &optional spec-or-alias)
    (let ((shift-fold-p (and (eq to (point-max)) (not (eq from to)) flag)))
      (when shift-fold-p
        (setq to (1- to)))
      (funcall oldfun from to flag spec-or-alias)
      (remove-overlays from (1+ to) 'cae-org-fold-heading t)
      (when flag
        (let ((o (make-overlay to (1+ to) nil 'front-advance)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'cae-org-fold-heading t)
          (overlay-put o 'face (save-excursion (goto-char from) (face-at-point)))
          (when shift-fold-p
            (overlay-put o 'display "\n"))))))

  (use-package! backline
    :when cae-theme-extend-heading-faces
    :defer t :init
    (advice-add 'outline-flag-region :after 'backline-update)
    (after! org
      (setq org-fontify-whole-heading-line t))
    (after! markdown-mode
      (setq markdown-fontify-whole-heading-line t))))

(defun cae-theme-customize-faces-h (_)
  (when (modulep! :lang org)
    (after! org
      ;; This is how I like my ellipsis to look. Subtle.
      (unless cae-theme-extend-heading-faces
        (set-face-attribute 'org-ellipsis nil
                            :inherit '(shadow default)
                            :weight 'normal))
      (unless (face-attribute 'org-document-title :height nil t)
        (set-face-attribute 'org-document-title nil
                            :height 1.2))))
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

  ;; The backgrounds currently break
  (when cae-theme-extend-heading-faces
    (after! org
      (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                      org-level-6 org-level-7 org-level-8))
        (set-face-attribute face nil :extend t)))
    (after! outline
      (dolist (face '(outline-1 outline-2 outline-3 outline-4 outline-5
                      outline-6 outline-7 outline-8))
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
        modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)
        ;; I have this mostly because I want to work on getting the heading
        ;; backgrounds working with the code folding!
        modus-themes-common-palette-overrides modus-themes-preset-overrides-intense))
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
  (require 'circadian))

(use-package! ewal
  :defer-incrementally t
  :defer t :init
  ;; Use all 16 colors from our palette, not just the primary 8.
  (setq ewal-ansi-color-name-symbols '(black red green yellow blue magenta cyan white
                                       brightblack brightred brightgreen brightyellow
                                       brightblue brightmagenta brightcyan brightwhite)))
(use-package! theme-magic
  :defer t :defer-incrementally t)

(unless cae-config-finished-loading
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
        ring-bell-function (lambda ()
                             (let ((buf (current-buffer))
                                   (cookie (face-remap-add-relative 'mode-line-active 'cae-modeline-bell-face)))
                               (force-mode-line-update)
                               (run-with-timer 0.1 nil
                                               (lambda ()
                                                 (with-current-buffer buf
                                                   (face-remap-remove-relative cookie)
                                                   (force-mode-line-update))))))))
