;;; ~/.doom.d/lisp/cae-theme.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;;; ----------------------------------------------------------------------------
;;; Options and theme selection
;;; ----------------------------------------------------------------------------

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
                                  'modus-vivendi
                                ;; A little bit more legible in the Windows
                                ;; Terminal by default.
                                'modus-vivendi-tritanopia))
(defvar cae-ef-day-theme 'ef-trio-light)
(defvar cae-ef-night-theme 'ef-trio-dark)
(defvar cae-circadian-fixed-day-time "7:30")
(defvar cae-circadian-fixed-night-time "19:30")
(defvar cae-circadian-used-fixed-times nil)

;; Choose theme family (example, could be a defcustom)
(defvar cae-theme-family 'modus) ; or 'ef

;; Set day/night themes based on family
(defvar cae-day-theme (if (eq cae-theme-family 'modus) cae-modus-day-theme cae-ef-day-theme))
(defvar cae-night-theme (if (eq cae-theme-family 'modus) cae-modus-night-theme cae-ef-night-theme))

;;; ----------------------------------------------------------------------------
;;; Modeline bell
;;; ----------------------------------------------------------------------------

(when cae-theme-enable-modeline-bell
  (defface cae-modeline-bell-face
    '((t (:inherit mode-line-highlight)))
    "Face used for the modeline beep.")
  (setq visible-bell t
        ring-bell-function #'cae-theme-ring-bell-function))

;;; ----------------------------------------------------------------------------
;;; Mixed-pitch fonts
;;; ----------------------------------------------------------------------------

(when cae-theme-enable-mixed-pitch-fonts
  (defvar cae-theme-mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
    "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
  (defun cae-theme-init-mixed-pitch-h ()
    "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
    (when (memq major-mode cae-theme-mixed-pitch-modes)
      (mixed-pitch-mode 1))
    (dolist (hook cae-theme-mixed-pitch-modes)
      (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode))
    (when (memq 'org-mode cae-theme-mixed-pitch-modes)
      (add-hook 'org-mode-hook #'valign-mode)))

  (add-hook 'doom-init-ui-hook #'cae-theme-init-mixed-pitch-h))

;;; ----------------------------------------------------------------------------
;;; Outline / heading faces
;;; ----------------------------------------------------------------------------

;; Disable Outline highlighting
(when cae-theme-disable-outline-headings
  (cae-advice-add #'outline-minor-mode-highlight-buffer :override #'ignore)
  (after! outline
    (setq outline-font-lock-keywords nil)))

;; Make Org headlines respect the heading backgrounds.
(when cae-theme-extend-heading-faces
  (after! org-modern
    ;; These features interfere with the heading backgrounds.
    (setq org-modern-tag nil
          org-modern-todo nil
          org-modern-priority nil))

  (after! org
    (setq org-fontify-whole-heading-line t))
  (after! markdown-mode
    (setq markdown-fontify-whole-heading-line t))

  (use-package! backline
    :when (and cae-theme-extend-heading-faces
               (not cae-theme-disable-outline-headings))
    :defer t :init
    (cae-advice-add #'outline-flag-region :after #'backline-update)))

;;; ----------------------------------------------------------------------------
;;; Re-run the theme hook for commands that bypass it
;;; ----------------------------------------------------------------------------

;; I can PR a fix to Doom once we drop support for Emacs 28.
(cae-defadvice! cae-run-theme-hook-h (_)
  :after #'consult-theme
  (run-hooks 'doom-load-theme-hook))
(add-hook! 'circadian-after-load-theme-hook
  (defun cae-run-theme-hook-h (_)
    (run-hooks 'doom-load-theme-hook)))

;;; ----------------------------------------------------------------------------
;;; Custom face tweaks
;;; ----------------------------------------------------------------------------

(defun cae-theme-customize-faces-h (_)
  "Apply personal face tweaks on top of whichever theme is enabled.
Run from `enable-theme-functions' (see the `add-hook' below)."
  ;; --- General ---
  ;; Make the mouse cursor more visible.
  (set-face-attribute 'mouse nil :background (face-foreground 'default nil t))
  ;; Remove redundant underline in `describe-mode'.
  (set-face-attribute 'separator-line nil :underline nil)

  ;; --- Org ---
  (after! org
    ;; This is how I like my ellipsis to look. Subtle.
    (unless cae-theme-extend-heading-faces
      (set-face-attribute 'org-ellipsis nil
                          :inherit '(shadow default)
                          :weight 'normal))
    (unless (face-attribute 'org-document-title :height nil t)
      (set-face-attribute 'org-document-title nil
                          :height 1.2)))

  ;; --- Completion popups (company / esh-autosuggest) ---
  (after! company
    (set-face-attribute 'company-preview-common nil
                        :inherit 'shadow
                        :background 'unspecified)
    (set-face-attribute 'company-preview nil
                        :inherit 'shadow
                        :background 'unspecified))

  ;; --- lsp-ui doc popup ---
  (when (and (modulep! :tools lsp)
             (not (modulep! :tools lsp +eglot)))
    ;; For `lsp-ui'. Fixes the background color of the doc popup.
    (after! lsp-mode
      (set-face-attribute 'markdown-code-face nil
                          :background 'unspecified)
      (set-face-attribute 'lsp-inlay-hint-face nil
                          :inherit 'shadow
                          :height 0.8)))

  ;; --- Subtler backgrounds for highlight packages ---
  (let ((subtle-bg-color (face-attribute 'lazy-highlight :background nil t)))
    (when (stringp subtle-bg-color)
      (after! goggles
        (set-face-attribute 'goggles-added nil :background subtle-bg-color))
      (after! beacon
        (setq beacon-color subtle-bg-color))
      (after! scrollkeeper
        (set-face-attribute 'scrollkeeper-guideline-highlight nil
                            :background subtle-bg-color))))

  ;; --- Extend heading faces to the edge of the window ---
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

  ;; --- Remove bold constructs ---
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

(add-hook 'enable-theme-functions #'cae-theme-customize-faces-h)

;;; ----------------------------------------------------------------------------
;;; Theme package settings (modus / ef)
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Keybindings
;;; ----------------------------------------------------------------------------

(map! :leader :desc "Toggle theme" "tT" #'cae-theme-toggle)

;;; ----------------------------------------------------------------------------
;;; Day/night switching (circadian)
;;; ----------------------------------------------------------------------------

(use-package! circadian
  :when cae-theme-enable-day-night-theme-switching
  :defer t :defer-incrementally t :init
  ;; Add the hook to update circadian when geolocation changes.
  (add-hook 'cae-geolocation-update-hook #'cae-theme--update-circadian-on-location-change)
  :config
  ;; Initial configuration when circadian loads.
  (cae-theme--configure-circadian)

  ;; Cache the theme times on exit.
  (add-hook! 'kill-emacs-hook
    (defun cae-theme-store-circadian-times-h ()
      (when (and (boundp 'circadian-themes) circadian-themes)
        (doom-store-put 'circadian-themes (circadian-themes-parse))))))

;; Set the initial theme based on time and cached circadian data if available.
(cond
 ((eq (cae-terminal-type) 0)
  (setq doom-theme 'modus-vivendi-deuteranopia))
 ((and cae-theme-enable-day-night-theme-switching
       (doom-store-get 'circadian-themes))
  ;; Try setting theme from cache
  (let* ((themes (doom-store-get 'circadian-themes))
         (now (reverse (cl-subseq (decode-time) 0 3))) ; (hour min sec)
         (past-themes
          (cl-remove-if-not (lambda (entry)
                              (let ((theme-time (car entry))) ; (hour min)
                                (or (< (car theme-time) (car now)) ; Earlier hour
                                    (and (= (car theme-time) (car now)) ; Same hour, earlier or equal minute
                                         (<= (cadr theme-time) (cadr now))))))
                            themes))
         (entry (car (last (or past-themes themes)))) ; Last past theme, or last overall if none are past
         (theme (cdr entry)))
    (setq doom-theme theme)))
 (t
  ;; Fallback if cache unavailable or day/night switching disabled
  (setq doom-theme (if (cae-night-time-p) cae-night-theme cae-day-theme))))

;;; ----------------------------------------------------------------------------
;;; Export theme to the rest of Linux (pywal)
;;;
;;; Everything here runs asynchronously so loading a theme never blocks Emacs:
;;;   - the pywal Python script is run via `start-process' (see `:config'), and
;;;     its completion drives `cae-theme-magic-after-apply-functions';
;;;   - the WM-bar reloads use `call-process' with a destination of 0, i.e.
;;;     fire-and-forget AND detached, so the relaunched bar outlives Emacs.
;;; ----------------------------------------------------------------------------

(use-package! theme-magic
  :if cae-theme-export-theme-with-pywal
  :defer t :defer-incrementally t :init

  ;; --- WM-bar reload helpers (async, detached) ---
  (defun cae-theme-magic-reload-stumpwm-h ()
    "Ask a running StumpWM to reload its colors.
Fire-and-forget: a `call-process' destination of 0 runs the command
asynchronously and detached, so it never blocks Emacs."
    (when (and (executable-find "stumpish")
               (equal (cae-wm-name) "stumpwm"))
      (call-process "stumpish" nil 0 nil "reload-colors")))
  (defun cae-theme-magic-reload-polybar-h ()
    "Relaunch the polybar bars after a theme change (async, detached).
pywal's reload runs `pkill -USR1 polybar', which terminates polybar 3.x
rather than reloading it, so relaunch the bars instead.  StumpWM draws
its own mode-line (handled above); ratpoison advertises no EWMH name
(`cae-wm-name' is nil) and needs the override-redirect bar.  Run via
`call-process' destination 0 so the new bar is detached from Emacs."
    (let ((wm (cae-wm-name))
          (launch (expand-file-name "~/.config/polybar/launch.sh")))
      (when (and (not (equal wm "stumpwm"))
                 (file-executable-p launch))
        (call-process launch nil 0 nil (if wm "example" "example-ratpoison")))))

  ;; --- Export orchestration ---
  (defvar cae-theme-magic-after-apply-functions nil
    "Abnormal hook run when an async pywal export finishes.
Each function receives one argument: non-nil if pywal exited
successfully.  Since pywal now runs asynchronously (see the `:config'
overrides below), theme-dependent side effects such as reloading the WM
bar must run from here, once the colors have actually been written.")
  (defun cae-theme-magic-export-theme-h ()
    "Export the current theme to the rest of Linux via pywal (async).
The bar reloads and the last-applied bookkeeping run from
`cae-theme-magic-after-apply-functions' once pywal finishes."
    (unless (eq (car custom-enabled-themes)
                (doom-store-get 'cae-theme-last-applied))
      (theme-magic-from-emacs)))
  (defun cae-theme-magic-after-apply-h (success)
    "Reload the WM bar and record the applied theme after pywal SUCCESS."
    (when success
      (cae-theme-magic-reload-stumpwm-h)
      (cae-theme-magic-reload-polybar-h)
      (doom-store-put 'cae-theme-last-applied (car custom-enabled-themes))))
  (add-hook 'cae-theme-magic-after-apply-functions #'cae-theme-magic-after-apply-h)
  (add-hook 'doom-load-theme-hook #'cae-theme-magic-export-theme-h)

  :config
  ;; --- Async pywal override ---
  ;; Upstream `theme-magic--call-pywal-process' blocks Emacs on a synchronous
  ;; `call-process' until the Python script finishes.  Override it to use
  ;; `start-process' and report the result from a sentinel instead, so loading
  ;; a theme never freezes the UI.
  (defun cae-theme-magic--pywal-sentinel (proc _event)
    "Report the result of async pywal PROC and run the after-apply hook."
    (when (memq (process-status proc) '(exit signal))
      (let ((ok (and (eq (process-status proc) 'exit)
                     (zerop (process-exit-status proc)))))
        (if ok
            (message "Successfully applied colors!")
          (message "theme-magic: error applying colors (see buffer %S)"
                   theme-magic--pywal-buffer-name))
        (run-hook-with-args 'cae-theme-magic-after-apply-functions ok))))
  (defadvice! cae-theme-magic--call-pywal-async-a (colors)
    "Run the pywal theming script asynchronously via `start-process'.
Returns the process object; completion is handled by
`cae-theme-magic--pywal-sentinel'."
    :override #'theme-magic--call-pywal-process
    (theme-magic--erase-pywal-buffer)
    (let* ((default-directory "~/")
           (process-connection-type nil)
           (proc (apply #'start-process
                        "theme-magic-pywal"
                        theme-magic--pywal-buffer-name
                        "python"
                        theme-magic--pywal-python-script
                        colors)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'cae-theme-magic--pywal-sentinel)
      proc))
  (defadvice! cae-theme-magic--apply-colors-async-a (colors)
    "Async variant of `theme-magic--apply-colors-with-pywal'.
Starts pywal and lets the sentinel report success/failure rather than
branching on a synchronous exit code."
    :override #'theme-magic--apply-colors-with-pywal
    (message "Applying colors:\n%s"
             (cl-mapcar #'cons
                        (number-sequence 0 (length colors))
                        colors))
    (theme-magic--call-pywal-process colors)))

;;; ----------------------------------------------------------------------------
;;; Org LaTeX previews
;;; ----------------------------------------------------------------------------

(after! org
  (add-hook 'doom-load-theme-hook #'cae-theme-refresh-latex-images-previews-h))
(after! org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

;;Local Variables:
;;eval: (unless (eq (cae-terminal-type) 0))
;;End:
