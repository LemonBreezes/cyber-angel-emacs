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
                        :background (face-attribute 'lazy-highlight :background nil t)))

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
        modus-themes-bold-constructs nil
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(italic semibold)
        modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)))
(after! ef-themes
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-trio-dark ef-trio-light)))

(setq doom-theme 'modus-vivendi-deuteranopia)

(map! :leader
      :desc "Toggle theme" "t T" #'modus-themes-toggle)

;;; Set theme based on time of day.

;; TODO I need separate functions for when it's night time and for when the
;; theme is dark. This is for if I want to use a dark theme during the day.
(defun cae-dark-theme-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))
(defun cae-night-mode-p ()
  (circadian-sunset))

(use-package! circadian
  ;;:defer t :defer-incrementally t
  :config
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
    (doom-store-put 'circadian-themes (circadian-themes-parse))))

;;;; Cache the theme times so that we can set the theme on startup without loading
;;;; the circadian package.
(add-hook 'kill-emacs-hook
          (cae-defun cae-theme-store-circadian-times-h ()
            (when (require 'circadian nil t)
              (doom-store-put 'circadian-themes (circadian-themes-parse)))))

;;;; Set the theme on startup.
;;(when (doom-store-get 'circadian-themes)
;;  (let* ((themes (doom-store-get 'circadian-themes))
;;         (now (reverse (cl-subseq (decode-time) 0 3)))
;;         (past-themes
;;          (cl-remove-if (lambda (entry)
;;                          (let ((theme-time (cl-first entry)))
;;                            (not (or (and (= (cl-first theme-time) (cl-first now))
;;                                          (<= (cl-second theme-time) (cl-second now)))
;;                                     (< (cl-first theme-time) (cl-first now))))))
;;                        themes))
;;         (entry (car (last (or past-themes themes))))
;;         (theme (cdr entry)))
;;    (setq doom-theme theme)))
