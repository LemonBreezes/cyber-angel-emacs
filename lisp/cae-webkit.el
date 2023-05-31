;;; ~/.doom.d/lisp/cae-webkit.el -*- lexical-binding: t; -*-

;; This file has been deprecated because `webkit' has not been updated for over
;; a year and no longer works on my systems.

(use-package! webkit
  :defer t
  :init
  ;; Fix an error
  (autoload 'org-link-set-parameters "ol")
  (autoload 'org-link-types "ol")

  ;; Stop the weird blinking of the Webkit window when in focus.
  (modify-frame-parameters nil '((inhibit-double-buffering . t)))
  :config
  (require 'webkit-ace)  ;; If you want link hinting
  (require 'webkit-dark) ;; If you want to use the simple dark mode

  ;; Set webkit as the default browse-url browser
  (setq browse-url-browser-function 'webkit-browse-url)

  ;; Override the "loading:" mode line indicator with an icon from `all-the-icons.el'
  ;; You could also use a unicode icon like ↺
  (defun webkit--display-progress (progress)
    (setq webkit--progress-formatted
          (if (equal progress 100.0)
              ""
            (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
    (force-mode-line-update))

  ;; Set action to be taken on a download request. Predefined actions are
  ;; `webkit-download-default', `webkit-download-save', and `webkit-download-open'
  ;; where the save function saves to the download directory, the open function
  ;; opens in a temp buffer and the default function interactively prompts.
  (setq webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                       ("\\.png\\'" . webkit-download-save)
                                       (".*" . webkit-download-default)))

  ;; Globally use the simple dark mode
  (setq webkit-dark-mode t)

  (add-hook 'webkit-mode-hook #'doom-mark-buffer-as-real-h))
