;;; vanilla-emacs-configs/elysium-not-inserting-code.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(elysium :recipe (:host github :repo "lanceberge/elysium")))
(require 'elysium)

(defvar cae-anthropic-default-model "claude-3-5-sonnet-20240620")

;; Get the API key.
(ignore-errors
  (load-file "~/.config/doom/secrets/secrets.el"))

(setq
 gptel-model cae-anthropic-default-model
 gptel-backend (gptel-make-anthropic "Claude"
                 :stream t :key (getenv "ANTHROPIC_API_KEY")))

;; For preventing error
(setq gptel-use-curl nil)

(global-set-key (kbd "<f5>") #'elysium-query)
(global-set-key (kbd "<f2>") #'gptel-menu)
(global-set-key (kbd "<f6>") #'gptel-send)
