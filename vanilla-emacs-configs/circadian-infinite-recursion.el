;;; vanilla-emacs-configs/circadian-infinite-recursion.el -*- lexical-binding: t; -*-

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

(straight-use-package 'circadian)

(require 'calendar)
(setq calendar-latitude 30.1662208
      calendar-longitude -97.8386944)

(require 'circadian)

(setq circadian-themes
      '((:sunrise . (modus-operandi-tinted))
        (:sunset  . (modus-vivendi-tinted))))

(circadian-activate-current)
