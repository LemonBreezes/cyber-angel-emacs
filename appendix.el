;;; appendix.el -*- lexical-binding: t; -*-

(doom-load-packages-incrementally
 `(,@(when (modulep! :completion corfu)
       '(corfu))
   ,@(when (modulep! :emacs dirvish)
       '(dired transient dirvish diredfl dirvish-widgets dirvish-side
         dirvish-subtree))
   bookmark auth-source tramp-compat tramp-integration tramp tramp-sh
   ,@(when (and (getenv "INSIDE_EXWM") (modulep! :cae exwm))
       '(xelb exwm exwm-mff exwm-evil exwm-firefox-evil app-launcher))
   ,@(when (modulep! :term eshell)
       '(esh-util esh-module esh-proc esh-io esh-cmd eshell
         em-tramp em-smart em-banner em-basic em-cmpl
         em-extpipe em-glob em-hist em-ls em-script em-term
         em-alias em-elecslash em-rebind em-prompt em-pred))
   ,@(when (and (modulep! :term vterm)
                (executable-find "cmake"))
       '(vterm))
   ,@(when (modulep! :ui hydra)
       '(hydra))
   ,@(when (modulep! :email mu4e)
       '(mu4e))
   ,@(when (or (modulep! :completion helm)
               (modulep! :cae helm))
       '(async helm-lib helm-multi-match helm-source helm-core
         helm-global-bindings helm))
   ,@(when (modulep! :app rss)
       '(elfeed))
   ,@(when (modulep! :cae misc-applications) ; Music apps
       (append '(emms elfeed-tube empv somafm helm-emms lyrics-fetcher)
               (when (executable-find "mpd")
                 '(mpc))))
   ,@(when (modulep! :cae ai)
       (append
        (when (modulep! :cae lsp +copilot)
          '(copilot))
        (when (modulep! :cae lsp -copilot)
          '(minuet))
        '(chatgpt-shell gptel magit-gptcommit
          aidermacs dall-e-shell forge-llm)))
   ,@(when (modulep! :tools direnv)
       '(envrc))
   ,@(when (and (modulep! :tools lsp)
                (not (modulep! :tools lsp +eglot)))
       '(lsp-mode lsp-ui))
   ,@(when (and (modulep! :tools lsp)
                (not (modulep! :tools lsp +eglot))
                (modulep! :lang cc))
       '(lsp-clangd))
   ,@(when (modulep! :tools lsp +eglot)
       '(eglot))
   ,@'(eww)
   ,@(when (modulep! :tools magit)
       '(parrot))
   ,@'(parrot-rotate)
   ,@(when (modulep! :cae gnus)
       '(gnus gnus-group gnus-sum gnus-srvr))
   ,@(when (modulep! :tools pdf)
       '(image-mode pdf-util pdf-info pdf-cache pdf-view pdf-tools))
   ,@(when (modulep! :ui treemacs)
       '(treemacs))
   ,@(when (and (modulep! :ui treemacs +lsp)
                (not (modulep! :tools lsp +eglot)))
       '(lsp-treemacs))
   ,@(when (modulep! :app rss +org)
       '(elfeed-org))
   ,@(when (and (modulep! :completion vertico)
                (modulep! :cae misc-applications))
       '(consult-gh))
   ,@(when (and (modulep! :completion vertico)
                (modulep! :email mu4e)
                (executable-find "mu"))
       '(consult-mu))
   ,@(when (and (or (modulep! :completion helm)
                    (modulep! :cae helm))
                (modulep! :cae misc-applications))
       '(helm-system-packages helm-emms helm-linux-disks helm-rage))
   ,@(when (modulep! :cae misc-applications)
       (append                          ; The music apps are in a previous line.
        ;; System
        '(trashed pulseaudio-control disk-usage daemons neato-graph-bar
          journalctl-mode)
        ;; Games
        '(snake speed-type tetris bubbles dunnet autotetris-mode chess klondike)
        ;; Eye candy
        '(fireplace flames-of-freedom snow zone zone-matrix zone-rainbow
          zone-nyan zone-pgm-spoopy selectric-mode)
        ;; Insert
        '(lorem-ipsum password-generator uuidgen)
        ;; Random
        '(pomm debbugs pomm noaa hackernews leetcode)))
   ,@(when (modulep! :editor multiple-cursors)
       (if (modulep! :editor evil)
           '(evil-multiedit evil-mc)
         '(multiple-cursors)))
   ,@(when (modulep! :editor format)
       '(apheleia))
   anzu isearch-mb
   ,@(when (modulep! :editor evil)
       '(evil-anzu))))
(setq doom-incremental-packages
      (delq t doom-incremental-packages))

(unless (or (executable-find "termux-setup-storage")
            (not (cae-display-graphic-p)))
  (let* ((pdf-tools-load-path (file-name-directory (locate-library "pdf-tools")))
         (straight-base (file-name-directory
                         (directory-file-name
                          (file-name-directory
                           (directory-file-name pdf-tools-load-path)))))
         (build-dir (expand-file-name "repos/pdf-tools/server" straight-base))
         (target-dir pdf-tools-load-path)
         (epdfinfo-path (expand-file-name "epdfinfo" target-dir)))
    (if (and (file-exists-p epdfinfo-path)
             (file-executable-p epdfinfo-path)
             (ignore-errors
               (setq pdf-info-epdfinfo-program epdfinfo-path)
               (pdf-info-check-epdfinfo)
               t))
        (pdf-tools-install-noverify)
      (pdf-tools-build-server
       target-dir
       nil nil
       (lambda (executable)
         (when executable
           (setq pdf-info-epdfinfo-program executable)
           (pdf-tools-install-noverify)))
       build-dir))))

(load! "lisp/cae-compile" doom-user-dir)

;; Do not spam me with warnings.
(unless init-file-debug
  (setq warning-minimum-level :error
        warning-minimum-log-level :warning))
