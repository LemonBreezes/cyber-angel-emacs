;;; appendix.el -*- lexical-binding: t; -*-

(doom-load-packages-incrementally
 `(,@(when (modulep! :completion corfu)
       '(corfu))
   ,@(when (modulep! :emacs dirvish)
       '(dired transient dirvish diredfl))
   bookmark auth-source tramp-compat tramp-integration tramp tramp-sh
   ,@(when (modulep! :term eshell)
       '(esh-util esh-module esh-proc esh-io esh-cmd eshell
         em-tramp em-smart em-banner em-basic em-cmpl
         em-extpipe em-glob em-hist em-ls em-script em-term
         em-alias em-elecslash em-rebind em-prompt))
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
       (nconc '(emms elfeed-tube empv somafm helm-emms lyrics-fetcher)
              (when (executable-find "mpd")
                '(mpc))))
   ,@(when (modulep! :cae ai)
       '(copilot whisper greader org-ai chatgpt-shell gptel magit-gptcommit
         aider aidermacs elysium minuet dall-e-shell))
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
       '(treemacs-lsp))
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
       (nconc                         ; The music apps are in a previous line.
        ;; System
        '(trashed pulseaudio-control disk-usage daemons neato-graph-bar
          journalctl-mode)
        ;; Games
        '(snake speed-type tetris bubbles dunnet autotetris chess klondike)
        ;; Eye candy
        '(fireplace flames-of-freedom snow zone zone-matrix zone-rainbow
          zone-nyan zone-pgm-spoopy selectric)
        ;; Insert
        '(lorem-ipsum password-generator uuidgen)
        ;; Random
        '(pomm debbugs pomm noaa hackernews leetcode)))
   ,@(when (modulep! :editor multiple-cursors)
       (if (modulep! :editor evil)
           '(evil-multiedit evil-mc)
         '(multiple-cursors)))
   ,@(when (modulep! :editor format)
       '(apheleia)))
 t)

(unless (or (executable-find "termux-setup-storage")
            (not (cae-display-graphic-p)))
  (after! pdf-tools
    (pdf-tools-install t nil t)))

;; Do not spam me with warnings.
(unless init-file-debug
  (setq warning-minimum-level :error
        warning-minimum-log-level :error))
