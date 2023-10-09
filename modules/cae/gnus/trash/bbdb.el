;;; private/gnus/trash/bbdb.el -*- lexical-binding: t; -*-

(use-package! bbdb
  :after gnus :init
  (add-hook 'message-mode-hook
            (function (lambda()
                        (local-set-key (kbd "<tab>") 'bbdb-complete-mail))))
  :defer t :config
  (require 'bbdb-com)
  (require 'bbdb-gnus)
  (bbdb-initialize 'message 'gnus)
  (bbdb-mua-auto-update-init 'message 'gnus)
  (setq bbdb-mua-pop-up nil)
  (setq bbdb-allow-duplicates t)
  (setq bbdb-pop-up-window-size 5)
  (setq bbdb-ignore-redundant-mails t)
  (setq bbdb-update-records-p 'create)
  (setq bbdb-mua-interactive-action '(create . query))
  (setq bbdb-mua-auto-update-p 'create)
  (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases)
  (add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
  (add-hook 'kill-emacs-hook 'bbdb-save)
  (setq bbdb-completion-display-record t
        bbdb-default-area-code nil
        bbdb-add-aka t
        bbdb-add-name nil
        bbdb-add-mails t
        bbdb-ignore-message-alist '(("Newsgroup" . ".*")))
  (defalias 'bbdb-y-or-n-p #'(lambda (prompt) t))
  (setq bbdb-auto-notes-rules
        '(("Newsgroups" ("[^,]+" newsgroups 0))
          ("Subject" (".*" last-subj 0 t))
          ("User-Agent" (".*" mailer 0))
          ("X-Mailer" (".*" mailer 0))
          ("Organization" (".*" organization 0))
          ("X-Newsreader" (".*" mailer 0))
          ("X-Face" (".+" face 0 'replace))
          ("Face" (".+" face 0 'replace)))))
