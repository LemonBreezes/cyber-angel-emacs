;;; vanilla-emacs-configs/email-report-bug-not-inserting-template-mu4e.el -*- lexical-binding: t; -*-

(add-to-list 'safe-local-variable-directories (getenv "HOME"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Fastmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Fastmail"
                               (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "look@strawberrytea.xyz")
                  (user-full-name . "StrawberryTea")
                  (smtpmail-smtp-server . "smtp.fastmail.com")
                  (smtpmail-default-smtp-server . "smtp.fastmail.com")
                  (smtpmail-stream-type . tls)
                  (smtpmail-smtp-service . 465)
                  (mu4e-trash-folder . "/Fastmail/Trash")
                  (mu4e-refile-folder . "/Fastmail/Archive")
                  (mu4e-drafts-folder . "/Fastmail/Drafts")
                  (mu4e-sent-folder . "/Fastmail/Sent")))))
(setq mu4e-context-policy 'pick-first)
(setq mail-user-agent 'mu4e-user-agent
      message-mail-user-agent 'mu4e-user-agent)
(mu4e t)

(scratch-buffer)
(call-interactively #'report-emacs-bug)
