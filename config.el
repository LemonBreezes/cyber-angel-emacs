;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Core aka stuff that should not be disabled.

(when cae-init-core-enabled-p
  (load! "core" doom-user-dir))

(when cae-init-tty-enabled-p
  (load! "lisp/cae-tty" doom-user-dir))

(when cae-init-bindings-enabled-p
  (load! "lisp/cae-bindings" doom-user-dir))

(when cae-init-multi-enabled-p
  (load! "lisp/cae-multi" doom-user-dir))

(when cae-init-smartparens-enabled-p
  (load! "lisp/cae-smartparens" doom-user-dir))

(when cae-init-projectile-enabled-p
  (load! "lisp/cae-projectile" doom-user-dir))


;;; UI

(when cae-init-ui-enabled-p
  (load! "ui" doom-user-dir))


;;; Tools

(when cae-init-tools-enabled-p
  (load! "tools" doom-user-dir))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "editor" doom-user-dir))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (load! "autocompletion" doom-user-dir))


;;; Term

(when cae-init-term-enabled-p
  (load! "term" doom-user-dir))


;;; Text

(when cae-init-text-enabled-p
  (load! "text" doom-user-dir))


;;; Email

;; Always enable the essential email configuration, including maildirs and addresses.
;; This is crucial for testing and debugging any issues with the email setup.
(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz"
      mail-source-directory "~/.mail/")
(make-directory "~/.mail/Fastmail/" t)

(after! mu4e
  (setq mu4e-eldoc-support t)
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
                    (mu4e-sent-folder . "/Fastmail/Sent"))))))

(after! message
  (setq message-default-mail-headers "Bcc: look@strawberrytea.xyz\n"))

(after! smtpmail
  (setq smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-default-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'tls
        smtpmail-queue-mail nil
        smtpmail-queue-dir "~/.mail/queued-mail/"
        smtpmail-servers-requiring-authorization ".*"
        smtpmail-smtp-user user-mail-address))

(setq +notmuch-sync-backend 'mbsync
      +notmuch-mail-folder "~/.mail/fastmail")

(when cae-init-email-enabled-p
  (load! "email" doom-user-dir))


;;; Applications

(when cae-init-applications-enabled-p
  (load! "applications" doom-user-dir))


;;; Languages

(when cae-init-languages-enabled-p
  (load! "languages" doom-user-dir))


;;; Appendix

(when cae-init-appendix-enabled-p
  (load! "appendix" doom-user-dir))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
