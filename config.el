;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar cae-config-finished-loading nil)

;;; Stuff that should not be disabled.

(when cae-init-core-enabled-p
  (load! "core"))


;;; UI

(when cae-init-ui-enabled-p
  (load! "ui"))


;;; Tools

(when cae-init-tools-enabled-p
  (load! "tools"))


;;; Editor

(when cae-init-editor-enabled-p
  (load! "editor"))


;;; Autocompletion

(when cae-init-autocompletion-enabled-p
  (load! "autocompletion"))


;;; Term

(when cae-init-term-enabled-p
  (load! "term"))


;;; Text

(when cae-init-text-enabled-p
  (load! "text"))


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
  (load! "email"))



;;; Applications

(when cae-init-applications-enabled-p
  (load! "applications"))


;;; Languages

(when cae-init-languages-enabled-p
  (load! "languages"))

;;; Appendix

(when cae-init-appendix-enabled-p
  (load! "appendix"))

(setq cae-config-finished-loading t)

;;Local Variables:
;;eval: (when (featurep 'aggressive-indent) (aggressive-indent-mode -1))
;;End:
