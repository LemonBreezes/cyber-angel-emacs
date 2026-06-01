;;; email.el -*- lexical-binding: t; -*-

(setq report-emacs-bug-no-explanations t)
(autoload 'async-smtpmail-send-it "smtpmail-async" nil t)
(setq compose-mail-user-agent-warnings nil)
(after! sendmail
  (setq send-mail-function #'async-smtpmail-send-it
        mail-self-blind t))
(after! message
  (setq message-send-mail-function #'async-smtpmail-send-it
        message-forward-as-mime t
        message-forward-before-signature t)
  (add-hook 'message-setup-hook #'message-check-recipients))

(setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
(after! notmuch
  (map! :map notmuch-search-mode-map
        "q" #'cae-notmuch-quit))
(after! notmuch-hello
  (map! :map notmuch-hello-mode-map
        "q" #'cae-notmuch-quit))

(when (modulep! :email mu4e)
  (map! [remap compose-mail] #'+mu4e/compose)
  (after! mu4e-vars
    (setq mu4e-modeline-support t
          mu4e-notification-support t)))
