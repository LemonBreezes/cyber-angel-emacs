;;; Email configuration

;; Always enable the essential email configuration, including maildirs and addresses.
(setq user-full-name "StrawberryTea"
      user-mail-address "look@strawberrytea.xyz"
      mail-host-address "strawberrytea.xyz"
      mail-source-directory "~/.mail/")
(make-directory "~/.mail/Fastmail/" t)
;; ... (Email configuration continues, including mu4e, smtpmail, notmuch, etc.) ...
(when cae-init-email-enabled-p
  ;; Additional email-related settings…
  )
