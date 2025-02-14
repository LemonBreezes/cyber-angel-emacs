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
  (use-package! consult-mu
    :when (and (modulep! :email mu4e)
               (modulep! :completion vertico))
    :commands consult-mu
    :defer t :custom
    (consult-mu-maxnum 200)
    (consult-mu-preview-key 'any)
    (consult-mu-mark-previewed-as-read nil)
    (consult-mu-mark-viewed-as-read t)
    (consult-mu-use-wide-reply t)
    ;; define a template for headers view in minibuffer. The example below
    ;; adjusts the width based on the width of the screen.
    (consult-mu-headers-template
     (lambda () (concat "%f"
                        (number-to-string (floor (* (frame-width) 0.15)))
                        "%s"
                        (number-to-string (floor (* (frame-width) 0.5)))
                        "%d13" "%g" "%x")))
    :init
    (map! [remap mu4e-search] #'consult-mu)
    :config
    ;;create a list of saved searches for quick access using
    ;;`history-next-element' with `M-n' in minibuffer. Note the "#" character
    ;;at the beginning of each query! Change these according to
    (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
    (setq consult-mu-saved-searches-async '("#flag:unread"))
    ;; require embark actions for marking, replying, forwarding, etc. directly
    ;; from minibuffer
    (require 'consult-mu-embark)
    ;; require extra module for composing (e.g. for interactive attachment) as
    ;; well as embark actions
    (require 'consult-mu-compose)
    (require 'consult-mu-compose-embark)
    ;; require extra module for searching contacts and runing embark actions
    ;; on contacts
    (require 'consult-mu-contacts)
    (require 'consult-mu-contacts-embark)
    (setq consult-mu-embark-attach-file-key "C-a")
    (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
    (setq consult-mu-contacts-ignore-case-fold-search t)
    (consult-mu-compose-embark-bind-attach-file-key)
    ;; choose if you want to use dired for attaching files (choice of 'always,
    ;; 'in-dired, or nil)
    (setq consult-mu-compose-use-dired-attachment 'in-dired)
    ;; `consult-mu-dynamic' is way too slow.
    (setq consult-mu-default-command #'consult-mu-async))

  (map! [remap compose-mail] #'+mu4e/compose))
(after! mu4e-vars
  (setq mu4e-modeline-support t
        mu4e-notification-support t))
