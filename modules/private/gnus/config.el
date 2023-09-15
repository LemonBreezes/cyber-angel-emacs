;;; private/gnus/config.el -*- lexical-binding: t; -*-

(unless (or (modulep! :email mu4e)
            (modulep! :email notmuch))
  (map! :leader :desc "Gnus" "o m" #'=gnus))

(when (modulep! :editor evil)
  (after! evil-snipe
    (dolist (mode '(gnus-group-mode gnus-summary-mode gnus-article-mode gnus-server-mode))
      (cl-pushnew mode evil-snipe-disabled-modes))))
(use-package! gnus
  :commands gnus-unplugged
  :config
  (gnus-registry-initialize)
  (setq nnmail-split-methods 'nnmail-split-fancy)
  (setq nnimap-split-methods 'nnmail-split-fancy)
  (setq nnmail-split-fancy
        `(|
          (: spam-split)
          (from ,user-mail-address "Sent")
          "INBOX" ;; or "mail.misc" for nnml/POP3
          ))

  (setq!
   message-subscribed-address-functions '(gnus-find-subscribed-addresses)
   gnus-group-use-permanent-levels t
   gnus-agent-queue-mail nil
   gnus-agent-expire-days 90
   gnus-always-read-dribble-file t
   gnus-use-cache t
   gnus-use-scoring nil
   gnus-suppress-duplicates t
   gnus-novice-user t
   gnus-expert-user nil
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t
   gnus-select-method '(nnnil "")
   gnus-secondary-select-methods '((nntp "Gmane"
                                    (nntp-address "news.gmane.io"))
                                   (nntp "Eternal September"
                                         (nntp-address "news.eternal-september.org")
                                         (nntp-authinfo-user "StrawberryTea"))
                                   (nnhackernews "")
                                   ;;(nnimap "Fastmail"
                                   ;;        (nnimap-inbox "INBOX")
                                   ;;        (nnimap-address "imap.fastmail.com")
                                   ;;        (nnimap-server-port 993)
                                   ;;        (nnimap-stream ssl)
                                   ;;        (nnimap-streaming t)
                                   ;;        (nnimap-expunge never)
                                   ;;        (nnimap-split-methods default))
                                   (nnmaildir "Fastmail"
                                              (directory "~/Mail/Fastmail")))
   gnus-registry-ignored-groups '(("nntp" t) ("^INBOX" t))
   gnus-signature-separator '("^-- $" "^-- *$" "^_____+$")
   gnus-uncacheable-groups "^nnml"
   gnus-large-newsgroup 200
   network-security-level 'low
   gnus-permanently-visible-groups "INBOX"
;;; Startup functions
   gnus-save-killed-list nil
   gnus-check-new-newsgroups nil
   ;; No other newsreader is used.
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
   ;; Emacs 28 introduces a unified query lang
   gnus-search-use-parsed-queries t
;;; Article mode for Gnus
   gnus-visible-headers (rx line-start (or "From"
                                           "Subject"
                                           "Mail-Followup-To"
                                           "Date"
                                           "To"
                                           "Cc"
                                           "Newsgroups"
                                           "User-Agent"
                                           "X-Mailer"
                                           "X-Newsreader")
                            ":")
   gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date))
   gnus-article-browse-delete-temp t
   gnus-article-show-cursor t
   ;; Display more MINE stuff
   gnus-mime-display-multipart-related-as-mixed t
;;; Asynchronous support for Gnus
   gnus-asynchronous t
   gnus-use-header-prefetch t
   gnus-use-article-prefetch t
;;; Cache interface for Gnus
   gnus-cache-enter-articles '(ticked dormant unread)
   gnus-cache-remove-articles '(read)
   gnus-cacheable-groups "^\\(nntp\\|\\)"))

;; Group mode commands for Gnus
(use-package! gnus-group
  :defer t :init
  (autoload 'gnus-topic-mode "gnus-topic")
  :hook
  (gnus-group-mode . gnus-topic-mode)
  :config
  ;;          indentation ------------.
  ;;  #      process mark ----------. |
  ;;                level --------. | |
  ;;           subscribed ------. | | |
  ;;  %          new mail ----. | | | |
  ;;  *   marked articles --. | | | | |
  ;;                        | | | | | |  Ticked    New     Unread  open-status Group
  (setq
   gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n"
   gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet)

   gnus-topic-alist '(("emms" "nntp+Gmane:gmane.emacs.emms.patches"
                       "nntp+Gmane:gmane.emacs.emms.user")
                      ("gnus" "nntp+Gmane:gmane.emacs.gnus.user"
                       "nntp+Gmane:gmane.emacs.gnus.patches" "nntp+Gmane:gmane.emacs.gnus.announce"
                       "nntp+Gmane:gmane.emacs.gnus.general")
                      ("orgmode" "nntp+Gmane:gmane.emacs.orgmode") ("misc")
                      ("fastmail" "nnimap+fastmail:INBOX" "nnimap+fastmail:Sent") ("hackernews" "nnhackernews:news")
                      ("emacs" "nntp+Gmane:gmane.linux.gentoo.announce" "nntp+Gmane:gmane.emacs.help"
                       "nntp+Gmane:gmane.emacs.diffs" "nntp+Gmane:gmane.emacs.bugs"
                       "nntp+Gmane:gmane.emacs.devel" "nntp+Gmane:gmane.emacs.announce")
                      ("Root" "nndraft:drafts" "nnhackernews:ask" "nnhackernews:show"
                       "nnhackernews:job"))
   gnus-topic-topology '(("Root" visible) (("misc" visible))
                         (("emacs" visible nil nil) (("orgmode" visible nil nil))
                          (("gnus" visible nil nil)) (("emms" invisible nil nil)))
                         (("fastmail" visible nil nil)) (("hackernews" visible))))
  (map! :map gnus-group-mode-map
        "<f6>" #'cae-gnus-group-cheatsheet/body
        :n "Tm" #'gnus-topic-move-group))

;; Summary mode commands for Gnus
(use-package! gnus-sum
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :config
  (unless (cae-tty-disable-unicode-p)
    ;; Pretty marks
    (setq! gnus-sum-thread-tree-false-root nil
           gnus-sum-thread-tree-single-indent nil
           gnus-sum-thread-tree-root nil
           gnus-sum-thread-tree-vertical "│ "
           gnus-sum-thread-tree-leaf-with-other "├── "
           gnus-sum-thread-tree-single-leaf "└── "
           gnus-sum-thread-tree-indent " "
           gnus-sum-thread-tree-single-indent nil))
  (setq! gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n"
         ;; Loose threads
         gnus-summary-make-false-root 'adopt
         gnus-simplify-subject-functions '(gnus-simplify-subject-fuzzy)
         gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
         ;; Filling in threads
         gnus-build-sparse-threads 'some
         gnus-fetch-old-headers 'some
         ;; More threading
         gnus-show-threads t
         gnus-thread-indent-level 2
         gnus-thread-hide-subtree t
         ;; Sorting
         gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
         gnus-subthread-sort-functions '(gnus-thread-sort-by-date)
         ;; Viewing
         gnus-view-pseudos 'automatic
         gnus-view-pseudos-separately t
         gnus-view-pseudo-asynchronously t
         ;; No auto select
         gnus-auto-select-first nil
         gnus-auto-select-next nil
         gnus-paging-select-next nil)
  (map! :map gnus-summary-mode-map
        "<f6>" #'cae-gnus-summary-cheatsheet/body))

(use-package! gnus-art
  :defer t :config
  (setq! gnus-default-article-saver 'gnus-summary-save-in-mail
         gnus-treat-hide-citation-maybe t
         gnus-treat-strip-cr t
         gnus-treat-strip-leading-blank-lines t
         gnus-treat-strip-multiple-blank-lines t
         gnus-treat-strip-trailing-blank-lines t
         gnus-treat-unsplit-urls t
         gnus-ignored-mime-types
         '("application/x-pkcs7-signature"
           "application/ms-tnef"
           "text/x-vcard"))
  (map! :map gnus-article-mode-map
        "<f6>" #'cae-gnus-article-cheatsheet/body))

(use-package! gnus-dired
  :hook (dired-mode . gnus-dired-mode))

(use-package! spam
  :after gnus)

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

(use-package! nnhackernews
  :defer t :config
  (setq nnhackernews-render-story t))

(use-package! gnus-srvr
  :defer t :config
  (map! [remap gnus-browse-unsubscribe-current-group] #'gnus-browse-toggle-subscription-at-point))

(use-package! nnmairix
  :defer t :config
  (setq nnmairix-allowfast-default t))
