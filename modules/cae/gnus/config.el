;;; private/gnus/config.el -*- lexical-binding: t; -*-

(map! :leader :desc "Gnus" "o g" #'=gnus)

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
                                    (nntp-address "news.gmane.org"))
                                   (nntp "Eternal September"
                                         (nntp-address "news.eternal-september.org")
                                         (nntp-authinfo-user "StrawberryTea"))
                                   (nnhackernews ""))
   gnus-registry-ignored-groups '(("nntp" t) ("^INBOX" t))
   gnus-signature-separator '("^-- $" "^-- *$" "^_____+$")
   gnus-uncacheable-groups "^nnml"
   gnus-large-newsgroup 200
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
                      ("hackernews" "nnhackernews:news")
                      ("emacs" "nntp+Gmane:gmane.linux.gentoo.announce" "nntp+Gmane:gmane.emacs.help"
                       "nntp+Gmane:gmane.emacs.diffs" "nntp+Gmane:gmane.emacs.bugs"
                       "nntp+Gmane:gmane.emacs.devel" "nntp+Gmane:gmane.emacs.announce")
                      ("Root" "nndraft:drafts" "nnhackernews:ask" "nnhackernews:show"
                       "nnhackernews:job"))
   gnus-topic-topology '(("Root" visible) (("misc" visible))
                         (("emacs" visible nil nil) (("orgmode" visible nil nil))
                          (("gnus" visible nil nil)) (("emms" invisible nil nil)))
                         (("hackernews" visible))))
  (map! :map gnus-group-mode-map
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
         gnus-paging-select-next nil))

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
           "text/x-vcard")))

(use-package! gnus-dired
  :hook (dired-mode . gnus-dired-mode))

(use-package! spam
  :after gnus)

(use-package! nnhackernews
  :defer t :config
  (setq nnhackernews-render-story t))

(use-package! gnus-srvr
  :defer t :config
  (map! [remap gnus-browse-unsubscribe-current-group] #'gnus-browse-toggle-subscription-at-point))

(use-package! nnmairix
  :defer t :config
  (setq nnmairix-allowfast-default t))
