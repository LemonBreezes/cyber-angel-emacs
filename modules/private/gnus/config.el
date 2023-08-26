;;; private/gnus/config.el -*- lexical-binding: t; -*-

(map! :leader :desc "Gnus" "og" #'=gnus)

(use-package! gnus
  :commands gnus gnus-unplugged gnus-agent-batch
  :custom
  (mail-user-agent 'gnus-user-agent)
  (gnus-use-cache t)
  (gnus-use-scoring nil)
  (gnus-suppress-duplicates t)
  (gnus-novice-user t)
  (gnus-expert-user t)
  (gnus-interactive-exit 'quiet)
  (gnus-inhibit-startup-message t)
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods '((nntp "news.gmane.io")
                                   (nntp "news.eternal-september.org")
                                   (nnimap "fastmail"
                                           (nnimap-inbox "INBOX")
                                           (nnimap-address "imap.fastmail.com")
                                           (nnimap-server-port 993)
                                           (nnimap-stream ssl)
                                           (nnimap-expunge 'never)
                                           (nnimap-split-fancy '(| (any "emacs-devel" "emacs-devel")
                                                                 "INBOX")))))
  (gnus-large-newsgroup 4000)
  (network-security-level 'low)
;;; Startup functions
  (gnus-save-killed-list nil)
  (gnus-check-new-newsgroups nil)
  ;; No other newsreader is used.
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)
  ;; Emacs 28 introduces a unified query lang
  (gnus-search-use-parsed-queries t)
;;; Article mode for Gnus
  (gnus-visible-headers (rx line-start (or "From"
                                           "Subject"
                                           "Mail-Followup-To"
                                           "Date"
                                           "To"
                                           "Cc"
                                           "Newsgroups"
                                           "User-Agent"
                                           "X-Mailer"
                                           "X-Newsreader")
                            ":"))
  (gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date)))
  (gnus-article-browse-delete-temp t)
  ;; Display more MINE stuff
  (gnus-mime-display-multipart-related-as-mixed t)
;;; Asynchronous support for Gnus
  (gnus-asynchronous t)
  (gnus-use-header-prefetch t)
;;; Cache interface for Gnus
  (gnus-cache-enter-articles '(ticked dormant unread))
  (gnus-cache-remove-articles '(read))
  (gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"))

;; Group mode commands for Gnus
(use-package! gnus-group
  :hook (gnus-group-mode . gnus-topic-mode)
  :custom
  ;;          indentation ------------.
  ;;  #      process mark ----------. |
  ;;                level --------. | |
  ;;           subscribed ------. | | |
  ;;  %          new mail ----. | | | |
  ;;  *   marked articles --. | | | | |
  ;;                        | | | | | |  Ticked    New     Unread  open-status Group
  (gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n")
  (gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))
  :config
  (map! :map gnus-group-mode-map
        "<f6>" #'cae-gnus-group-cheatsheet/body))

;; Summary mode commands for Gnus
(use-package! gnus-sum
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :custom
  ;; Pretty marks
  (gnus-sum-thread-tree-root            "┌ ")
  (gnus-sum-thread-tree-false-root      "◌ ")
  (gnus-sum-thread-tree-single-indent   "◎ ")
  (gnus-sum-thread-tree-vertical        "│")

  (gnus-sum-thread-tree-leaf-with-other "├─►")
  (gnus-sum-thread-tree-single-leaf     "╰─►")
  (gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n")
  ;; Loose threads
  (gnus-summary-make-false-root 'adopt)
  (gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  ;; Filling in threads
  ;; 2 old articles are enough for memory
  (gnus-fetch-old-headers 2)
  (gnus-fetch-old-ephemeral-headers 2)
  (gnus-build-sparse-threads 'some)
  ;; More threading
  (gnus-show-threads t)
  (gnus-thread-indent-level 2)
  (gnus-thread-hide-subtree nil)
  ;; Sorting
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-subthread-sort-functions '(gnus-thread-sort-by-date))
  ;; Viewing
  (gnus-view-pseudos 'automatic)
  (gnus-view-pseudos-separately t)
  (gnus-view-pseudo-asynchronously t)
  ;; No auto select
  (gnus-auto-select-first nil)
  (gnus-auto-select-next nil)
  (gnus-paging-select-next nil)
  :config
  (map! :map gnus-summary-mode-map
        "<f6>" #'cae-gnus-summary-cheatsheet/body))

(use-package! gnus-art
  :defer t :config
  (map! :map gnus-article-mode-map
        "<f6>" #'cae-gnus-article-cheatsheet/body))

(use-package bbdb
  :defer t :config
  (require 'bbdb-com)
  (require 'bbdb-anniv)
  (require 'bbdb-gnus)
  (bbdb-initialize 'message 'gnus)
  (bbdb-mua-auto-update-init 'message 'gnus)
  (setq bbdb-mua-pop-up nil)
  (setq bbdb-allow-duplicates t)
  (setq bbdb-pop-up-window-size 5)
  (setq bbdb-ignore-redundant-mails t)
  (setq bbdb-update-records-p 'create)
  (setq bbdb-mua-update-interactive-p '(create . query))
  (setq bbdb-mua-auto-update-p 'create)
  (add-hook 'mail-setup-hook 'bbdb-mail-aliases)
  (add-hook 'message-setup-hook 'bbdb-mail-aliases)
  (add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
  ;; (add-hook 'list-diary-entries-hook 'bbdb-include-anniversaries)
  (setq bbdb-always-add-addresses t
        bbdb-complete-name-allow-cycling t
        bbdb-completion-display-record t
        bbdb-default-area-code nil
        bbdb-dwim-net-address-allow-redundancy t
        bbdb-electric-p nil
        bbdb-add-aka 'query
        bbdb-add-name 'query
        bbdb-add-mails t
        bbdb-new-nets-always-primary 'never
        bbdb-north-american-phone-numbers-p nil
        bbdb-offer-save 'auto
        bbdb-pop-up-target-lines 3
        bbdb-print-net 'primary
        bbdb-print-require t
        bbdb-use-pop-up nil
        bbdb-user-mail-names gnus-ignored-from-addresses
        bbdb/gnus-split-crosspost-default nil
        bbdb/gnus-split-default-group nil
        bbdb/gnus-split-myaddr-regexp gnus-ignored-from-addresses
        bbdb/gnus-split-nomatch-function nil
        bbdb/gnus-summary-known-poster-mark "+"
        bbdb/gnus-summary-mark-known-posters t
        bbdb-ignore-message-alist '(("Newsgroup" . ".*")))
  (defalias 'bbdb-y-or-n-p #'(lambda (prompt) t))
  (setq bbdb-auto-notes-alist
        '(("Newsgroups" ("[^,]+" newsgroups 0))
          ("Subject" (".*" last-subj 0 t))
          ("User-Agent" (".*" mailer 0))
          ("X-Mailer" (".*" mailer 0))
          ("Organization" (".*" organization 0))
          ("X-Newsreader" (".*" mailer 0))
          ("X-Face" (".+" face 0 'replace))
          ("Face" (".+" face 0 'replace)))))
