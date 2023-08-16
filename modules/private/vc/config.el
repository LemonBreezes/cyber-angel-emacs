;;; private/vc/config.el -*- lexical-binding: t; -*-

(when (modulep! :ui vc-gutter)
  (setq +vc-gutter-in-remote-files t
        +vc-gutter-in-margin t))

;; Got these from Prot's config. Say, how do I even use `add-log' and `vc'?
(after! add-log
  (setq add-log-keep-changes-together t))
(after! vc-git
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"
                               "--ignore-cr-at-eol" "--ignore-space-change"
                               "--ignore-all-space" "--ignore-blank-lines")
        vc-git-print-log-follow t))
(after! vc-annotate
  (setq vc-annotate-background-mode nil))

(when (modulep! :tools magit)
  (after! magit
    (when (modulep! :editor fold)
      (map! :map magit-status-mode-map
            [remap +fold/toggle] #'magit-section-toggle))
    (setq magit-diff-refine-hunk 'all
          ;;magit-revision-show-gravatars '("^Author:     " . "^Commit:     ") ;causes lag
          magit-repository-directories '(("~/src/" . 2))
          transient-values '((magit-rebase "--autosquash" "--autostash")
                             (magit-pull "--rebase" "--autostash")
                             (magit-revert "--autostash")))
    (when (modulep! :tools magit +forge)
      (map! :map magit-status-mode-map
            ;; Killing the Magit status buffer removes the `forge-pull' progress
            ;; from the modeline. One alternative is setting
            ;; `forge--mode-line-buffer' every time the new Magit buffer is
            ;; created in that repo.
            "q" #'magit-mode-bury-buffer)
      (after! forge
        (setq forge-pull-notifications t
              forge-buffer-draft-p t)
        (map! :map forge-post-mode-map
              "<f6>" #'cae-magit-forge-post-hydra/body
              :map forge-topic-mode-map
              "<f6>" #'cae-magit-forge-topic-hydra/body
              :map forge-pullreq-list-mode-map
              "<f6>" #'cae-magit-forge-pullreq-list-hydra/body
              :map forge-pullreq-list-mode-map
              "j" #'+default/search-buffer
              :map forge-topic-list-mode-map
              "j" #'+default/search-buffer
              :map forge-repository-list-mode-map
              "j" #'+default/search-buffer
              :map forge-notifications-mode-map
              "j" #'+default/search-buffer)))))

(when (modulep! :ui vc-gutter +diff-hl)
  (after! diff-hl
    (setq diff-hl-reference-revision "master"))

  ;; For the built-in repeat map.
  (map! [remap +vc-gutter/previous-hunk] #'diff-hl-show-hunk-previous
        [remap +vc-gutter/next-hunk]     #'diff-hl-show-hunk-next))

(use-package! git-link
  :defer t :init
  (when (modulep! :tools magit)
    (after! magit
      (map! :map magit-mode-map
            :n "yc" #'git-link-commit)))
  (after! file-info
    (let ((pos (cl-position-if
                (lambda (x) (equal (plist-get x :handler)
                                   '(file-info--get-headline "GIT")))
                file-info-handlers)))
      (dolist (info-handler '((:name "Git link"
                               :handler (cl-letf (((symbol-function #'git-link--new)
                                                   (symbol-function #'identity)))
                                          (call-interactively #'git-link))
                               :face font-lock-string-face
                               :bind "G")
                              (:name "Git link homepage"
                               :handler (unwind-protect
                                            (progn (advice-add #'git-link--new :override #'identity)
                                                   (call-interactively #'git-link-homepage))
                                          (advice-remove #'git-link--new #'identity))
                               :face font-lock-string-face
                               :bind "g")))
        (cl-pushnew info-handler (nthcdr (1+ pos) file-info-handlers)
                    :test (lambda (x y) (equal (plist-get x :name) (plist-get y :name))))))))

(use-package! gitignore-templates
  :defer t :init
  (map! :leader :desc ".gitignore template" "iI" #'gitignore-templates-insert))

(use-package! git-email
  :defer t :init
  (let ((vc-prefix (if (modulep! :editor evil) "g" "v")))
    (map! :leader
          :prefix vc-prefix
          :desc "Email patch" "RET" #'git-email-format-patch))
  (map! :map dired-mode-map
        :localleader
        "g" #'git-email-send-email)
  :config
  (cond ((modulep! :email mu4e)
         (require 'git-email-mu4e)
         (git-email-mu4e-mode +1))
        ((modulep! :email notmuch)
         (require 'git-email-notmuch)
         (git-email-notmuch-mode +1)))
  (setq git-email-subject-regexp
        "^Subject:[[:space:]]*\\[[^]\n]*PATCH[^]\n]*][[:space:]]+.+\\(?:\\(?:$\\)[\s\t]\\{2\\}[^	\n$]+$\\|$\\)"))

;; `vc-backup' refuses to build on Doom Emacs so I had to fork it and remove the
;; autoload line it uses.
(add-to-list 'vc-handled-backends 'Backup t)
(use-package! vc-backup
  :defer t :init
  (setq make-backup-files t
        vc-make-backup-files t))

(use-package! magit-stats
  :defer t)

(use-package! embark-vc
  :after embark)

(after! smerge-mode
  (map! :map smerge-mode-map
        "<f6>" #'+vc/smerge-hydra/body))

(use-package! blamer
  :defer t :init
  (map! :leader :desc "Blamer" "tB" #'blamer-mode)
  :config
  (setq blamer-idle-time 0.3
        blamer-avatar-folder (concat doom-cache-dir "avatars/"))
  (setq blamer-author-formatter "  ✎ %s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s"))

;; TODO `igist' and `consult-gh'.
(use-package! consult-gh
  :defer t :when (modulep! :completion vertico)
  :commands (consult-gh-orgs
             consult-gh-repo-clone
             consult-gh-search-repos
             consult-gh-search-issues)
  :config
  (setq consult-gh-default-clone-directory "~/src/"
        consult-gh-show-preview t
        consult-gh-issue-action #'consult-gh--issue-view-action
        consult-gh-repo-action #'consult-gh--repo-browse-files-action
        consult-gh-file-action #'consult-gh--files-view-action
        consult-gh-default-orgs-list '("oantolin" "minad" "alphapapa" "LemonBreezes" "protesilaos")))
(use-package! consult-gh-embark
  :after (consult-gh embark))

(use-package! igist :defer t)
