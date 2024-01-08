;;; private/vc/config.el -*- lexical-binding: t; -*-

(setq +vc-gutter-in-remote-files t
      +vc-gutter-in-margin t)

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

(after! code-review
  (setq code-review-auth-login-marker 'forge))
(when (modulep! :tools magit)
  (add-hook 'magit-mode-hook #'cae-magit-add-PR-fetch-ref)
  (after! magit
    (when (modulep! :editor fold)
      (map! :map magit-status-mode-map
            [remap +fold/toggle] #'magit-section-toggle))
    (setq magit-diff-refine-hunk 'all
          ;;magit-revision-show-gravatars '("^Author:     " . "^Commit:     ") ;causes lag
          magit-repository-directories '(("~/src/" . 2))
          transient-values '((magit-rebase "--autosquash" "--autostash")
                             (magit-pull "--rebase" "--autostash")
                             (magit-revert "--autostash"))
          magit-log-auto-more t
          magit-pull-or-fetch t)
    (when (modulep! :tools magit +forge)
      (advice-remove #'forge-get-repository #'+magit--forge-get-repository-lazily-a)
      (advice-remove #'forge-dispatch #'+magit--forge-build-binary-lazily-a)
      (after! forge
        (setq forge-pull-notifications t
              forge-buffer-draft-p t)))))

(when (modulep! :ui vc-gutter +diff-hl)
  (after! diff-hl
    (setq diff-hl-reference-revision "master")
    (unless (cae-display-graphic-p)
      (diff-hl-margin-mode +1)))

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

(use-package! abridge-diff
  :after magit :config (abridge-diff-mode +1))

(use-package! magit-stats
  :defer t)

(use-package! embark-vc
  :after embark)

(use-package! blamer
  :defer t :init
  (map! :leader :desc "Blamer" "tB" #'blamer-mode)
  :config
  (setq blamer-idle-time 0.3
        blamer-avatar-folder (concat doom-cache-dir "avatars/"))
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s"))

(use-package! consult-gh
  :defer t :when (modulep! :completion vertico)
  :commands (consult-gh-search-repos
             consult-gh-search-code
             consult-gh-search-prs
             consult-gh-search-issues
             consult-gh-pr-list
             consult-gh-issue-list
             consult-gh-default-repos
             consult-gh-find-file
             consult-gh-repo-clone
             consult-gh-repo-fork)
  :init
  (setq consult-gh-prioritize-local-folder 'suggest
        consult-gh-confirm-before-clone nil)
  (let ((vc-prefix (if (modulep! :editor evil) "g" "v")))
    (map! :leader
          :prefix vc-prefix
          (:prefix-map ("h" . "GitHub")
           :desc "Search repos" "r" #'consult-gh-search-repos
           :desc "Search code" "s" #'consult-gh-search-code
           :desc "Search PRs" "p" #'consult-gh-search-prs
           :desc "Search issues" "i" #'consult-gh-search-issues
           :desc "List PRs" "P" #'consult-gh-pr-list
           :desc "List issues" "I" #'consult-gh-issue-list
           :desc "Default repos" "d" #'consult-gh-default-repos
           :desc "Find file" "f" #'consult-gh-find-file
           :desc "Clone" "c" #'consult-gh-repo-clone
           :desc "Fork" "k" #'consult-gh-repo-fork)))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-external-apps-map
      "g" "GitHub CLI"
      "go" "Organizations"
      "gc" "Clone repo"
      "gs" "Search repos"
      "gi" "Search issues"
      "gf" "Find file"
      "gk" "Fork repo"))
  :config
  (setq consult-gh-default-clone-directory "~/src/"
        consult-gh-show-preview t
        consult-gh-issue-action #'consult-gh--issue-browse-url-action
        consult-gh-repo-action #'consult-gh--repo-browse-files-action
        consult-gh-file-action #'consult-gh--files-view-action
        consult-gh-preview-buffer-mode #'org-mode
        consult-gh-default-orgs-list '("oantolin" "minad" "alphapapa"
                                       "LemonBreezes" "protesilaos"
                                       "emacs-mirror" "doomemacs" "tecosaur"
                                       "systemcrafters"))
  (after! projectile
    (add-hook! 'consult-gh-repo-post-clone-hook
      (defun cae-projectile-discover-projects-in-search-path-h (&rest _)
        (projectile-discover-projects-in-search-path)))))
(use-package! consult-gh-embark
  :after (consult-gh embark))

(use-package! igist :defer t)           ; TODO

;; I have this disabled because it's not legible when I have my laser protection
;; goggles on.
(use-package! diff-ansi
  :when (modulep! +delta)
  :after magit-diff
  :config
  (setq diff-ansi-extra-args-for-delta
        '("--no-gitconfig" "--true-color=always" "--paging=never"))
  (diff-ansi-mode +1))
