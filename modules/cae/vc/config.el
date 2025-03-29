;;; cae/vc/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(setq +vc-gutter-in-remote-files t
      +vc-gutter-in-margin t)

;; Got these from Prot's config. Say, how do I even use `add-log' and `vc'?
(after! add-log
  (setq add-log-keep-changes-together t))
(after! vc-git
  (setq
   vc-git-diff-switches '("--patch-with-stat" "--histogram"
                          "--ignore-cr-at-eol" "--ignore-space-change"
                          "--ignore-all-space" "--ignore-blank-lines")
   vc-git-print-log-follow t))
(after! vc-annotate
  (setq vc-annotate-background-mode nil))

(after! code-review
  (setq code-review-auth-login-marker 'forge))
(when (modulep! :tools magit)
  ;; Back up uncommitted changes.
  ;; EDIT: This causes the index of projects to become locked. I am no longer
  ;; using this.
  ;;(add-hook 'doom-first-file-hook #'magit-wip-mode)

  ;; Set `forge.graphqlItemLimit' to 20 locally in some repositories where
  ;; `forge-pull' will hit the rate limits.

  (add-hook 'magit-mode-hook #'cae-magit-add-PR-fetch-ref)
  (after! ghub-graphql
    ;; This works around an issue where killing and recreating the status buffer
    ;; prevents progress updates from being relayed.
    (setq ghub-graphql-message-progress t))
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
      (after! consult
        (add-to-list 'consult-buffer-filter "\\`magit\\(?:-[a-z]+\\)?:")
        (add-to-list 'consult-buffer-filter "^\\*forge\\(?:-[a-z]+\\)"))
      (after! forge
        (setq forge-pull-notifications t
              forge-buffer-draft-p t))
      (after! forge-list
        (map! :map forge-topic-list-mode-map
              :n "@" #'forge-topics-menu))))
  (add-hook 'magit-status-mode-hook
            #'cae-magit-status-setup-upstream-diff-section-h))

(when (modulep! :ui vc-gutter +diff-hl)
  (after! diff-hl
    (setq diff-hl-reference-revision "master")
    (setq diff-hl-flydiff-delay 2)
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
  (map! :leader :desc ".gitignore template" "gI" #'gitignore-templates-insert))

(use-package! vc-backup
  :defer t :init
  (setq make-backup-files t
        vc-make-backup-files t))

(use-package! embark-vc
  :when (modulep! :tools magit +forge)
  :after (embark forge))

(use-package! consult-gh
  :defer t :when (modulep! :completion vertico) :init
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
           :desc "Fork" "k" #'consult-gh-fork-current-repo)))
  (defvar-keymap doom-leader-GitHub-map) ; Silence byte-compiler.
  (after! which-key
    (which-key-add-keymap-based-replacements doom-leader-GitHub-map
      "g" "GitHub CLI"
      "go" "Organizations"
      "gc" "Clone repo"
      "gs" "Search repos"
      "gi" "Search issues"
      "gf" "Find file"
      "gk" "Fork repo"))
  (after! (:all which-key consult-gh-embark)
    (which-key-add-keymap-based-replacements consult-gh-embark-general-actions-map
      "r"  "repo"
      "l"  "link"
      "b"  "add/remove"
      "bo" "org"
      "br" "repo"
      "C"  "consult"
      "f"  "file"))
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
               #'projectile-discover-projects-in-directory)))
(use-package! consult-gh-embark
  :after (consult-gh embark))

(use-package! diff-ansi
  :when (modulep! +delta)
  :after magit-diff :config
  (setq diff-ansi-extra-args-for-delta
        '("--no-gitconfig" "--true-color=always" "--paging=never"))
  (diff-ansi-mode +1))

(use-package! difftastic
  :defer t :init
  (after! evil
    (evil-set-initial-state 'difftastic-mode 'emacs))
  (after! magit-diff
    (unless (ignore-errors (transient-get-suffix 'magit-diff "D"))
      (transient-append-suffix 'magit-diff '(-1 -1)
        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
         ("S" "Difftastic show" difftastic-magit-show)])))
  (after! magit-blame
    (map! :map magit-blame-read-only-mode-map
          "D" #'difftastic-magit-show
          "S" #'difftastic-magit-show))
  (defun cae-difftastic--requested-window-width-single-window ()
    (- (frame-width) (fringe-columns 'left) (fringe-columns 'right)))
  :config
  (setq difftastic-requested-window-width-function
        #'cae-difftastic--requested-window-width-single-window
        difftastic-display-buffer-function
        (lambda (buf _)
          (display-buffer-same-window buf nil))))
