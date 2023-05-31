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

(after! magit
  (when (modulep! :editor fold)
    (map! :map magit-status-mode-map
          [remap +fold/toggle] #'magit-section-toggle))
  (setq magit-diff-refine-hunk 'all
        ;;magit-revision-show-gravatars '("^Author:     " . "^Commit:     ") ;causes lag
        magit-repository-directories '(("~/src/" . 2))
        transient-values '((magit-rebase "--autosquash" "--autostash")
                           (magit-pull "--rebase" "--autostash")
                           (magit-revert "--autostash"))))

(when (modulep! :ui vc-gutter +diff-hl)
  (after! diff-hl
    (setq diff-hl-reference-revision "master"))

  ;; For the built-in repeat map.
  (map! [remap +vc-gutter/previous-hunk] #'diff-hl-show-hunk-previous
        [remap +vc-gutter/next-hunk]     #'diff-hl-show-hunk-next))

(use-package! git-link
  :defer t
  :init
  (after! magit
    (map! :map magit-mode-map
          :n "yc" #'git-link-commit))
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
  :defer t)

(use-package! git-email
  :defer t
  :init
  (after! mu4e
    (require 'git-email-mu4e)
    (git-email-mu4e-mode +1))
  (setq git-email-subject-regexp
        "^Subject:[[:space:]]*\\[[^]\n]*PATCH[^]\n]*][[:space:]]+.+\\(?:\\(?:$\\)[\s\t]\\{2\\}[^	\n$]+$\\|$\\)")
  (let ((vc-prefix (if (modulep! :editor evil) "g" "v")))
    (map! :leader
          :prefix vc-prefix
          "RET" #'git-email-format-patch))
  (map! :map dired-mode-map
        :localleader
        "g" #'git-email-send-email))

;; `vc-backup' refuses to build on Doom Emacs so I had to fork it and remove the
;; autoload line it uses.
(add-to-list 'vc-handled-backends 'Backup t)
(use-package! vc-backup
  :defer t
  :init
  (setq make-backup-files t
        vc-make-backup-files t))

(use-package! magit-stats
  :defer t)

(use-package! embark-vc
  :after embark)

(use-package! debbugs
  :defer t)
