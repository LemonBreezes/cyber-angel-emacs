;;; lisp/cae-projectile.el -*- lexical-binding: t; -*-

;; Automatically find projects in the I personally use.
(setq projectile-project-search-path
      `((,doom-user-dir . 0)
        (,doom-emacs-dir . 0)
        ,@(when (file-exists-p "~/Sync/") '(("~/Sync/" . 0)))
        ,@(when (file-exists-p "~/org/") '(("~/org/" . 0)))
        ,@(when (file-exists-p "~/projects/") '(("~/projects/" . 1)))
        ,@(when (file-exists-p "~/src/") '(("~/src/" . 1)))
        ,@(when (file-exists-p "~/src/love2d/") '(("~/src/love2d" . 1)))
        ,@(when (file-exists-p "~/src/ai/") '(("~/src/ai" . 1)))
        ,@(when (file-exists-p "~/src/trading") '(("~/src/trading" . 1)))
        ,@(when (file-exists-p "~/src/haskell/") '(("~/src/haskell" . 1)))
        ,@(when (file-exists-p "~/Documents/") '(("~/Documents/" . 1)))))
;; This is a performance optimization. I am okay with not tracking projects
;; automatically and using the list above instead.
(setq projectile-track-known-projects-automatically nil)

(defun cae-projectile-discover-projects-in-search-path ()
  (quiet!! (projectile-discover-projects-in-search-path)))
(cae-run-with-idle-timer 5 5 "projectile-discover-projects"
                         #'cae-projectile-discover-projects-in-search-path)

;; Increase the cache limit to because of monolithic company repositories.
(setq doom-projectile-cache-limit 30000)

;; Work around a bug with `projectile-skel-dir-locals' that is not in Doom Emacs.
;; https://discord.com/channels/406534637242810369/406554085794381833/1025743716662661170
(defadvice! fixed-projectile-skel-dir-locals (&optional str arg)
  :override #'projectile-skel-dir-locals
  (interactive "*P\nP")
  (skeleton-proxy-new
   '(nil "((nil . (" ("" '(projectile-skel-variable-cons) n)
     resume: ")))")
   str arg))

;; BUG Disable `projectile-root-local' because sometimes it is being used to
;; incorrectly set the project root somehow and then setting that incorrect
;; value in the cache.
(setq projectile-project-root-functions
      '(;;projectile-root-local
        projectile-root-marked
        projectile-root-bottom-up
        projectile-root-top-down
        projectile-root-top-down-recurring))

(if (locate-library "projectile")
;;; Projectile configuration
    (after! projectile
      (run-with-idle-timer 1.0 nil #'projectile--cleanup-known-projects)

      ;; Stop prompting me about the project root.
      (setq projectile-require-project-root t)

      (map! :leader :prefix "p"
            :desc "Dired in project root"  "-" #'projectile-dired)

      (when projectile-track-known-projects-automatically
        (add-to-list 'projectile-globally-ignored-directories
                     (expand-file-name ".local/straight/repos/" user-emacs-directory)))

      ;; Recognize `makefile' as a Makefile.
      (add-to-list
       'projectile-project-types
       '(make marker-files
         ("makefile")
         project-file "Makefile" compilation-dir nil configure-command nil
         compile-command "make" test-command "make test"
         install-command "make install" package-command nil run-command nil))
      ;; Also recognize Truffle projects.
      (add-to-list
       'projectile-project-types
       '(evm-truffle marker-files
         ("truffle-config.js")
         compile-command "truffle compile" test-command "truffle test" install-command
         "truffle migrate" package-command nil run-command nil))
      (add-to-list 'projectile-project-root-files-top-down-recurring
                   "truffle-config.js")
      (add-to-list 'projectile-globally-ignored-directories "^.ccls-cache$")
      (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
      (add-to-list 'projectile-project-root-files-top-down-recurring
                   "compile_commands.json")
      ;; Set up compilation.
      (setq projectile-per-project-compilation-buffer t
            compilation-read-command nil)
      ;; Make the project prefix more readable.
      (after! which-key
        (push '((nil . "projectile-\\(.*\\)") . (nil . "\\1"))
              which-key-replacement-alist)))

;;; Configure Doom Emacs with project.el instead of Projectile.
  (defalias 'projectile-find-file #'project-find-file)
  (defalias 'projectile-project-root
    (lambda (&optional dir)
      (if-let* ((proj (project-current nil dir)))
          (project-root proj)
        dir)))
  (defvar projectile-before-switch-project-hook nil)
  (defvar projectile-after-switch-project-hook nil)
  (defvar projectile-project-name-function
    (lambda (dir)
      (project-name (project-current nil dir))))
  (map! :leader
        (:when (modulep! :editor evil)
         "SPC" #'consult-fd
         "DEL" #'consult-fd)
        "p" project-prefix-map)
  (after! project
    (advice-add #'message :override #'ignore)
    (unwind-protect (progn (project-forget-projects-under doom-local-dir t)
                           (project-forget-projects-under temporary-file-directory t))
      (advice-remove #'message #'ignore))
    (map! :map project-prefix-map
          "t" #'magit-todo-list
          "." #'+default/search-project-for-symbol-at-point
          "x" #'doom/open-project-scratch-buffer
          "X" #'doom/switch-to-project-scratch-buffer
          "M-x" #'project-execute-extended-command
          "-" #'project-dired
          "D" #'project-forget-project)
    (setq project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))
    (setq +workspaces-switch-project-function #'ignore
          project-switch-commands `((consult-fd "Find file" "f")
                                    (+default/search-project "Search regexp" "s")
                                    (project-find-dir "Find directory")
                                    (cae-unpackaged-magit-status "Magit status" "g")
                                    (project-eshell "Eshell")))
    (defadvice! cae-project-switch-action-a (dir)
      :before #'projectile-switch-project
      (+workspaces-switch-to-project-h dir))))
