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
        ,@(when (file-exists-p "~/src/haskell/") '(("~/src/haskell" . 1)))
        ,@(when (file-exists-p "~/Documents/") '(("~/Documents/" . 1)))))
;; This is a performance optimization. I am okay with not tracking projects
;; automatically and using the list above instead.
(setq projectile-track-known-projects-automatically nil)

;; Ensure projects list is up-to-date since I usually switch to a project
;; after running a `git clone'.
(defadvice! cae-projectile-update-projects-list (&optional _)
  :before #'projectile-switch-project
  (projectile-discover-projects-in-search-path))

;; Work around a bug with `projectile-skel-dir-locals' that is not in Doom Emacs.
;; https://discord.com/channels/406534637242810369/406554085794381833/1025743716662661170
(defadvice! fixed-projectile-skel-dir-locals (&optional str arg)
  :override #'projectile-skel-dir-locals
  (interactive "*P\nP")
  (skeleton-proxy-new
   '(nil "((nil . (" ("" '(projectile-skel-variable-cons) n)
     resume: ")))")
   str arg))

(if (locate-library "projectile")
    ;;; Projectile configuration
    (after! projectile
      (run-with-idle-timer 1.0 nil #'projectile--cleanup-known-projects)
      
      ;; Projectile's caching of commands is really annoying because if I change
      ;; the commands, the cache doesn't get invalidated automatically.
      (setq projectile-project-enable-cmd-caching nil)

      ;; Stop prompting me about the project root.
      (setq projectile-require-project-root t)

      ;; Don't crash my Emacs when running `projectile-project-files' on HOME.
      (defadvice! cae-projectile-project-files-a (project-root)
        :before-until #'projectile-project-files
        (when (or (file-equal-p project-root "~")
                  (file-equal-p project-root "/root"))
          (user-error "Running `projectile-project-files' on HOME is disabled.")))
      (add-to-list 'projectile-globally-ignored-directories "^/home/st/?$" nil #'equal)
      (defadvice! cae-projectile-root-bottom-up-a (dir)
        :filter-return #'projectile-root-bottom-up
        (unless (and dir (or (file-equal-p dir "~")
                             (file-equal-p dir "/root")))
          dir))

      (map! :leader :prefix "p"
            :desc "Dired in project root"  "-" #'projectile-dired)

      (add-to-list 'projectile-globally-ignored-directories
                   (expand-file-name ".local/straight/repos/" user-emacs-directory))
      (unless (or (cl-set-difference projectile-known-projects
                                     '("~/.doom.d/" "~/.emacs.d/" "~/.config/doom/"
                                       "~/.config/emacs/")
                                     :test #'string=)
                  (not (file-directory-p "~/src/"))
                  (directory-empty-p "~/src/"))
        (projectile-discover-projects-in-search-path))
      ;; Recognize `makefile' as a Makefile.
      (add-to-list
       'projectile-project-types
       '(make marker-files
         ("makefile")
         project-file "Makefile" compilation-dir nil configure-command nil
         compile-command "make" test-command "make test"
         install-command "make install" package-command nil run-command nil))
      (add-to-list
       'projectile-project-types
       '(gnumake marker-files
         ("GNUmakefile")
         project-file "GNUMakefile" compilation-dir nil configure-command nil
         compile-command "make" test-command "make test" install-command
         "make install" package-command nil run-command nil))
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
