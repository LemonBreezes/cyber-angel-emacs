;;; lisp/cae-projectile.el -*- lexical-binding: t; -*-

(if (locate-library "projectile")
    (after! projectile
      ;; Work around a bug with `projectile-skel-dir-locals' that is not in Doom Emacs.
      ;; https://discord.com/channels/406534637242810369/406554085794381833/1025743716662661170
      (defadvice! fixed-projectile-skel-dir-locals (&optional str arg)
        :override #'projectile-skel-dir-locals
        (interactive "*P\nP")
        (skeleton-proxy-new
         '(nil "((nil . (" ("" '(projectile-skel-variable-cons) n)
           resume: ")))")
         str arg))

      ;; Automatically find projects in the I personally use.
      (setq projectile-project-search-path
            `((,doom-user-dir . 0)
              (,doom-emacs-dir . 0)
              ,@(when (file-exists-p "~/projects/") '(("~/projects/" . 1)))
              ("~/src/" . 1)))
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
  (defalias 'projectile-find-file #'project-find-file)
  (defalias 'projectile-project-root
    (lambda (&optional dir)
      (if-let ((proj (project-current nil dir)))
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
    (map! :map project-prefix-map
          "t" #'magit-todo-list
          "." #'+default/search-project-for-symbol-at-point
          "x" #'doom/open-project-scratch-buffer
          "X" #'doom/switch-to-project-scratch-buffer)
    (setq +workspaces-switch-project-function #'ignore
          project-switch-commands `((consult-fd "Find file" "f")
                                    (+default/search-project "Search regexp" "s")
                                    (project-find-dir "Find directory")
                                    (cae-unpackaged-magit-status "Magit status" "g")
                                    (project-eshell "Eshell")))
    (advice-add #'project-switch-project :before
                (cae-defun cae-project-switch-action-a (dir)
                  (+workspaces-switch-to-project-h dir)))))
