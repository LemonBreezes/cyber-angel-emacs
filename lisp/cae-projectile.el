;;; lisp/cae-projectile.el -*- lexical-binding: t; -*-

(after! projectile
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
     project-file "truffle-config.js" compilation-dir nil configure-command nil
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
