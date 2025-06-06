;;; lisp/cae-dir-locals.el -*- lexical-binding: t; -*-

(defun cae--setup-doom-dir-locals ()
  "Setup directory locals for the 'doom class."
  (when (buffer-file-name)
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (not (cl-member (file-name-nondirectory (buffer-file-name))
                               `(,dir-locals-file "doctor.el" "packages.el")
                               :test #'string=))
               (not (string-match-p "/packages/" buffer-file-name))
               (not (string-match-p "/trash/" buffer-file-name))
               (not (string-match-p "/benchmarks/" buffer-file-name))
               (not (string-match-p "/vanilla-emacs-configs/" buffer-file-name))
               (not (string-match-p "/dir-local-files/" buffer-file-name))
               (bound-and-true-p cae-config-finished-loading))
      (add-hook 'write-file-functions 'eval-buffer 1 t))
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (require 'apheleia nil t))
      (apheleia-mode +1))
    (setq-local blamer--block-render-p t)
    (setq-local aidermacs-auto-commits t)
    (when (and (bound-and-true-p eshell-aliases-file)
               (file-equal-p (buffer-file-name) eshell-aliases-file)
               (fboundp 'eshell-read-aliases-list))
      (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))
    (when (and (eq system-type 'gnu/linux)
               (bound-and-true-p doom-user-dir)
               (not (file-in-directory-p (buffer-file-name) (concat doom-user-dir "secrets/")))
               (require 'git-auto-commit-mode nil t))
      (setq-local gac-automatically-add-new-files-p nil
                  gac-automatically-push-p t)
      (git-auto-commit-mode 1))))

(defun cae--setup-home-dir-locals ()
  "Setup directory locals for the 'home class."
  (when (buffer-file-name)
    (setq-local blamer--block-render-p t)
    (when (and (require 'git-auto-commit-mode nil t)
               (require 'vc-git nil t)
               (file-equal-p (vc-git-root (buffer-file-name)) "~/"))
      (setq-local aidermacs-auto-commits t)
      (setq-local gac-automatically-add-new-files-p nil)
      (setq-local gac-automatically-push-p t)
      (git-auto-commit-mode 1))))

(defun cae--setup-org-dir-locals ()
  "Setup directory locals for the 'org class."
  (when (buffer-file-name)
    (setq-local blamer--block-render-p t)
    (when (and (require 'git-auto-commit-mode nil t)
               (require 'vc-git nil t)
               (file-in-directory-p (buffer-file-name)
                                    (expand-file-name cae-multi-org-dir)))
      (setq-local aidermacs-auto-commits t)
      (setq-local gac-automatically-add-new-files-p t)
      (setq-local gac-automatically-push-p t)
      (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h nil t)
      (git-auto-commit-mode 1))))

(defun cae--setup-secrets-dir-locals ()
  "Setup directory locals for the 'secrets class."
  (when (buffer-file-name)
    (setq-local blamer--block-render-p t)
    (when (and (require 'git-auto-commit-mode nil t)
               (require 'vc-git nil t))
      (setq-local aidermacs-auto-commits t)
      (setq-local gac-automatically-add-new-files-p nil)
      (setq-local gac-automatically-push-p t)
      (git-auto-commit-mode 1))))

(defun cae--setup-polybar-dir-locals ()
  "Setup directory locals for the 'polybar class, inheriting from 'home."
  ;; First apply home directory locals
  (cae--setup-home-dir-locals)
  ;; Then add polybar-specific settings
  (when (and (buffer-file-name) 
             (fboundp #'cae-theme-export-using-pywal))
    (add-hook 'after-save-hook #'cae-theme-export-using-pywal nil t)))

(dir-locals-set-class-variables
 'doom
 '((nil . ((eval . (cae--setup-doom-dir-locals))))
   (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/"))))))

(dir-locals-set-directory-class doom-user-dir 'doom)

;; Disable `diff-hl-mode' in my Doom private dir.
(defadvice! cae-hacks-disable-diff-hl-in-private-config-a (&optional arg)
  :before-until #'diff-hl-mode
  (file-in-directory-p default-directory doom-user-dir))

(dir-locals-set-class-variables
 'home
 '((nil . ((eval . (cae--setup-home-dir-locals))))))

(when (eq system-type 'gnu/linux)
  (dir-locals-set-directory-class (getenv "HOME") 'home))

(dir-locals-set-class-variables
 'org
 '((nil . ((eval . (cae--setup-org-dir-locals))))))

(dir-locals-set-directory-class (expand-file-name cae-multi-org-dir) 'org)

(dir-locals-set-class-variables
 'secrets
 '((nil . ((eval . (cae--setup-secrets-dir-locals))))))

(dir-locals-set-directory-class cae-multi-secrets-dir 'secrets)

(dir-locals-set-class-variables
 'polybar
 '((nil . ((eval . (cae--setup-polybar-dir-locals))))))

(dir-locals-set-directory-class (concat (getenv "HOME") "/home/st/.config/polybar") 'polybar)


;;Local Variables:
;;eval: (cae-reload-all-dir-locals)
;;End:
