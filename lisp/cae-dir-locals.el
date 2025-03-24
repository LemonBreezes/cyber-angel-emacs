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
               (file-in-directory-p (buffer-file-name) (expand-file-name cae-multi-org-dir)))
      (setq-local aidermacs-auto-commits t)
      (setq-local gac-automatically-add-new-files-p t)
      (setq-local gac-automatically-push-p t)
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

(dir-locals-set-class-variables
 'doom
 '((nil . ((eval . (progn (cae--setup-doom-dir-locals)))))
   (magit-status-mode . ((magit-todos-exclude-globs . (".git/" "shared-local/"))))))

(dir-locals-set-directory-class doom-user-dir 'doom)

;; Disable `diff-hl-mode' in my Doom private dir.
(defadvice! cae-hacks-disable-diff-hl-in-private-config-a (&optional arg)
  :before-until #'diff-hl-mode
  (file-in-directory-p default-directory doom-user-dir))

(dir-locals-set-class-variables
 'home
 '((nil . ((eval . (progn (cae--setup-home-dir-locals)))))))

(when (eq system-type 'gnu/linux)
  (dir-locals-set-directory-class (getenv "HOME") 'home))

(dir-locals-set-class-variables
 'org
 '((nil . ((eval . (progn (cae--setup-org-dir-locals)))))))

(dir-locals-set-directory-class (expand-file-name cae-multi-org-dir) 'org)

(dir-locals-set-class-variables
 'secrets
 '((nil . ((eval . (progn (cae--setup-secrets-dir-locals)))))))

(dir-locals-set-directory-class cae-multi-secrets-dir 'secrets)

(defun cae-fix-seq-empty-p-eval (fn &rest args)
  "Fix for `seq-empty-p' when called with an `(eval)` form.
This advice wraps around `seq-empty-p` to handle the case where it's
called with an unevaluated `(eval)` form, which can happen during
directory local variable processing."
  (condition-case nil
      (apply fn args)
    (cl-no-applicable-method
     (let ((arg (car args)))
       (if (and (listp arg) (eq (car-safe arg) 'eval))
           ;; When given an (eval) form, evaluate it first
           (apply fn (list (eval (cadr arg) t)))
         ;; Re-signal the original error for other cases
         (signal 'cl-no-applicable-method (list 'seq-empty-p arg)))))))

(advice-add 'seq-empty-p :around #'cae-fix-seq-empty-p-eval)


;;Local Variables:
;;eval: (cae-reload-all-dir-locals)
;;End:
