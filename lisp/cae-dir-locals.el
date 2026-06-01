;;; lisp/cae-dir-locals.el -*- lexical-binding: t; -*-

(defun cae--setup-doom-dir-locals ()
  "Setup directory locals for the 'doom class."
  (when (buffer-file-name)
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (not (cl-member (file-name-nondirectory (buffer-file-name))
                               `(,dir-locals-file "doctor.el" "packages.el")
                               :test #'string=))
               (not (equal (bound-and-true-p cae-packages-freeze-file) buffer-file-name))
               (not (string-match-p "/packages/" buffer-file-name))
               (not (string-match-p "/trash/" buffer-file-name))
               (not (string-match-p "/benchmarks/" buffer-file-name))
               (not (string-match-p "/vanilla-emacs-configs/" buffer-file-name))
               (not (string-match-p "/dir-local-files/" buffer-file-name))
               (bound-and-true-p cae-config-finished-loading))
      (add-hook 'write-file-functions 'eval-buffer 1 t))
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (not (file-equal-p (bound-and-true-p cae-packages-freeze-file) buffer-file-name))
               (require 'apheleia nil t))
      (apheleia-mode +1))
    (setq-local blamer--block-render-p t)
    (setq-local aidermacs-auto-commits t)
    (when (equal (buffer-file-name) (bound-and-true-p eshell-aliases-file))
      (add-hook 'after-save-hook #'eshell-read-aliases-list nil t))
    (when (and (eq system-type 'gnu/linux)
               (not (string-match-p cae-multi-secrets-dir
                                    (buffer-file-name)))
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
               (equal (expand-file-name (vc-git-root (buffer-file-name)))
                      (expand-file-name "~/")))
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
               (string-match-p (buffer-file-name)
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

(defun cae-stumpwm-reload-file ()
  "Reload the visited file into the running StumpWM.
This is the Common Lisp analogue of the `eval-buffer' hot-reload the
'doom class performs for my Emacs Lisp.

Prefer a live SLY connection: it evaluates over a socket inside the
image, so errors land in the sly-db debugger and compiler notes keep
their source locations.  Fall back to `stumpish' (an X-property round
trip through StumpWM's command interpreter), which always works while
StumpWM is running but flattens any result or error to one message line."
  (when buffer-file-name
    (let ((file (expand-file-name buffer-file-name)))
      (cond
       ((and (require 'sly nil t)
             (sly-connected-p))
        (sly-eval-async `(slynk:load-file ,file)
                        (lambda (result)
                          (message "StumpWM (SLY) reload: %s" result))))
       ((executable-find "stumpish")
        (with-temp-buffer
          (call-process "stumpish" nil t nil "eval" (format "(load %S)" file))
          (let ((out (string-trim (buffer-string))))
            (unless (string-empty-p out)
              (message "StumpWM reload: %s" out)))))))))

(defun cae--setup-stumpwm-dir-locals ()
  "Setup directory locals for the 'stumpwm class.
For the Common Lisp files in ~/.stumpwm.d this mirrors the 'doom class:
refuse to save when parens are unbalanced, hot-reload the running
StumpWM image after a successful save, and auto-format on save."
  (when (and (buffer-file-name)
             (derived-mode-p 'lisp-mode)
             (not (string= (file-name-nondirectory (buffer-file-name))
                           dir-locals-file)))
    ;; Abort the save on unbalanced parens — the CL analogue of the
    ;; `eval-buffer' guard the 'doom class puts on `write-file-functions'.
    (add-hook 'write-file-functions #'check-parens nil t)
    ;; Hot-reload into the live StumpWM image once the file hits disk.
    (add-hook 'after-save-hook #'cae-stumpwm-reload-file nil t)
    ;; Auto-format on save (apheleia maps `lisp-mode' -> `lisp-indent').
    (when (require 'apheleia nil t)
      (apheleia-mode +1))
    (setq-local blamer--block-render-p t)
    (setq-local aidermacs-auto-commits t)
    (when (and (eq system-type 'gnu/linux)
               (require 'git-auto-commit-mode nil t))
      (setq-local gac-automatically-add-new-files-p nil
                  gac-automatically-push-p t)
      (git-auto-commit-mode 1))))

;; Requires updating.
;;(defun cae--setup-polybar-dir-locals ()
;;  "Setup directory locals for the 'polybar class, inheriting from 'home."
;;  ;; First apply home directory locals
;;  (cae--setup-home-dir-locals)
;;  ;; Then add polybar-specific settings
;;  (when (and (buffer-file-name) 
;;             (fboundp #'cae-theme-export-using-pywal))
;;    (add-hook 'after-save-hook #'cae-theme-export-using-pywal nil t)))

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
 'stumpwm
 '((nil . ((eval . (cae--setup-stumpwm-dir-locals))))))

(when (eq system-type 'gnu/linux)
  (dir-locals-set-directory-class (expand-file-name "~/.stumpwm.d") 'stumpwm))

(dir-locals-set-class-variables
 'polybar
 '((nil . ((eval . (cae--setup-polybar-dir-locals))))))

(dir-locals-set-directory-class (concat (getenv "HOME") "/home/st/.config/polybar") 'polybar)


;;Local Variables:
;;eval: (cae-reload-all-dir-locals)
;;End:
