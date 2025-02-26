;;; ~/.doom.d/lisp/cae-multi.el -*- lexical-binding: t; -*-

;; This is code written for the purpose of using this Emacs configuration on
;; multiple machines.

;;;; Configuration Groups and Variables

(defgroup cae-multi nil
  "Multi-machine synchronization for Emacs configuration."
  :group 'convenience)

(defcustom cae-multi-local-dir (expand-file-name "shared-local/" cae-multi-secrets-dir)
  "Directory for shared local files across multiple machines."
  :type 'directory
  :group 'cae-multi)

(defcustom cae-multi-data-dir (expand-file-name "etc/" cae-multi-local-dir)
  "Directory for shared data files."
  :type 'directory
  :group 'cae-multi)

(defcustom cae-multi-cache-dir (expand-file-name "cache/" cae-multi-local-dir)
  "Directory for shared cache files."
  :type 'directory
  :group 'cae-multi)

(defcustom cae-multi-org-dir (expand-file-name "org/" cae-multi-local-dir)
  "Directory for shared org files."
  :type 'directory
  :group 'cae-multi)

(defcustom cae-multi-secrets-dir (expand-file-name "secrets/" cae-multi-local-dir)
  "Directory for shared secret files."
  :type 'directory
  :group 'cae-multi)

(defcustom cae-multi-repositories
  (list doom-user-dir
        cae-multi-org-dir
        cae-multi-secrets-dir
        (getenv "HOME"))
  "List of directories containing Git repositories to sync between machines."
  :type '(repeat directory)
  :group 'cae-multi)

(defcustom cae-multi-enable-auto-pull (eq system-type 'gnu/linux)
  "If non-nil, automatically pull repositories when idle."
  :type 'boolean
  :group 'cae-multi)

;;;; Abbrev Variables

(defvar cae-multi-abbrev--auto-commit-disabled nil
  "When non-nil, automatic saving of abbrev file is temporarily disabled.")

;;;; File Watching Utilities

(defun cae-multi--setup-file-watch (file-path callback watch-var-symbol)
  "Set up a file watch for FILE-PATH using CALLBACK.
WATCH-VAR-SYMBOL is the symbol of the variable to store the watch descriptor."
  (when (and file-path (file-exists-p file-path))
    (unless (symbol-value watch-var-symbol)
      (set watch-var-symbol
           (file-notify-add-watch
            file-path
            '(change)
            callback)))))

(defun cae-multi--remove-file-watch (watch-var-symbol message)
  "Remove file watch stored in WATCH-VAR-SYMBOL and display MESSAGE."
  (when (symbol-value watch-var-symbol)
    (file-notify-rm-watch (symbol-value watch-var-symbol))
    (set watch-var-symbol nil)
    (message message)))

;;;; Directory Setup

;; Create necessary directories
(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)
(make-directory cae-multi-org-dir t)

;;;; Package Configuration

;; Configure file locations for various packages
(after! abbrev
  (setq abbrev-file-name (concat cae-multi-data-dir "abbrev_defs")))
(after! bookmark
  (setq bookmark-default-file (concat cae-multi-secrets-dir "bookmarks")))
(after! bbdb
  (setq bbdb-file (concat cae-multi-secrets-dir "bbdb")))
(after! calendar
  (setq diary-file (concat cae-multi-secrets-dir "diary")))
(after! calc
  (setq calc-settings-file (concat cae-multi-data-dir "calc.el")))
(setq cape-dict-file (expand-file-name "en.dic" cae-multi-data-dir))
(after! eww
  (setq eww-bookmarks-directory (concat cae-multi-data-dir "eww-bookmarks/")
        eww-download-directory (expand-file-name "~/Downloads/")))
(after! ispell
  (setq ispell-complete-word-dict (concat cae-multi-data-dir "en.dic")
        ispell-personal-dictionary (concat cae-multi-secrets-dir "aspell.en.pws")))
(after! spell-fu
  (setq spell-fu-directory (concat cae-multi-data-dir "spell-fu/")))
(after! transient
  (setq transient-values-file (concat cae-multi-data-dir "transient/values.el")))

;;;; Git Auto Commit Mode

(use-package! git-auto-commit-mode
  :defer t :init
  (autoload 'gac--after-save "git-auto-commit-mode")
  :config
  (setq-default gac-automatically-add-new-files-p nil)
  (setq-hook! 'git-auto-commit-mode-hook
    backup-inhibited t))

;;;; Bookmark Handling

(defun cae-multi-bookmark-push-changes-a (&rest _)
  "Push changes to the bookmark file after it's saved."
  (when (and bookmark-default-file (file-exists-p bookmark-default-file))
    (cae-multi--push-changes bookmark-default-file " *cae-multi-bookmark-push-changes-a*")))

(defun cae-multi-org-archive-push-changes-h ()
  "Push changes to org files and their archives after archiving."
  (when buffer-file-name
    (gac--after-save (buffer-file-name))
    (dolist (file (org-all-archive-files))
      (when (file-exists-p file)
        (gac--after-save file)))))

;; Configure bookmark saving
(setq bookmark-save-flag 1)
(setq bookmark-watch-bookmark-file 'silent)

;; Set up advice and hooks
(advice-add #'bookmark-save :after #'cae-multi-bookmark-push-changes-a)
(after! org
  (add-hook 'org-archive-hook #'cae-multi-org-archive-push-changes-h))

;;;; Abbrev Handling

(defmacro with-abbrev-auto-save-disabled (&rest body)
  "Execute BODY with automatic saving of the abbrev file disabled."
  (declare (indent 0))
  `(let ((cae-multi-abbrev--auto-commit-disabled t))
     ,@body))

(defun cae-multi--disable-auto-save-handler (orig-fun &rest args)
  "Run ORIG-FUN with automatic abbrev file saving disabled.
ARGS are passed on to ORIG-FUN. This prevents the advice on
`define-abbrev' from scheduling a save during bulk operations such as
reading the abbrev file or defining an abbrev table."
  (with-abbrev-auto-save-disabled
    (apply orig-fun args)))

(defun cae-multi-auto-save-abbrev (&rest _args)
  "Automatically schedule saving the abbrev file after a new abbrev is defined.
This function is meant to be used as :after advice on `define-abbrev'.
It does nothing if `cae-multi-abbrev--auto-commit-disabled' is non-nil."
  (unless cae-multi-abbrev--auto-commit-disabled
    (cae-multi--schedule-auto-save-abbrev))
  nil)

;;;###autoload
(defun cae-multi--track-abbrev-write (orig-fun &rest args)
  "Advice for `write-abbrev-file' to track when Emacs is writing the file.
Sets `cae-multi-abbrev--emacs-is-writing' to t during the write operation
and updates the stored modification time afterward."
  (let ((cae-multi-abbrev--emacs-is-writing t))
    (unwind-protect
        (condition-case err
            (prog1 (apply orig-fun args)
              ;; Update our stored mtime after writing
              (cae-multi--update-abbrev-mtime))
          (error
           (message "Error in write-abbrev-file: %s" (error-message-string err))
           (signal (car err) (cdr err))))
      (setq cae-multi-abbrev--emacs-is-writing nil))))

;; Initialize abbrev file tracking
(after! abbrev
  (setq cae-multi-abbrev--file-mtime 
        (and abbrev-file-name
             (file-exists-p abbrev-file-name)
             (nth 5 (file-attributes abbrev-file-name)))))

;; Set up advice for abbrev-related functions
(advice-add #'define-abbrev :after #'cae-multi-auto-save-abbrev)
(advice-add #'read-abbrev-file :around #'cae-multi--disable-auto-save-handler)
(advice-add #'define-abbrevs :around #'cae-multi--disable-auto-save-handler)
(advice-add #'write-abbrev-file :around #'cae-multi--track-abbrev-write)

;; Start watching abbrev file for changes on Linux systems
(when (eq system-type 'gnu/linux)
  (run-with-idle-timer 5 nil #'cae-multi-start-abbrev-watch))

;;;; Repository Synchronization

(defun cae-multi-sync-repositories-when-idle ()
  "Run repository sync when system has been idle for at least 10 seconds."
  (when (and (current-idle-time)
             (> (time-to-seconds (current-idle-time)) 10))
    (cae-multi-sync-repositories)))

(when cae-multi-enable-auto-pull
  (cae-run-with-timer 30 30 "cae-multi-sync-repositories"
                      #'cae-multi-sync-repositories-when-idle))
