;;; ~/.doom.d/lisp/cae-bookmark.el -*- lexical-binding: t; -*-

;;; This theming of the bookmark bmenu is from Prot's Emacs config.

(defvar cae-bookmark-common-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	 (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.")

(defconst cae-bookmark-keywords
  `((,(concat "\\(.*\\)" " " cae-bookmark-common-url-regexp)
     (1 '(bold cae-bookmark-url) t)
     (2 'cae-bookmark-url t))
    ("\\(.*\\)\\( [~/].*\\.pdf\\)"
     (1 '(bold cae-bookmark-pdf) t)
     (2 'cae-bookmark-pdf t))
    ("\\(.*\\)\\( [~/].*/$\\)"
     (1 '(bold cae-bookmark-directory) t)
     (2 'cae-bookmark-directory t))
    ("\\(.*org.*last-stored.*\\)"
     (1 'shadow t)))
  "Extra font-lock patterns for the Bookmark menu.")


(defgroup cae-bookmark ()
  "Bookmark extras for my dotemacs."
  :group 'matching)
(defface cae-bookmark-url
  '((((class color) (min-colors 88) (background light))
     :foreground "#0000c0")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00bcff")
    (t :foreground "blue"))
  "Face for URL bookmarks.")

(defface cae-bookmark-pdf
  '((((class color) (min-colors 88) (background light))
     :foreground "#7f1010")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffa0a0")
    (t :foreground "red"))
  "Face for PDF bookmarks.")

(defface cae-bookmark-directory
  '((((class color) (min-colors 88) (background light))
     :foreground "#0f3d8c")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0acef")
    (t :foreground "cyan"))
  "Face for directory bookmarks.")

(defconst cae-bookmark-keywords
  `((,(concat "\\(.*\\)" " " cae-bookmark-common-url-regexp)
     (1 '(boqld cae-bookmark-url) t)
     (2 'cae-bookmark-url t))
    ("\\(.*\\)\\( [~/].*\\.pdf\\)"
     (1 '(bold cae-bookmark-pdf) t)
     (2 'cae-bookmark-pdf t))
    ("\\(.*\\)\\( [~/].*/$\\)"
     (1 '(bold cae-bookmark-directory) t)
     (2 'cae-bookmark-directory t))
    ("\\\(.*-- Unknown location --\\)"
     (1 'shadow t))
    ("\\(.*org.*last-stored.*\\)"
     (1 'shadow t)))
  "Extra font-lock patterns for the Bookmark menu.")

;;;###autoload
(define-minor-mode cae-bookmark-extra-keywords
  "Apply extra font-lock rules to bookmark list buffers."
  :init-value nil
  :global t
  (if cae-bookmark-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil cae-bookmark-keywords nil)
        (add-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords))
    (font-lock-remove-keywords nil cae-bookmark-keywords)
    (remove-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

;;; Custom bookmark functions

(defvar cae-bookmark-downloads-directory (expand-file-name "~/Downloads/"))

(defun cae-directory-exists-and-not-empty-p (dir)
  "Return non-nil if DIR exists and is not empty."
  (and (file-exists-p dir)
       (not (directory-empty-p dir))))

(defun cae-newest-file-in-directories (dirs)
  "Return the most recently modified file in DIRS, excluding hidden files and directories."
  (let* ((files (cl-loop for dir in dirs
                         when (file-directory-p dir)
                         append (directory-files dir t "^[^.]")))
         (regular-files (cl-remove-if #'file-directory-p files)))
    (car (sort regular-files #'file-newer-than-file-p))))

;;;###autoload
(defun cae-bookmark-jump-to-newest-download (_)
  (let ((newest-file (cae-newest-file-in-directories
                      (list "~/Downloads/" "~/" "~/Sync/"))))
    (if newest-file
        (progn
          (dired (file-name-directory newest-file))
          (dired-goto-file newest-file))
      (dired "~/Downloads/"))))

;; (defun cae-bookmark-jump-to-project-bookmarks (_)
;;   ;; This function is broken. I am no longer using project bookmarks.
;;   (let* ((bookmark-file (cae-project--get-bookmark-file))
;;          (bookmark-dir (file-name-directory bookmark-file)))
;;     (if (file-exists-p bookmark-dir)
;;         (progn (dired bookmark-dir)
;;                (if (file-exists-p bookmark-file)
;;                    (dired-goto-file bookmark-file)
;;                  (kill-new bookmark-file)
;;                  (message "Bookmark file %s does not exist. It has been copied to the kill ring."
;;                           bookmark-file)))
;;       (message "No bookmark files found for this project."))))

(defun cae-get-windows-username ()
  "Extract the Windows username from cmd.exe output in WSL."
  (when (file-exists-p "/mnt/c/Windows/System32/cmd.exe")
    (string-trim
     (shell-command-to-string
      "/mnt/c/Windows/System32/cmd.exe /c echo %USERNAME% 2>/dev/null | tr -d '\n\r'"))))

;;;###autoload
(defun cae-bookmark-jump-to-syncthing-directory (_)
  (let ((username (cae-get-windows-username)))
    (cond
     ((cae-directory-exists-and-not-empty-p "/mnt/c/Users/SyncthingServiceAcct/Sync/")
      (dired "/mnt/c/Users/SyncthingServiceAcct/Sync/"))
     ((and username
           (cae-directory-exists-and-not-empty-p
            (format "/mnt/c/Users/%s/Sync/" username)))
      (dired (format "/mnt/c/Users/%s/Sync/" username)))
     ((cae-directory-exists-and-not-empty-p "~/Sync/")
      (dired "~/Sync/"))
     (t
      (message "No Syncthing directory found.")))))
