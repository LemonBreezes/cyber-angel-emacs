;;; ~/.doom.d/lisp/cae-bookmark.el -*- lexical-binding: t; -*-

;; This theming of the bookmark bmenu is from Prot's Emacs config.

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

(font-lock-flush (point-min) (point-max))
(font-lock-add-keywords nil cae-bookmark-keywords nil)
(add-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords)
