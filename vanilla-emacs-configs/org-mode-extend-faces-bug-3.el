;;; vanilla-emacs-configs/org-mode-extend-faces-bug-3.el -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)
(require 'org)
(load-theme 'leuven t)
(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))
(setq org-fontify-whole-heading-line t)
(scratch-buffer)
(defun org-set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let ((org-font-lock-extra-keywords
     (list
      ;; Call the hook
      '(org-font-lock-hook)
      ;; Headlines
      `(,(if org-fontify-whole-heading-line
         "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
           "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
        (1 (org-get-level-face 1))
        (2 (org-get-level-face 2))
        (3 (org-get-level-face 3)))
      ;; Table lines
      '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)\n?"
            (0 'org-table-row t)
        (1 'org-table t))
      ;; Table internals
      '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
      '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
      '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
      '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
      ;; Properties
      (list org-property-re
        '(1 'org-special-keyword t)
        '(3 'org-property-value t))
      ;; Drawers
      '(org-fontify-drawers)
      ;; Link related fontification.
      '(org-activate-links)
      (when (memq 'tag org-highlight-links) '(org-activate-tags (1 'org-tag prepend)))
      (when (memq 'radio org-highlight-links) '(org-activate-target-links (1 'org-link prepend)))
      (when (memq 'date org-highlight-links) '(org-activate-dates (0 'org-date prepend)))
      (when (memq 'footnote org-highlight-links) '(org-activate-footnote-links))
          ;; Targets.
          (list org-radio-target-regexp '(0 'org-target t))
      (list org-target-regexp '(0 'org-target t))
      ;; Diary sexps.
      '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
      ;; Macro
      '(org-fontify-macros)
      ;; TODO keyword
      (list (format org-heading-keyword-regexp-format
            org-todo-regexp)
        '(2 (org-get-todo-face 2) prepend))
      ;; TODO
      (when org-fontify-todo-headline
        (list (format org-heading-keyword-regexp-format
              (concat
               "\\(?:"
               (mapconcat 'regexp-quote org-not-done-keywords "\\|")
               "\\)"))
          '(2 'org-headline-todo prepend)))
      ;; DONE
      (when org-fontify-done-headline
        (list (format org-heading-keyword-regexp-format
              (concat
               "\\(?:"
               (mapconcat 'regexp-quote org-done-keywords "\\|")
               "\\)"))
          '(2 'org-headline-done prepend)))
      ;; Priorities
      '(org-font-lock-add-priority-faces)
      ;; Tags
      '(org-font-lock-add-tag-faces)
      ;; Tags groups
      (when (and org-group-tags org-tag-groups-alist)
        (list (concat org-outline-regexp-bol ".+\\(:"
              (regexp-opt (mapcar 'car org-tag-groups-alist))
              ":\\).*$")
          '(1 'org-tag-group prepend)))
      ;; Special keywords
      (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
      (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
      (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
      (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
      ;; Emphasis
      (when org-fontify-emphasized-text '(org-do-emphasis-faces))
      ;; Checkboxes
      `(,org-list-full-item-re 3 'org-checkbox prepend lax)
      (when (cdr (assq 'checkbox org-list-automatic-rules))
        '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
          (0 (org-get-checkbox-statistics-face) prepend)))
      ;; Description list items
          '("\\(?:^[ \t]*[-+]\\|^[ \t]+[*]\\)[ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
        1 'org-list-dt prepend)
          ;; Inline export snippets
          '("\\(@@\\)\\([a-z-]+:\\).*?\\(@@\\)"
            (1 'font-lock-comment-face t)
            (2 'org-tag t)
            (3 'font-lock-comment-face t))
      ;; ARCHIVEd headings
      (list (concat
         org-outline-regexp-bol
         "\\(.*:" org-archive-tag ":.*\\)")
        '(1 'org-archived prepend))
      ;; Specials
      '(org-do-latex-and-related)
      '(org-fontify-entities)
      '(org-raise-scripts)
      ;; Code
      '(org-activate-code (1 'org-code t))
      ;; COMMENT
      (list (format
         "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
         org-todo-regexp
         org-comment-string)
        '(9 'org-special-keyword prepend))
      ;; Blocks and meta lines
      '(org-fontify-meta-lines-and-blocks)
          '(org-fontify-inline-src-blocks)
          ;; Citations.  When an activate processor is specified, if
          ;; specified, try loading it beforehand.
          (progn
            (unless (null org-cite-activate-processor)
              (org-cite-try-load-processor org-cite-activate-processor))
            '(org-cite-activate))
          '(org-activate-folds))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
        '(org-font-lock-keywords t nil nil backward-paragraph))
    (setq-local font-lock-extend-after-change-region-function
        #'org-fontify-extend-region)
    (kill-local-variable 'font-lock-keywords)
    nil))
(org-mode)
(insert "<<<radio>>>\n* [2020-09-11 Fri] Headline 1\n
* COMMENT Headline 2")
