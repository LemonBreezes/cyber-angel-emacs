;;; vanilla-emacs-configs/cape-tree-sitter-faces-bug.el -*- lexical-binding: t; -*-

;; Bootstrap straight
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

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter-hl)

(defun tree-sitter-hl--append-text-property (start end prop value &optional object)
  "Append VALUE to PROP of the text from START to END.
This is similar to `font-lock-append-text-property', but deduplicates values. It
also expects VALUE to be a single value, not a list. Additionally, if PROP was
previously nil, it will be set to VALUE, not (list VALUE)."
  (let (next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
           (listp prev)
           (or (keywordp (car prev))
               (memq (car prev) '(foreground-color background-color)))
           (setq prev (list prev)))
      (unless (listp prev)
        (setq prev (list prev)))
      (unless (memq value prev)
        (put-text-property start next prop
                           ;; Reduce GC pressure by not making a list if it's
                           ;; just a single face.
                           (if prev
                               (append prev (list value))
                             value)
                           object)
        (message "put-text-property: %s %s %s %s %s" start next prop (get-text-property start prop) object)
        (message "get-text-property: %s | get-pos-property: %s"
                 (get-text-property start prop)
                 (get-pos-property start prop)))
      (setq start next))))

(defun tree-sitter-hl--invalidate (&optional old-tree)
  "Mark regions of text to be rehighlighted after a text change.
Installed on `tree-sitter-after-change-functions'.

OLD-TREE is the tree before the edit."
  )

(require 'cc-mode)
(scratch-buffer)
(insert "int main() { return 0; // comment")
(c-mode)
(setq tree-sitter-hl-enable-query-region-extension t)
(tree-sitter-hl-mode)
(tree-sitter-hl--highlight-region (point-min) (point-max))
(message "HELLO: get-text-property: %s | get-pos-property: %s"
         (get-text-property (point-max) 'face)
         (get-pos-property (point-max) 'face))
