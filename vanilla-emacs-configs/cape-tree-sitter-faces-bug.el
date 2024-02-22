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

(defun tree-sitter-hl--highlight-region (beg end &optional loudly)
  "Highlight the region (BEG . END).
This is intended to be used as a buffer-local override of
`font-lock-fontify-region-function'.

If LOUDLY is non-nil, print debug messages."
  (tsc--save-context
    (let ((inhibit-point-motion-hooks t)
          (cursor-intangible-mode t))
      ;; Extend the region to be highlighted, so that it is not too wastefully
      ;; small. Then extend it again, based on some heuristic, for querying, to
      ;; avoid certain pathological cases. This is partially analogous to the
      ;; extension done by `font-lock-default-fontify-region'.
      (let ((hl-region `(,beg . ,end))
            (query-region `(,beg . ,end)))
        (tree-sitter-hl--extend-regions hl-region query-region)
        (setf `(,beg . ,end) hl-region)
        (tsc--query-cursor-set-byte-range tree-sitter-hl--query-cursor
                                          (position-bytes (car query-region))
                                          (position-bytes (cdr query-region))))
      (let* ((root-node (tsc-root-node tree-sitter-tree))
             (captures  (tsc--query-cursor-captures-1
                         tree-sitter-hl--query-cursor
                         tree-sitter-hl--query
                         root-node
                         #'tsc--buffer-substring-no-properties)))
        ;; TODO: Handle quitting.
        (with-silent-modifications
          (font-lock-unfontify-region beg end)
          ;; TODO: Consider giving certain combinations of highlight names their
          ;; own faces. For example, it might be desirable for fontification of
          ;; a node that matches both "constructor" and "variable" to be
          ;; different from the union of "constructor fontification" and
          ;; "variable fontification".
          (mapc #'tree-sitter-hl--highlight-capture captures)
          ;; This should primarily be for keywords added through
          ;; `font-lock-add-keywords' (minor modes and end users).
          (when (and tree-sitter-hl-use-font-lock-keywords
                     font-lock-set-defaults)
            (font-lock-fontify-keywords-region beg end loudly)))
        ;; We may have highlighted more, but are only reasonably sure about
        ;; HL-REGION.
        `(jit-lock-bounds ,beg . ,end)))))

(require 'cc-mode)
(scratch-buffer)
(insert "int main() { return 0; // comment")
(c-mode)
(setq tree-sitter-hl-enable-query-region-extension t)
(tree-sitter-hl-mode)
(tree-sitter-hl--highlight-region (point-min) (point-max))
(message "HELLO2: %s %s" (current-buffer) (window-buffer))
(message "HELLO: get-text-property: %s | get-pos-property: %s"
         (get-text-property (point-max) 'face)
         (get-pos-property (point-max) 'face))
