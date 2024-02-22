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

(defun tree-sitter-hl--setup ()
  "Set up `tree-sitter-hl' in the current buffer.
This assumes both `tree-sitter-mode' and `font-lock-mode' were already enabled."
  ;; TODO: If there's an error, disable `tree-sitter-hl--extra-patterns-list'
  ;; and retry.
  (when (tree-sitter-hl--ensure-query)
    (unless tree-sitter-hl--query-cursor
      (setq tree-sitter-hl--query-cursor (tsc-make-query-cursor))
      ;; Invalidate the buffer, only if we were actually disabled previously.
      (tree-sitter-hl--invalidate))
    ;; TODO: Override `font-lock-extend-after-change-region-function', or hook
    ;; into `jit-lock-after-change-extend-region-functions' directly. For that to
    ;; work, we need to make sure `tree-sitter--after-change' runs before
    ;; `jit-lock-after-change'.
    ;;(add-hook 'tree-sitter-after-change-functions
    ;;          #'tree-sitter-hl--invalidate
    ;;          nil :local)
    ;; TODO: Figure out how to properly integrate with `jit-lock-mode' directly,
    ;; without relying on `font-lock-mode'. Among other things, it would enable
    ;; highlighting without setting `font-lock-defaults'. At the moment,
    ;; `font-lock-mode' somehow helps with making sure that fontification is
    ;; updated in-time, instead of eventually.
    (add-function :around (local 'font-lock-fontify-region-function)
                  #'tree-sitter-hl--highlight-region-with-fallback)
    ;;(tree-sitter-hl--minimize-font-lock-keywords)
    ;; XXX: We used to have a hack that calls`font-lock-turn-on-thing-lock',
    ;; which allows turning on tree-based syntax highlighting by temporarily
    ;; binding `major-mode', even though such a major mode may not be installed,
    ;; or does not exist. For example:
    ;;
    ;;     (let ((major-mode 'go-mode)) (tree-sitter-hl-mode))
    ;;
    ;; However, if `font-lock-mode' is subsequently disabled, because
    ;; `font-lock-turn-off-thing-lock' does not properly clean up the local
    ;; value of `font-lock-ensure-function', calling `font-lock-ensure' will
    ;; signal an error. For example, this happens when org-mode's code blocks
    ;; are highlighted). Therefore, we disabled that hack. See
    ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/74
    ))

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
