;;; cae/ai/autoload/fancy-dabbrev.el -*- lexical-binding: t; -*-

;; Wrappers for fancy-dabbrev that overload C-f / M-f when a preview is up:
;;   C-f -> commit the whole expansion (like `fancy-dabbrev-expand')
;;   M-f -> commit just the first word of the suggestion, keep the rest in
;;          the preview (Copilot-style partial accept).
;; With no preview to consume, both fall through to ordinary `forward-char' /
;; `forward-word'.
;;
;; The preview overlay is destroyed by `fancy-dabbrev--pre-command-hook' before
;; any command runs, so by the time these functions execute the overlay is
;; gone. We key off `fancy-dabbrev--preview-overlay-was-visible' (set by that
;; same hook) and, for the partial-accept path, recompute the suffix from
;; scratch with `fancy-dabbrev--get-first-expansion'. Point hasn't moved yet,
;; so dabbrev finds the same expansion the overlay was showing.

(defun cae-fancy-dabbrev--preview-suffix ()
  "Recompute the suffix the preview overlay would (or did) show. Nil if there
isn't one in the current context."
  (when (and (bound-and-true-p fancy-dabbrev-mode)
             (fancy-dabbrev--looking-back-at-expandable)
             (fancy-dabbrev--in-previewable-context))
    (when-let* ((expansion (ignore-errors (fancy-dabbrev--get-first-expansion)))
                (abbrev fancy-dabbrev--entered-abbrev))
      (substring expansion (length abbrev)))))

(defun cae-fancy-dabbrev--take-units (suffix n step-fn)
  "Return the prefix of SUFFIX that STEP-FN traverses in N steps, using the
calling buffer's syntax table so word boundaries match the major mode."
  (let ((st (syntax-table)))
    (with-temp-buffer
      (set-syntax-table st)
      (insert suffix)
      (goto-char (point-min))
      (ignore-errors (funcall step-fn n))
      (buffer-substring-no-properties (point-min) (point)))))

(defun cae-fancy-dabbrev--accept (n step-fn)
  "Commit N units of the current preview suffix (via STEP-FN). Non-nil if
anything was inserted. Re-displays the preview for the remaining suffix."
  (when (and (bound-and-true-p fancy-dabbrev--preview-overlay-was-visible)
             (> n 0))
    (when-let* ((suffix (cae-fancy-dabbrev--preview-suffix))
                ((not (string-empty-p suffix)))
                (take (cae-fancy-dabbrev--take-units suffix n step-fn))
                ((not (string-empty-p take))))
      (insert take)
      ;; Show what's left as the next preview. `fancy-dabbrev--post-command-hook'
      ;; won't do this for us because our command isn't in
      ;; `fancy-dabbrev-self-insert-commands'; calling synchronously is fine.
      (ignore-errors (fancy-dabbrev--preview))
      t)))

;;;###autoload
(defun cae-fancy-dabbrev-forward-word-or-complete-word (n)
  "If a fancy-dabbrev preview was just showing, commit its first N words and
keep the rest in the preview. Otherwise, `forward-word' by N."
  (interactive "p")
  (unless (cae-fancy-dabbrev--accept n #'forward-word)
    (forward-word n)))

;;;###autoload
(defun cae-fancy-dabbrev-forward-char-or-complete (n)
  "If a fancy-dabbrev preview was just showing, commit the whole suggestion
in one shot. Otherwise, `forward-char' by N. We insert the suffix directly
rather than calling `fancy-dabbrev-expand', because the latter relies on
`dabbrev--last-abbrev-location' being non-nil at the moment of its call —
which it isn't when invoked indirectly through our wrapper after the pre-
command hook ran, producing a wrong-type-argument error."
  (interactive "p")
  (let ((suffix (and (bound-and-true-p fancy-dabbrev--preview-overlay-was-visible)
                     (cae-fancy-dabbrev--preview-suffix))))
    (if (and suffix (not (string-empty-p suffix)))
        (progn (insert suffix)
               (ignore-errors (fancy-dabbrev--preview)))
      (forward-char n))))
