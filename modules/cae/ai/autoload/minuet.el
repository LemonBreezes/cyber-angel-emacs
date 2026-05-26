;;; cae/ai/autoload/minuet.el -*- lexical-binding: t; -*-

(defvar minuet--current-suggestions)
(defvar minuet--current-overlay)
(defvar minuet--current-suggestion-index)
(declare-function minuet--cleanup-suggestion "minuet")
(declare-function minuet--display-suggestion "minuet")

;;;###autoload
(defun cae-minuet-accept-suggestion-word (&optional n)
  "Accept N words of the current suggestion.
When called interactively with a numeric prefix argument, accept that
many words.  Without a prefix argument, accept only the first word.

Modeled after `minuet-accept-suggestion-line', but splits on word
boundaries instead of lines.  The source buffer's syntax table is
carried into the scan so word boundaries match the language being
edited."
  (interactive "p")
  (when (and minuet--current-suggestions
             minuet--current-overlay)
    (let* ((suggestion (nth minuet--current-suggestion-index
                            minuet--current-suggestions))
           (n (or n 1))
           ;; Replay `forward-word' over the suggestion in a temp buffer to find
           ;; where the Nth word ends, then split the raw string there so any
           ;; leading/trailing whitespace is preserved in the remainder.
           (split-point (let ((syntax-table (syntax-table)))
                          (with-temp-buffer
                            (set-syntax-table syntax-table)
                            (insert suggestion)
                            (goto-char (point-min))
                            (forward-word n)
                            (1- (point)))))
           (selected-text (substring suggestion 0 split-point))
           (remaining-suggestion (substring suggestion split-point)))
      (minuet--cleanup-suggestion)
      (insert selected-text)
      (unless (string-empty-p remaining-suggestion)
        (minuet--display-suggestion (list remaining-suggestion) 0)
        ;; Keep the remainder on screen so successive M-f presses accept more
        ;; words.  Minuet's auto-suggestion (`minuet--maybe-show-suggestion' on
        ;; `post-command-hook') only refrains from clobbering the overlay when
        ;; the last command's name matches "^minuet" (see
        ;; `minuet--is-minuet-command').  This command is `cae-'-prefixed, so we
        ;; masquerade as a Minuet command to suppress that re-trigger.
        (setq this-command 'minuet-accept-suggestion-word)))))
