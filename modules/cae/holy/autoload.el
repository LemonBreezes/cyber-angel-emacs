;;; private/holy/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-avy-symbol-at-point ()
  "Jump to another occurance of symbol with avy."
  (interactive)
  (avy-with symbol-overlay-jump-avy
    (avy-process
     (avy--regex-candidates (regexp-quote (thing-at-point 'symbol t))))))

;;;###autoload
(defun cae-edit-indirect-dwim ()
  "DWIM version of edit-indirect-region.
When region is selected, behave like `edit-indirect-region'
but when no region is selected and the cursor is in a 'string' syntax
mark the string and call `edit-indirect-region' with it."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'edit-indirect-region))
        ((and (derived-mode-p 'org-mode)
              (ignore-error 'user-error (call-interactively #'org-edit-special))))
        (t
         (call-interactively #'string-edit-at-point))))

;;;###autoload
(defun cae-lookup-definition-dwim ()
  (interactive)
  (require 'ffap)
  (if-let ((file (ffap-file-at-point)))
      (if (and (file-exists-p file)
               (not (and buffer-file-name
                         (string= (file-truename file)
                                  (file-truename buffer-file-name)))))
          (progn (better-jumper-set-jump (marker-position (point-marker)))
                 (find-file file))
        (call-interactively #'ffap))
    (call-interactively #'+lookup/definition)))

;;;###autoload
(defun cae-lookup-definition-dwim (identifier &optional arg)
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (require 'ffap)
  (cond ((null identifier) (user-error "Nothing under point"))
        ((and (string-match-p "/" identifier)
              (file-exists-p identifier)
              (not (and buffer-file-name
                        (file-equal-p identifier buffer-file-name))))
         (progn (better-jumper-set-jump (marker-position (point-marker)))
                (find-file identifier)))
        ((+lookup--jump-to :definition identifier nil arg))
        ((user-error "Couldn't find the definition of %S" (substring-no-properties identifier)))))
