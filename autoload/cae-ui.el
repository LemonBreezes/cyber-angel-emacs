;;; ~/.doom.d/autoload/cae-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ui-topsy-rjsx-fn ()
  (when (> (window-start) 1)
    (save-excursion
      (goto-char (window-start))
      (while (save-excursion
               (forward-line -1)
               (looking-at-p "^\\s-*//"))
        (forward-line -1))
      (ignore-errors
        (js2-beginning-of-defun))
      (back-to-indentation)
      (font-lock-ensure (point) (point-at-eol))
      (buffer-substring (point) (point-at-eol)))))

;;;###autoload
(defun cae-ui-which-key-show-workspace (orig-fun &rest pages-obj)
  "Show my workspaces in the echo thingy"
  (let ((out (apply orig-fun pages-obj))
        (prefix-title (which-key--pages-prefix-title (car pages-obj))))
    (if (not (or (string-equal prefix-title "workspace")
                 (string-equal prefix-title "workspaces/windows")))
        out
      (cons (car out)
            (lambda ()
              (funcall (cdr out))
              (which-key--echo (concat (current-message) " " (+workspace--tabline))))))))
