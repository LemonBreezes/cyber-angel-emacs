;;; trash/autoload/cae-editor.el -*- lexical-binding: t; -*-

;; Doom Emacs made some improvements to `doom/sudo-this-file' so I'm using that
;; instead now:
;; https://github.com/doomemacs/doomemacs/commit/f18603e66a1160927044d13a46f15bbddfd72d39
;;;###autoload
(defun cae-toggle-sudo ()
  "Toggle sudo access for the current file."
  (interactive)
  (let* ((file (or buffer-file-name
                   (when (or (derived-mode-p 'dired-mode)
                             (derived-mode-p 'wdired-mode))
                     default-directory)))
         (file-localname (file-remote-p file 'localname))
         (tramp-prefix (and file-localname
                            (string-remove-suffix file-localname file)))
         (sudo-prefix (format "/sudo:root@%s:" (file-remote-p file 'host)))
         (p (point)))
    (if (string-suffix-p sudo-prefix tramp-prefix)
        (find-file (concat (string-remove-suffix sudo-prefix tramp-prefix)
                           (tramp-file-local-name file)))
      (doom/sudo-this-file))
    (unless (buffer-modified-p)
      (goto-char p))))

;; I used to advise `kill-buffer' with this but stopped doing so since it always
;; caused problems to pop up elsewhere somehow.
(defun cae-kill-buffer-query-diff-a (orig-func &optional buffer-or-name)
  "Like `kill-buffer', but prompts to diff or save the buffer if it's modified."
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (when (and (buffer-local-value 'buffer-file-name (get-buffer buffer-or-name))
             (buffer-modified-p (get-buffer buffer-or-name)))
    (when (+popup-window-p)
      (backtrace))
    (catch 'quit
      (save-window-excursion
        (with-current-buffer buffer-or-name
          (let ((done nil) (buf (current-buffer)))
            (while (not done)
              (let ((response (read-char-choice
                               (format "Save file %s? (y, n, d, q) "
                                       (buffer-file-name buf))
                               '(?y ?n ?d ?q))))
                (setq done (cond
                            ((eq response ?q) (throw 'quit nil))
                            ((eq response ?y) (save-buffer) t)
                            ((eq response ?n) (set-buffer-modified-p nil) t)
                            ((eq response ?d) (diff-buffer-with-file) nil))))))))))
  (funcall orig-func buffer-or-name))
