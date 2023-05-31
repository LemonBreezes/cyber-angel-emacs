;;; autoload/cae-editor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-kill-buffer-a (orig-func &optional buffer-or-name)
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (catch 'quit
    (save-window-excursion
      (with-current-buffer buffer-or-name
        (let (done (buf (current-buffer)))
          (when (and buffer-file-name (buffer-modified-p))
            (while (not done)
              (let ((response (read-char-choice
                               (format "Save file %s? (y, n, d, q) " (buffer-file-name buf))
                               '(?y ?n ?d ?q))))
                (setq done (cond
                            ((eq response ?q) (throw 'quit nil))
                            ((eq response ?y) (save-buffer) t)
                            ((eq response ?n) (set-buffer-modified-p nil) t)
                            ((eq response ?d) (diff-buffer-with-file) nil))))))
          (funcall orig-func buffer-or-name))))))

;;;###autoload
(defun cae-delete-char ()
  (interactive)
  ;; Needs refactoring.
  (let ((delete-fn
         (cond ((condition-case error
                    (scan-sexps (point-min) (point-max))
                  (scan-error t))
                #'delete-char)
               ((bound-and-true-p lispy-mode)
                #'lispy-delete)
               ((bound-and-true-p smartparens-mode)
                (if (region-active-p)
                    #'sp-delete-region
                  #'sp-delete-char))
               (t #'delete-char))))
    (call-interactively delete-fn)))

;;;###autoload
(defun cae-toggle-sudo ()
  (interactive)
  (let* ((file (or buffer-file-name
                   (when (or (derived-mode-p 'dired-mode)
                             (derived-mode-p 'wdired-mode))
                     default-directory)))
         (file-localname (file-remote-p file 'localname))
         (tramp-prefix (and file-localname
                            (string-remove-suffix file-localname file)))
         (sudo-prefix (format "/sudo:root@%s:" (file-remote-p file 'host))))
    (if (string-suffix-p sudo-prefix tramp-prefix)
        (find-file (concat (string-remove-suffix sudo-prefix tramp-prefix)
                           (tramp-file-local-name file)))
      (doom/sudo-this-file))))

;;;###autoload
(defun cae-raise-sexp ()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end)))
          (goto-char end)
          (delete-region end (progn (sp-up-sexp) (point)))
          (goto-char beg)
          (delete-region beg (progn (sp-backward-up-sexp) (point)))))
    (call-interactively #'sp-raise-sexp)))


(defun cae-switch-buffer--handle-dirvish ()
  (when (and (featurep 'dirvish)
             (dirvish-curr)
             (> (length (dv-layout (dirvish-curr))) 1))
    (dirvish-layout-toggle)))

;;;###autoload
(defun cae-previous-buffer ()
  (interactive)
  (cae-switch-buffer--handle-dirvish)
  (call-interactively #'previous-buffer))

;;;###autoload
(defun cae-next-buffer ()
  (interactive)
  (cae-switch-buffer--handle-dirvish)
  (call-interactively #'next-buffer))

;;;###autoload (autoload 'cae-embark-collect-cheatsheet-hydra/body "autoload/cae-editor" nil t)
(defhydra embark-collect-cheatsheet-hydra (:color pink :foreign-keys run)
  ("<f6>" nil "Exit" :exit t)
  ("q" nil "Exit" :exit t)
  ("a" embark-act "Act" :column "Act")
  ("<" beginning-of-buffer "First candidate" :column "Navigation")
  (">" end-of-buffer "Last candidate" :column "Navigation")
  ("A" embark-act-all "Act all" :column "Act")
  ("E" embark-export "Export" :column "Act")
  ("M-<left>" tabulated-list-previous-column "Previous column" :column "Navigation")
  ("M-<right>" tabulated-list-next-column "Next column" :column "Navigation")
  ("M-a" embark-collect-direct-action-minor-mode "Toggle direct action mode" :column "Act")
  ("M-{" outline-previous-heading "Previous heading" :column "Navigation")
  ("M-}" outline-next-heading "Next heading" :column "Navigation")
  ("S" tabulated-list-sort "Sort" :column "Navigation")
  ("SPC" scroll-up-command "Scroll up" :column "Navigation")
  ("S-SPC" scroll-down-command "Scroll down" :column "Navigation")
  ("U" embark-collect-unmark-all "Unmark all" :column "Act")
  ("g" revert-buffer "Revert buffer" :column "Act")
  ("m" embark-collect-mark "Mark candidate" :column "Act")
  ("s" isearch-forward "Search forward" :column "Navigation")
  ("t" embark-collect-toggle-marks "Toggle marks" :column "Act")
  ("u" embark-collect-unmark "Unmark candidate" :column "Act")
  ("{" tabulated-list-narrow-current-column "Narrow current column" :column "Navigation")
  ("}" tabulated-list-widen-current-column "Widen current column" :column "Navigation"))
