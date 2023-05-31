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

;;;###autoload (autoload 'cae-embark-collect-hydra/body "cae-editor" nil t)
(defhydra embark-collect-mode-hydra (:color blue :hint nil)
  "
Navigation^^             Mark/Unmark^^               Actions^^             Buffer^^
-------------------------------------------------------------------------------------------
[_p_] Outline previous  [_m_] Mark                   [_a_] Act               [_g_] Refresh
[_n_] Outline next      [_u_] Unmark                 [_A_] Act All           [_<_] Beginning
[_b_] Backward button   [_U_] Unmark All             [_e_] Export            [_>_] End
[_f_] Forward button    [_t_] Toggle Marks           [_q_] Quit Window
[_s_] Search Forward
"
  ("p" outline-previous-heading)
  ("n" outline-next-heading)
  ("b" backward-button)
  ("f" forward-button)
  ("s" isearch-forward)
  ("m" embark-collect-mark)
  ("u" embark-collect-unmark)
  ("U" embark-collect-unmark-all)
  ("t" embark-collect-toggle-marks)
  ("a" embark-act)
  ("A" embark-act-all)
  ("e" embark-export)
  ("g" embark-rerun-collect-or-export)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("q" quit-window :color red))
