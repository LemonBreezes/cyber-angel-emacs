;;; cae/ghostel/autoload.el -*- lexical-binding: t; -*-

(defvar ghostel-buffer-name)

(defvar cae-ghostel--id nil)

;;;###autoload
(defun cae-ghostel-toggle (arg)
  "Toggle a ghostel terminal popup window at project root.

If prefix ARG is non-nil, recreate the ghostel buffer in the current project's
root.

Returns the ghostel buffer."
  (interactive "P")
  (cae-ghostel--configure-project-root-and-display
   arg
   (lambda ()
     (let ((buffer-name
            (format "*doom:ghostel-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main")))
           confirm-kill-processes
           current-prefix-arg)
       (when arg
         (let ((buffer (get-buffer buffer-name))
               (window (get-buffer-window buffer-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (window-live-p window)
             (delete-window window))))
       (if-let* ((win (get-buffer-window buffer-name)))
           (delete-window win)
         ;; Reuse the ghostel buffer we tagged earlier, or create one.  We must
         ;; let `ghostel' name+create the buffer and use ITS return value:
         ;; `ghostel'/`ghostel--create' always `generate-new-buffer', ignoring
         ;; the current buffer.  Pre-creating it with `get-buffer-create' just
         ;; collides, so `ghostel' uniquifies to "...<2>" and starts the real
         ;; terminal there while we display the wrong, empty buffer.
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'ghostel-mode)
                                    if (equal (buffer-local-value 'cae-ghostel--id buf)
                                              buffer-name)
                                    return buf)
                           (let ((ghostel-buffer-name buffer-name))
                             ;; `ghostel' pops its buffer same-window; undo that
                             ;; with `save-window-excursion' so our own
                             ;; `pop-to-buffer' below routes through the popup
                             ;; rule in `display-buffer-alist' instead.
                             (save-window-excursion
                               (let ((buf (ghostel)))
                                 (with-current-buffer buf
                                   (setq-local cae-ghostel--id buffer-name))
                                 buf))))))
           (pop-to-buffer buffer)))
       (get-buffer buffer-name)))))

;;;###autoload
(defun cae-ghostel-here (arg)
  "Open a ghostel terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the ghostel buffer."
  (interactive "P")
  (cae-ghostel--configure-project-root-and-display
   arg
   (lambda ()
     (require 'ghostel)
     ;; Mirror `ghostel-project': prefix the buffer name with the project
     ;; so different projects get their own ghostel buffer (and
     ;; `ghostel-project-next' / -previous can cycle them by identity).
     (let ((ghostel-buffer-name
            (project-prefixed-buffer-name
             (string-trim ghostel-buffer-name "*" "*")))
           display-buffer-alist)
       (ghostel)))))

(defun cae-ghostel--configure-project-root-and-display (arg display-fn)
  "Set the environment variable PROOT and display a terminal using DISPLAY-FN.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the ghostel buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load ghostel"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
          (if arg
              default-directory
            project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))


;;;###autoload
(defun cae-ghostel-send-escape ()
  "Send a literal ESC keypress to the terminal."
  (interactive)
  (ghostel-send-key "escape"))

;;;###autoload
(defun cae-ghostel-send-C-x ()
  (interactive)
  (ghostel--send-encoded "x" "ctrl"))

;;;###autoload
(defun cae-ghostel-workspace ()
  (interactive)
  (+workspace-switch "*ghostel*" t)
  (call-interactively #'ghostel))


;;;###autoload
(defun ghostel-send-C-space ()
  "Send EOF to the terminal."
  (interactive)
  (ghostel--send-encoded "\s" "ctrl"))
