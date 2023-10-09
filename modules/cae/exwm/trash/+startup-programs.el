;;; private/exwm/+startup.el -*- lexical-binding: t; -*-

;; The way we integrate `persp-mode' and EXWM is by running our most used
;; programs on startup and using these programs in a dedicated workspace.

(defvar +startup-prefix "k")
(defvar +startup-directory (concat (dir!) "startup-programs/"))
(defvar +startup-config-dir (concat +startup-directory "config-files/"))
(defvar +startup-applications-map (make-sparse-keymap))
(define-prefix-command '+startup-applications-map)

(map! :leader :desc "startup applications" +startup-prefix #'+startup-applications-map)

(defun +workspace-switch-to-exwm-buffer-maybe (&rest _)
  ;; Also rename buffers to their titles
  (cl-loop for buf in (+workspace-buffer-list)
           if (buffer-local-value 'exwm-title buf)
           do (with-current-buffer buf
                (exwm-workspace-rename-buffer exwm-title)))
  (when-let ((buf (cl-find-if
              (lambda (buf)
                (and (buffer-local-value 'exwm-class-name buf)
                     (string-match-p (downcase (+workspace-current-name))
                                     (buffer-local-value 'exwm-class-name buf))))
              (buffer-list))))
    (set-persp-parameter 'dont-save-to-file t (get-current-persp))
    (unless (+popup-window-p)
      (unless (doom-visible-buffer-p buf)
        (switch-to-buffer buf)))))

;; Run startup progrms
(defun tmp/exwm-init-hook ()
  (dolist (file (directory-files +startup-directory t "\\.el$"))
    (load (file-name-sans-extension file) nil t))

  ;;(advice-add #'+workspace/switch-to :after #'+workspace-switch-to-exwm-buffer-maybe)
  (remove-hook 'exwm-init-hook #'tmp/exwm-init-hook))

(if exwm--connection
    (progn (tmp/exwm-init-hook)
           (add-to-list 'persp-filter-save-buffers-functions
                        #'exwm--buffer->id))
  (add-hook 'exwm-init-hook #'tmp/exwm-init-hook :append))
