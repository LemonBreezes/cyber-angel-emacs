;;; autoload/cae-project.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-project-maybe-add-project ()
  "If saving a .projectile file, add its directory to projectile's known projects."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".projectile"))
    (let ((project-root (file-name-directory buffer-file-name)))
      (projectile-add-known-project project-root)
      (message "Added %s to projectile known projects" project-root))))
