;;; autoload/cae-project.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-project-maybe-discover-projects-a (&rest _)
  "Discover projects when depth-0 search-path entries are missing from known projects."
  (require 'projectile)
  (when (cl-some
         (lambda (entry)
           (when (and (consp entry) (eq (cdr entry) 0))
             (let ((dir (file-name-as-directory (expand-file-name (car entry)))))
               (and (file-directory-p dir)
                    (directory-files dir nil directory-files-no-dot-files-regexp t)
                    (not (member (abbreviate-file-name dir)
                                 projectile-known-projects))
                    (not (member dir projectile-known-projects))))))
         projectile-project-search-path)
    (let ((projectile-known-projects-old projectile-known-projects))
      (message "Discovering projects in search path...")
      (message "%s | %s" projectile-known-projects-old projectile-known-projects)
      (projectile-discover-projects-in-search-path)
      (when (equal projectile-known-projects-old projectile-known-projects)
        (error "No new projects discovered. Update `cae-project-maybe-discover-projects-a'")))))

;;;###autoload
(defun cae-project-maybe-add-project ()
  "If saving a .projectile file, add its directory to projectile's known projects."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".projectile"))
    (let ((project-root (file-name-directory buffer-file-name)))
      (projectile-add-known-project project-root)
      (message "Added %s to projectile known projects" project-root))))
