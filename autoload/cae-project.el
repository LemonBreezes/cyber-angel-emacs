;;; autoload/cae-project.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-project-maybe-add-project ()
  "If saving a .projectile file, add its directory to projectile's known projects."
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".projectile"))
    (let ((project-root (file-name-directory buffer-file-name)))
      (projectile-add-known-project project-root)
      (message "Added %s to projectile known projects" project-root))))

(defvar cae-project--src-watch-timer nil
  "Debounce timer for `cae-project--src-changed'.")


;;;###autoload
(defun cae-project--src-changed (event)
  "Rediscover projects in ~/src/ when a subdirectory is added.
EVENT is a `file-notify' event."
  (pcase-let ((`(,_desc ,action ,file) event))
    (when (and (memq action '(created renamed))
               (file-directory-p file))
      (when (timerp cae-project--src-watch-timer)
        (cancel-timer cae-project--src-watch-timer))
      (setq cae-project--src-watch-timer
            (run-with-idle-timer
             1.0 nil
             (lambda ()
               (projectile-discover-projects-in-directory
                (expand-file-name "~/src/") 1)))))))
