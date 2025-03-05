;;; autoload/cae-lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-eglot-ensure-no-home-directory ()
  "Prevent Eglot from starting if the project directory is the home directory."
  (let ((project (project-current t)))
    (when project
      (let ((root (project-root project)))
        (when (or (equal (expand-file-name "~") (expand-file-name root))
                  (equal "/" root))
          (error "Eglot aborted: refusing to use home directory as project root"))
        ;; Proceed to call the original eglot-ensure only if checks pass
        (call-interactively #'eglot-ensure)))))
