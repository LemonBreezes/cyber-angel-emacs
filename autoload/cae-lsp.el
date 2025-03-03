;;; autoload/cae-lsp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-eglot-prevent-home-directory-project (orig-fun)
  "Advice for `eglot--current-project' to prevent using home directory as project."
  (let ((project (funcall orig-fun)))
    (when project
      (let ((root (project-root project)))
        (when (equal (expand-file-name "~") (expand-file-name root))
          (error "Eglot aborted: refusing to use home directory as project root"))))
    project))
