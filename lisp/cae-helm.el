;;; lisp/cae-helm.el -*- lexical-binding: t; -*-

(advice-add #'helm-packages-install :around
        (cae-defun cae-helm-packages-install-and-require-a (oldfun candidate)
          (let ((pkgs (helm-marked-candidates)))
          (cae-always-yes-a oldfun candidate)
          (dolist (pkg pkgs)
            (require pkg)))))
