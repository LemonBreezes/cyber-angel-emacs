;;; lisp/cae-helm.el -*- lexical-binding: t; -*-

;; It's super annoying to have to confirm the package
(advice-add #'helm-packages-install :around
            (cae-defun cae-helm-packages-install-and-require-a (oldfun candidate)
              (let ((pkgs (helm-marked-candidates)))
                (cae-always-yes-a oldfun candidate)
                (dolist (pkg pkgs)
                  (condition-case err
                      (require pkg)
                    (message "Failed to load %s: %s" pkg err))))))
