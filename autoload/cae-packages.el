;;; autoload/cae-packages.el -*- lexical-binding: t; -*-

;; Interactive commands to freeze every installed package to the SHA it is
;; *currently* checked out at, so that all of my Doom instances can be synced to
;; identical package versions.
;;
;; Doom's own `doom/bump-package*' commands pin to the *latest upstream* commit,
;; which is not what we want here.  These commands instead read the commit each
;; package's local repo is sitting on right now and write them out as Doom
;; `:pin's into `cae-packages-freeze-file' (packages.lock.el).  That file is
;; `load'ed at the tail of packages.el, so `doom sync' on any machine reproduces
;; the exact versions.
;;
;; CAVEAT: do NOT pair this with `(unpin! t)' in packages.el.  `unpin! t' stamps
;; `:unpin t' on every declared package, and `doom-package-pinned-alist' uses
;; `(unless unpin pin)', so it drops the pin of anything carrying `:unpin' --
;; which silently defeats every frozen pin.  The frozen pins already override
;; Doom's module pins by themselves (the file is loaded last, so its `:pin's
;; win), so `unpin! t' is both unnecessary and harmful here.

(require 'cl-lib)

(defvar cae-packages-freeze-file
  (file-name-concat doom-user-dir "packages.lock.el")
  "File that `cae-packages-freeze' writes generated `:pin's into.
This file is `load'ed from the end of packages.el.")

(defvar cae-packages-freeze-excluded-packages nil
  "List of package symbols to never freeze.
Useful for packages you intentionally want to track upstream HEAD of.")

(defun cae-packages--current-pins ()
  "Return a sorted alist of (PACKAGE-NAME-STRING . COMMIT) for installed packages.
Enumerates every non-built-in package straight has a recipe for and reads the
commit its local repo is currently checked out at."
  (doom-initialize-packages)
  (let (pins)
    (dolist (recipe (doom-package-recipe-alist))
      (cl-destructuring-bind (&key package local-repo type &allow-other-keys)
          recipe
        (when (and package local-repo
                   (not (memq (intern package)
                              cae-packages-freeze-excluded-packages)))
          (when-let* ((commit (ignore-errors
                                (straight-vc-get-commit type local-repo))))
            (setf (alist-get package pins nil nil #'equal) commit)))))
    (cl-sort pins #'string< :key #'car)))

;;;###autoload
(defun cae-packages-freeze ()
  "Freeze every installed package to its currently checked-out SHA.
Writes `(package! NAME :pin \"SHA\")' forms for all installed packages into
`cae-packages-freeze-file' (packages.lock.el).  Commit that file (and
packages.el) and run `doom sync' on your other instances to put them on
identical versions."
  (interactive)
  (let ((pins (cae-packages--current-pins)))
    (unless pins
      (user-error "No installed packages found to freeze"))
    (with-temp-file cae-packages-freeze-file
      (insert ";; -*- no-byte-compile: t; -*-\n"
              ";;; packages.lock.el --- AUTO-GENERATED, DO NOT EDIT BY HAND\n;;\n"
              ";; Regenerate with `M-x cae-packages-freeze'.  Each entry pins a package\n"
              ";; to the commit it was installed at when this file was written.  Loaded\n"
              ";; from the end of packages.el.\n\n")
      (dolist (pin pins)
        (insert (format "(package! %s :pin %S)\n" (car pin) (cdr pin)))))
    (message "Froze %d package%s to %s"
             (length pins)
             (if (= (length pins) 1) "" "s")
             (abbreviate-file-name cae-packages-freeze-file))))

;;;###autoload
(defun cae-packages-unfreeze ()
  "Remove all frozen pins by clearing `cae-packages-freeze-file' (packages.lock.el).
Run `doom sync' afterwards to let packages float again."
  (interactive)
  (when (and (file-exists-p cae-packages-freeze-file)
             (or (not (called-interactively-p 'interactive))
                 (yes-or-no-p (format "Clear all frozen pins in %s? "
                                      (abbreviate-file-name
                                       cae-packages-freeze-file)))))
    (with-temp-file cae-packages-freeze-file
      (insert ";; -*- no-byte-compile: t; -*-\n"
              ";;; packages.lock.el --- AUTO-GENERATED, DO NOT EDIT BY HAND\n\n"))
    (message "Cleared frozen pins; run `doom sync' to thaw")))
