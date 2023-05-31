;;; lisp/cae-project.el -*- lexical-binding: t; -*-

;;; Project-local bookmarks.

;; TODO handle bookmark bmenu
;; TODO handle the fringe indicator

(defvar cae-project-bookmark-dir (concat doom-cache-dir "cae-project-bookmarks/")
  "Directory to store project bookmarks.")

(defvar cae-project-bookmark-cache (make-hash-table :test 'equal)
  "Cache of project bookmarks.")

(defvar cae-project-bookmark-separate-into-branches t
  "If non-nil, separate bookmarks into Git branches.")

(defun cae-project--get-bookmark-file (&optional project)
  "Return the bookmark file for PROJECT."
  (expand-file-name
   (concat (doom-project-name project)
           "/"
           (if cae-project-bookmark-separate-into-branches
               (or (ignore-errors (vc-git--symbolic-ref
                               (or project
                                   (doom-project-root))))
                   "default")
             "default")
           ".bmk")
   cae-project-bookmark-dir))

(defun cae-project--bookmark-alist-from-file (file)
  "Return a bookmark alist from FILE."
  (let ((bookmark-default-file file)
        (bookmark-alist nil))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(let ((blist (bookmark-alist-from-buffer)))
	  (unless (listp blist)
	    (error "Invalid bookmark list in %s" file))
          ;; TODO: Make modification count compatible.
          (setq bookmark-alist blist))
	(kill-buffer (current-buffer)))
      bookmark-alist)))

(defun cae-project--bookmark-alist (&optional project)
  "Return the bookmark alist for the current project."
  (let ((file (cae-project--get-bookmark-file project)))
    (or (gethash file cae-project-bookmark-cache)
        (puthash file (cae-project--bookmark-alist-from-file file)
                 cae-project-bookmark-cache))))

(defmacro cae-project--with-bookmark-alist (project &rest body)
  "Execute BODY with the bookmark alist for PROJECT."
  (declare (indent defun))
  `(let ((bookmark-alist (cae-project--bookmark-alist ,project))
         (bookmark-default-file (cae-project--get-bookmark-file ,project))
         (bookmark-watch-bookmark-file nil)
         (bookmark-save-flag nil))
     (ignore bookmark-alist bookmark-default-file bookmark-watch-bookmark-file
             bookmark-save-flag)
     (advice-add #'bookmark-maybe-load-default-file :override #'ignore)
     (unwind-protect
         (progn
           ,@body)
       (advice-remove #'bookmark-maybe-load-default-file #'ignore)
       (puthash bookmark-default-file bookmark-alist cae-project-bookmark-cache)
       (unless bookmark-alist
         (delete-file bookmark-default-file)))))

;; (cae-project--bookmark-alist)
;; (cae-project-bookmark-delete)

(defun cae-project-bookmark-save-all ()
  "Save all project bookmarks."
  (interactive)
  (maphash (lambda (file alist)
             (when alist
               (make-directory (file-name-directory file) t)
               (let ((bookmark-alist alist)
                     (bookmark-default-file file)
                     (bookmark-watch-bookmark-file nil)
                     (bookmark-save-flag nil))
               (bookmark-write-file file))))
           cae-project-bookmark-cache))

(after! embark
  (defvar-keymap cae-project-bookmark-embark-map
    :doc "Keymap for Embark project bookmarks actions."
    :parent embark-bookmark-map)

  (map-keymap
   (lambda (key def)
     (when (string-match-p "^bookmark-" (symbol-name def))
       ;; define an analogous command that uses the current project's bookmark file
       (let ((command (intern (format "cae-project-%s"
                                      (symbol-name def)))))
         (defalias command
           `(lambda ()
              (interactive)
              ;; TODO: Fix edit annotation
              (cae-project--with-bookmark-alist nil
                (setq this-command ',def)
                (call-interactively ',def)))
           (format "Analogous command to `%s' that uses the current project's bookmark file."
                   (symbol-name def)))
         (define-key cae-project-bookmark-embark-map (vector key) command))))
   embark-bookmark-map)

  ;; These commands are exceptions to the above rule because they are noninteractive.
  (defun cae-project-bookmark-edit-annotation (bookmark-name-or-record &optional from-bookmark-list)
    (cae-project--with-bookmark-alist nil
      (bookmark-edit-annotation bookmark-name-or-record from-bookmark-list)
      (setq-local bookmark-alist (cae-project--bookmark-alist)
                  bookmark-default-file (cae-project--get-bookmark-file))))
  (defun cae-project-bookmark-show-annotation (bookmark-name-or-record)
    (cae-project--with-bookmark-alist nil
      (bookmark-show-annotation bookmark-name-or-record)
      (setq-local bookmark-alist (cae-project--bookmark-alist)
                  bookmark-default-file (cae-project--get-bookmark-file))))

  (map! :map cae-project-bookmark-embark-map
        "e" #'cae-project-bookmark-edit-annotation
        "a" #'cae-project-bookmark-show-annotation)

  (setf (alist-get 'project-bookmark embark-keymap-alist)
        #'cae-project-bookmark-embark-map)

  (setf (alist-get 'project-bookmark embark-exporters-alist)
        (defalias 'cae-project-bookmark-export
          (lambda (cands)
            (cae-project--with-bookmark-alist nil
              (embark-export-bookmarks cands)))))

  (setf (alist-get 'cae-project-bookmark-delete embark-pre-action-hooks)
        (alist-get 'bookmark-delete embark-pre-action-hooks))
  (setf (alist-get 'cae-project-bookmark-rename embark-post-action-hooks)
        (alist-get 'bookmark-rename embark-post-action-hooks))
  (setf (alist-get 'cae-project-bookmark-rename embark-post-action-hooks)
        (alist-get 'bookmark-rename embark-post-action-hooks)))

(defun cae-project-bookmark (name)
  "Consult bookmarks in the current project."
  (interactive
   (list
    (let ((narrow (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                          consult-bookmark-narrow)))
      (cae-project--with-bookmark-alist nil
        (consult--read
         (consult--bookmark-candidates)
         :prompt "Bookmark: "
         :state (consult--bookmark-preview)
         :category 'project-bookmark
         :history 'bookmark-history
         ;; Add default names to future history.
         ;; Ignore errors such that `consult-project-bookmark' can be used in
         ;; buffers which are not backed by a file.
         :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
         :group (consult--type-group narrow)
         :narrow (consult--type-narrow narrow))))))
  (cae-project--with-bookmark-alist
    (bookmark-maybe-load-default-file)
    (if (assoc name bookmark-alist)
        (bookmark-jump name)
      (cae-project-bookmark-set name))))

(unless (featurep 'embark)
  (defun cae-project-bookmark-set (&optional name no-overwrite)
    "Set a bookmark in the current project."
    (interactive (list nil current-prefix-arg))
    (setq this-command 'bookmark-set)
    (cae-project--with-bookmark-alist nil
      (bookmark-set name no-overwrite))))

(setf (alist-get 'project-bookmark marginalia-annotator-registry)
      (alist-get 'bookmark marginalia-annotator-registry))

(push '((nil . "cae-project-\\(bookmark-.*\\)") . (nil . "\\1"))
      which-key-replacement-alist)

(add-hook 'kill-emacs-hook #'cae-project-bookmark-save-all)
