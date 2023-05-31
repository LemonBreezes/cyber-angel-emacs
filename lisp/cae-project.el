;;; lisp/cae-project.el -*- lexical-binding: t; -*-

(defvar cae-project-bookmark-dir (concat doom-cache-dir "cae-project-bookmarks/")
  "Directory to store project bookmarks.")

(defvar cae-project-bookmark-cache (make-hash-table :test 'equal)
  "Cache of project bookmarks.")

(defvar cae-project-bookmark-separate-into-branches t
  "If non-nil, separate bookmarks into Git branches.")

(defun cae-project--get-bookmark-file (&optional project)
  "Return the bookmark file for PROJECT."
  (expand-file-name (concat (doom-project-name project)
                            "/"
                            (if cae-project-bookmark-separate-into-branches
                                (vc-git--symbolic-ref
                                 (or project
                                     (doom-project-root)))
                              "default")
                            ".bmk")
                    cae-project-bookmark-dir))

(defun cae-project--bookmark-alist-from-file (file)
  "Return a bookmark alist from FILE."
  (let ((bookmark-default-file file)
        (bookmark-alist nil))
    (when (file-exists-p file)
      (bookmark-load bookmark-default-file)
      bookmark-alist)))

(defun cae-project--bookmark-alist (&optional project)
  "Return the bookmark alist for the current project."
  (let ((file (cae-project--get-bookmark-file project)))
    (or (gethash file cae-project-bookmark-cache)
        (puthash file (cae-project--bookmark-alist-from-file file)
                 cae-project-bookmark-cache))))

(defun cae-project-bookmark-set (&optional name no-overwrite)
  "Set a bookmark in the current project."
  (interactive (list nil current-prefix-arg))
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (bookmark-set name no-overwrite)
    (puthash bookmark-default-file bookmark-alist cae-project-bookmark-cache)))

(defun cae-project-bookmark-save-all ()
  "Save all project bookmarks."
  (interactive)
  (maphash (lambda (bookmark-default-file bookmark-alist)
             (ignore bookmark-alist)
             (when bookmark-alist
               (make-directory (file-name-directory bookmark-default-file) t)
               (bookmark-write-file bookmark-default-file)))
           cae-project-bookmark-cache))

(add-hook 'kill-emacs-hook #'cae-project-bookmark-save-all)

(defvar-keymap cae-project-bookmark-embark-map
  :doc "Keymap for Embark project bookmarks actions."
  :parent embark-bookmark-map)

(map-keymap
 (lambda (key def)
   (when (string-match-p "^bookmark-" (symbol-name def))
     ;; define an analogous command that uses the current project's bookmark file
     (let ((command (intern (format "+%s"
                                    (symbol-name def)))))
       (defalias command
         `(lambda ()
           (interactive)
           (let ((bookmark-alist (cae-project--bookmark-alist))
                 (bookmark-default-file (cae-project--get-bookmark-file)))
             (ignore bookmark-alist bookmark-default-file)
             (call-interactively #',def)))
         (format "Analogous command to `%s' that uses the current project's bookmark file."
                 (symbol-name def)))
       (define-key cae-project-bookmark-embark-map (vector key) command))))
 embark-bookmark-map)

(setf (alist-get 'project-bookmark embark-keymap-alist)
      #'cae-project-bookmark-embark-map)

(setf (alist-get 'project-bookmark embark-exporters-alist)
      (defalias 'cae-project-bookmark-export
        (lambda (cands)
          (let ((bookmark-alist (cae-project--bookmark-alist))
                (bookmark-default-file (cae-project--get-bookmark-file)))
            (ignore bookmark-alist bookmark-default-file)
            (embark-export-bookmarks cands)))))

(setf (alist-get 'cae-project-bookmark-delete embark-pre-action-hooks)
      (alist-get 'bookmark-delete embark-pre-action-hooks))
(setf (alist-get 'cae-project-bookmark-rename embark-post-action-hooks)
      (alist-get 'bookmark-rename embark-post-action-hooks))
(setf (alist-get 'cae-project-bookmark-rename embark-post-action-hooks)
      (alist-get 'bookmark-rename embark-post-action-hooks))

(defun cae-project-bookmark (name)
  "Consult bookmarks in the current project."
  (interactive
   (list
    (let ((narrow (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                            consult-bookmark-narrow))
            (bookmark-alist (cae-project--bookmark-alist))
            (bookmark-default-file (cae-project--get-bookmark-file)))
      (ignore bookmark-alist bookmark-default-file)
      (consult--read
       (consult--bookmark-candidates)
       :prompt "Bookmark: "
       :state (consult--bookmark-preview)
       :category 'project-bookmark
       :history 'bookmark-history
       ;; Add default names to future history.
       ;; Ignore errors such that `consult-bookmark' can be used in
       ;; buffers which are not backed by a file.
       :add-history (ignore-errors (bookmark-prop-get (bookmark-make-record) 'defaults))
       :group (consult--type-group narrow)
       :narrow (consult--type-narrow narrow)))))
  (let ((bookmark-alist (cae-project--bookmark-alist))
        (bookmark-default-file (cae-project--get-bookmark-file)))
    (ignore bookmark-alist bookmark-default-file)
    (bookmark-maybe-load-default-file)
    (if (assoc name bookmark-alist)
        (bookmark-jump name)
      (cae-project-bookmark-set name))))

(map! :leader
      :prefix "p"
      :desc "Project bookmarks" "C-b" #'cae-project-bookmark)
