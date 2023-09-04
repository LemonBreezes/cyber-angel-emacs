;;; lisp/redorder-workspaces-automatically.el -*- lexical-binding: t; -*-

;; This code is very old and not good. I need to look at the code for
;; `+workspace/swap-left' eventually and use it write something better.
(when (modulep! :ui workspaces)
  (after! persp-mode
    (defvar +workspaces-prioritized-workspaces nil)
    (defvar +workspaces-deprioritized-workspaces nil)
    (setq +workspaces-prioritized-workspaces
          '("main" ".doom.d" "Chromium" "Discord" "Signal"))
    (setq +workspaces-deprioritized-workspaces
          '("atlas" "flx" "VirtualBox" "Teams"))
    (defun +workspaces--reorder-workspaces (&rest _)
      ;; right of less important workspace -> swap left
      ;; cl-loop for i below (length (+workspace-list-names)) do
      (let* ((i (-elem-index (+workspace-current-name)
                             (+workspace-list-names)))
             (priority (-elem-index (+workspace-current-name)
                                    +workspaces-prioritized-workspaces))
             (depriority (-elem-index (+workspace-current-name)
                                      +workspaces-deprioritized-workspaces))
             (p (-elem-index (nth (1- i) (+workspace-list-names))
                             +workspaces-prioritized-workspaces))
             (d (-elem-index (nth (1- i) (+workspace-list-names))
                             +workspaces-deprioritized-workspaces)))
        (while (or (and (null p) priority)
                   (and p priority (< priority p))
                   (and (null depriority) d)
                   (and d depriority (> d depriority)))
          (+workspace/swap-left)
          (setq i (1- i)
                p (-elem-index (nth (1- i) (+workspace-list-names))
                               +workspaces-prioritized-workspaces)
                d (-elem-index (nth (1- i) (+workspace-list-names))
                               +workspaces-deprioritized-workspaces)))))

    (defun insert-into-list (el n list)
      (let* ((padded-list (cons nil list))
             (c (nthcdr n padded-list)))
        (setcdr c (cons el (cdr c)))
        (cdr padded-list)))

    (advice-add #'+workspace--tabline
                :override
                (defun +workspace--tabline-with-separators (&optional names)
                  (let ((names (or (+workspace-list-names)))
                        (current-name (+workspace-current-name)))
                    (mapconcat
                     #'identity
                     (let ((tab-line-entries
                            (cl-loop for name in names
                                     for i to (length names)
                                     collect
                                     (propertize (format " [%d] %s " (1+ i) name)
                                                 'face (if (equal current-name name)
                                                           '+workspace-tab-selected-face
                                                         '+workspace-tab-face))))
                           (first-deprioritized-entry
                            (-elem-index
                             (--first (member it
                                              +workspaces-deprioritized-workspaces)
                                      names)
                             (+workspace-list-names))))
                       (when first-deprioritized-entry
                         (insert-into-list (propertize "|" 'face 'success)
                                           first-deprioritized-entry
                                           tab-line-entries))
                       tab-line-entries) " "))))

    (advice-add #'+workspace/display :before #'+workspaces--reorder-workspaces)))

(provide 'reorder-workspaces-automatically)
