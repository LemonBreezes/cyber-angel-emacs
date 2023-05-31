;;; private/helm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm--which-key-inhibit-hook ()
  (setq which-key-inhibit nil)
  (remove-hook 'pre-command-hook
               #'+helm--which-key-inhibit-hook))

;;;###autoload
(defun +helm-lazy-load ()
  (interactive)
  (require 'helm)
  (setq unread-command-events (list ?\C-x ?c))
  (setq which-key-inhibit t)

  (add-hook 'pre-command-hook #'+helm--which-key-inhibit-hook)
  (run-with-idle-timer
   which-key-idle-delay nil
   (lambda ()
     (when which-key-inhibit
       (which-key-show-keymap
        'helm-command-map)))))

;;;###autoload
(defun +helm--set-prompt-display (pos)
  "TODO"
  (let (beg state region-active m)
    (with-selected-window (minibuffer-window)
      (setq beg (save-excursion (vertical-motion 0 (helm-window)) (point))
            state evil-state
            region-active (region-active-p)
            m (mark t)))
    (when region-active
      (setq m (- m beg))
      ;; Increment pos to handle the space before prompt (i.e `pref').
      (put-text-property (1+ (min m pos)) (+ 2 (max m pos))
                         'face
                         (list :background (face-background 'region))
                         header-line-format))
    (put-text-property
     ;; Increment pos to handle the space before prompt (i.e `pref').
     (+ 1 pos) (+ 2 pos)
     'face
     (if (eq state 'insert)
         'underline
       ;; Don't just use 'cursor, this can hide the current character.
       (list :inverse-video t
             :foreground (face-background 'cursor)
             :background (face-background 'default)))
     header-line-format)))
