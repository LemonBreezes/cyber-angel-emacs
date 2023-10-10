;;; private/misc-applications/autoload/zone.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +zone-pgm-md5 ()
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
                   (point-min) (point-max))))
    ;; Whitespace-fill the window.
    (zone-fill-out-screen (window-width) (window-height))
    (random t)
    (goto-char (point-min))
    (while (not (input-pending-p))
      (when (eobp)
        (goto-char (point-min)))
      (while (not (eobp))
        (delete-region (point) (line-end-position))
        (let ((next-md5 (md5 prev-md5)))
          (insert next-md5)
          (setq prev-md5 next-md5))
        (forward-line 1)
        (zone-park/sit-for (point-min) 0.1)))))

;;;###autoload
(defun +zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
    (intern
     (completing-read
      "Program: "
      (mapcar 'symbol-name
              (progn (require 'zone)
                     zone-programs))))))
  (zone pgm))

(defmacro +misc-applications-with-cursor-hidden (&rest body)
  `(let ((cursor-type-old cursor-type)
         (evil-normal-state-cursor-old evil-normal-state-cursor)
         (evil-insert-state-cursor-old evil-insert-state-cursor)
         (evil-visual-state-cursor-old evil-visual-state-cursor)
         (evil-motion-state-cursor-old evil-motion-state-cursor)
         (evil-replace-state-cursor-old evil-replace-state-cursor)
         (evil-operator-state-cursor-old evil-operator-state-cursor)
         (evil-emacs-state-cursor-old evil-emacs-state-cursor))
     (setq-local cursor-type nil
                 evil-normal-state-cursor '(bar . 0)
                 evil-insert-state-cursor '(bar . 0)
                 evil-visual-state-cursor '(box . 0)
                 evil-motion-state-cursor '(box . 0)
                 evil-replace-state-cursor '(hbar . 0)
                 evil-operator-state-cursor '(hbar . 0)
                 evil-emacs-state-cursor '(hbar . 0))
     (unwind-protect (progn ,@body)
       (setq-local cursor-type cursor-type-old
                   evil-normal-state-cursor evil-normal-state-cursor-old
                   evil-insert-state-cursor evil-insert-state-cursor-old
                   evil-visual-state-cursor evil-visual-state-cursor-old
                   evil-motion-state-cursor evil-motion-state-cursor-old
                   evil-replace-state-cursor evil-replace-state-cursor-old
                   evil-operator-state-cursor evil-operator-state-cursor-old
                   evil-emacs-state-cursor evil-emacs-state-cursor-old))))

;;;###autoload
(defun +zone-switch-to-root-window-a (oldfun &rest args)
  (unless (minibufferp)
    (+misc-applications-with-cursor-hidden
     (let ((wconf (current-window-configuration)))
       (select-window (frame-root-window))
       (let ((ignore-window-parameters t))
         (delete-other-windows))
       (apply oldfun args)
       (set-window-configuration wconf)))))
