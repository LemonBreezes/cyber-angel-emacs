;;; cae/misc-applications/autoload/zone.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-zone-pgm-md5 ()
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
(defun cae-zone-choose (pgm)
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

;;;###autoload
(defun cae-zone-switch-to-root-window-a (oldfun &rest args)
  (unless (minibufferp nil t)
    (cae-misc-applications-with-cursor-hidden
     (let ((wconf (current-window-configuration)))
       (select-window (car (doom-visible-windows)))
       (let ((ignore-window-parameters t))
         (delete-other-windows))
       (apply oldfun args)
       (set-window-configuration wconf)))))

