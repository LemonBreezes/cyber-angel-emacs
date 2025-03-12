
;;; ~/.doom.d/autoload/cae-ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-apply-ansi-color-to-buffer-h ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(defun persp-buffer-visible-in-window-p (buffer persp)
  "Check if BUFFER is visible in any window in the saved window configurations of PERSP."
  (when (and (buffer-live-p buffer) persp)
    (let ((window-conf (persp-window-conf persp)))
      (when window-conf
        (cond
         ;; For workgroups.el or default window configs
         ((and persp-use-workgroups (fboundp 'wg-wconfig-buf-uids))
          (member (buffer-name buffer) (wg-wconfig-buf-uids window-conf)))
         ;; For window-state based configs
         ((consp window-conf)
          (let ((found nil))
            (letrec ((search-in-wconf
                      (lambda (wc)
                        (if (and (consp wc) (not found))
                            (cond
                             ((and (consp (car wc)) (eq (caar wc) 'buffer)
                                   (string= (buffer-name buffer) (cadadr wc)))
                              (setq found t))
                             (t
                              (dolist (w wc)
                                (when (and (consp w) (not found))
                                  (funcall search-in-wconf w)))))))))
              (funcall search-in-wconf window-conf)
              found))))))))
