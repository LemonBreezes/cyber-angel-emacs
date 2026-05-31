;;; autoload/cae-hacks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-persp-buffer-visible-in-window-p (buffer persp)
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
                        (when (and (consp wc) (not found))
                          (cond
                           ((and (consp (car wc)) (eq (caar wc) 'buffer)
                                 (string= (buffer-name buffer) (cadadr wc)))
                            (setq found t))
                           (t
                            ;; Use a safer iteration method that checks each element
                            (let ((items (if (proper-list-p wc) wc nil)))
                              (when items
                                (dolist (w items)
                                  (when (and (consp w) (not found))
                                    (funcall search-in-wconf w)))))))))))
              (when (consp window-conf)
                (funcall search-in-wconf window-conf))
              found))))))))
