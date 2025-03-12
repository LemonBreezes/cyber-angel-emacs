;;; autoload/cae-hacks.el -*- lexical-binding: t; -*-

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
                            (dolist (w wc)
                              (when (and (consp w) (not found))
                                (funcall search-in-wconf w)))))))))
              ;; Add a type check before calling search-in-wconf
              (when (consp window-conf)
                (funcall search-in-wconf window-conf))
              found))))))))


;;;###autoload
(defun cae-persp-kill-buffer-query-function ()
  "This must be the last hook in the `kill-buffer-query-functions'.
Otherwise if next function in the list returns nil -- the buffer will not be
killed, but just removed from a perspective(s)."
  (if (and persp-mode (not *persp-pretend-switched-off*))
      (let ((buffer (current-buffer)))
        (if (persp--buffer-in-persps buffer)
            (let* ((persp (get-current-persp))
                   (foreign-check
                    (if (and persp
                             (persp-contain-buffer-p buffer persp))
                        'not-foreign
                      (persp--kill-buffer-query-function-foreign-check
                       persp buffer))))
              (cl-case foreign-check
                (kill
                 (let (persp-autokill-buffer-on-remove)
                   (persp--remove-buffer-2 nil buffer))
                 t)
                (not-foreign
                 (if (persp-buffer-in-other-p* buffer persp)
                     (let ((visible-in-other-persps nil))
                       ;; Check if buffer is visible in any other perspective's window config
                       (dolist (other-persp-name (persp-names))
                         (let ((other-persp (persp-get-by-name other-persp-name)))
                           (when (and other-persp
                                      (not (eq other-persp persp))
                                      (persp-contain-buffer-p buffer other-persp)
                                      (cae-persp-buffer-visible-in-window-p buffer other-persp))
                             (setq visible-in-other-persps t))))

                       ;; If buffer is not visible in any other perspective, allow killing it
                       (if visible-in-other-persps
                           (progn
                             (persp--remove-buffer-2 persp buffer)
                             nil)
                         t))
                   (if (or (not (buffer-live-p buffer))
                           (persp--buffer-in-persps buffer))
                       nil
                     t)
                   t))
                (t
                 nil)))
          t))
    t))
