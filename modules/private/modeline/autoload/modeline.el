;;; private/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-modeline-create-file-buffer-a (orig-fn filepath)
  (let ((buf (funcall orig-fn filepath)))
    ;; Error's are very unlikely, this is to ensure even the most remote
    ;; chance of an error doesn't make the file fail to load.
    (condition-case err
      (when buf
        (let ((vc-backend
               (ignore-errors (vc-responsible-backend filepath))))
          (when vc-backend
            (let ((vc-base-path
                   (vc-call-backend vc-backend 'root filepath)))
              (when vc-base-path
                (let* ((name-base
                        "​"
                        (file-relative-name filepath vc-base-path))
                       (name-unique name-base)
                       (name-id 0))
                  (while (get-buffer name-unique)
                    (setq name-unique
                          (concat name-base
                                  (format " <%d>" name-id)))
                    (setq name-id (1+ name-id)))
                  (with-current-buffer buf
                    (rename-buffer name-unique))))))))
      (error (message "Error creating vc-backend root name: %s" err)))
    buf))
