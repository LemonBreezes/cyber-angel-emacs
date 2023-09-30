;;; private/misc-applications/trash/emms-mode-line-cycle-variable-pitch.el -*- lexical-binding: t; -*-

;;(defvar emms-mode-line-string-pixel-length-max-alist nil)
;;;;;###autoload
;;(defun +emms-mode-line-cycle-valign (&rest _)
;;  (let* ((song (or emms-mode-line-cycle--title
;;                   (funcall emms-mode-line-cycle-current-title-function))))
;;    (if (or (not emms-mode-line-string)
;;            (> (length emms-mode-line-string)
;;               (+ (length (string-replace "%s" "" emms-mode-line-format))
;;                  (min (length song)
;;                       emms-mode-line-cycle-max-width))))
;;        (and emms-mode-line-string
;;             (setq emms-mode-line-string
;;                   (replace-regexp-in-string "\\s-+" " " emms-mode-line-string)))
;;      (let* ((suffix (cadr (split-string emms-mode-line-format "%s")))
;;             (width (cae-variable-pitch-width emms-mode-line-string))
;;             (l (length emms-mode-line-string))
;;             (padding (max (- (max (setf (alist-get l emms-mode-line-string-pixel-length-max-alist)
;;                                         (max (alist-get l emms-mode-line-string-pixel-length-max-alist 0)
;;                                              width))
;;                                   (puthash song
;;                                            (max (or (gethash song emms-mode-line-song-pixel-length-max-hash-table)
;;                                                     0)
;;                                                 width)
;;                                            emms-mode-line-song-pixel-length-max-hash-table))
;;                              width 0))))
;;        (setq emms-mode-line-string
;;              (concat (string-remove-suffix suffix emms-mode-line-string)
;;                      (propertize " "
;;                                  'display `(space :width (,padding)))
;;                      suffix))))))


(defvar emms-mode-line-song-max-pixel-width-hash (make-hash-table :test 'equal))
(defvar emms-mode-line-song-pixel-width-hash (make-hash-table :test 'equal))

(defun +emms-compute-modeline-cycle-pixel-width ()
  (or (gethash emms-mode-line-cycle--title
               emms-mode-line-song-max-pixel-width-hash)
      (puthash
       emms-mode-line-cycle--title
       (with-current-buffer (get-buffer-create " *+emms-compute-modeline-cycle-pixel-width*")
         (cl-do* ((output 0)
                  (n 0)
                  (cache-length (+ emms-mode-line-cycle--title-width
                                   emms-mode-line-cycle-additional-space-num))
                  (continue (< emms-mode-line-cycle-max-width cache-length))
                  (s (emms-mode-line-cycle--get-title-cache n)))
             ((not continue) output)
           (delete-region (point-min) (point-max))
           (propertize s 'line-prefix nil 'wrap-prefix nil 'face 'variable-pitch)
           (setq output (max (puthash s (car (buffer-text-pixel-size nil nil t))
                                      emms-mode-line-song-pixel-width-hash)
                             output)
                 n (1+ n)
                 s (emms-mode-line-cycle--get-title-cache n)
                 continue (< n cache-length))))
       emms-mode-line-song-max-pixel-width-hash)))

;;(let ((initial (emms-mode-line-cycle--get-title-cache))
;;      (n 1))
;;  (while (not (string= (emms-mode-line-cycle--get-title-cache n) initial))
;;    (setq n (1+ n)))
;;  (+log n ))

;;(+emms-compute-modeline-cycle-pixel-width)

;;(unless (gethash (emms-mode-line-cycle-get-title emms-mode-line-cycle-velocity)
;;                 emms-mode-line-song-pixel-width-hash)
;;  (+log (concat "|" (emms-mode-line-cycle-get-title emms-mode-line-cycle-velocity)
;;                "|")))

;;;###autoload
(defun +emms-mode-line-cycle-valign (&rest _)
  (when-let* ((suffix (cadr (split-string emms-mode-line-format "%s")))
              (max-width (+emms-compute-modeline-cycle-pixel-width))
              (song (emms-mode-line-cycle-get-title))
              (width (or (gethash song emms-mode-line-song-pixel-width-hash)
                         (puthash song
                                  (cae-variable-pitch-width song)
                                  emms-mode-line-song-pixel-width-hash)))
              (padding (- max-width width))
              (padding-nontrivial-p (> padding 0)))
    ;;(+log padding width (+emms-compute-modeline-cycle-pixel-width))
    (setq emms-mode-line-string
          (concat (string-remove-suffix suffix emms-mode-line-string)
                  (propertize " " 'display `(space :width (,padding)))
                  suffix))))

;;(setq emms-mode-line-string
;;      (replace-regexp-in-string "\\s-+" " " emms-mode-line-string))
;;(- (+emms-compute-modeline-cycle-pixel-width
;;    (or emms-mode-line-cycle--title
;;        (funcall emms-mode-line-cycle-current-title-function)))
;;   (cae-variable-pitch-width emms-mode-line-string))
;;(length (or emms-mode-line-cycle--title
;;            (funcall emms-mode-line-cycle-current-title-function)))
