;;; lisp/cae-visible-mark.el -*- lexical-binding: t; -*-

(defface mmv-face
  '((t :inherit 'match :foreground "white"))
  "Face used for showing the mark's position.")

(defvar-local mmv-mark-overlay nil
  "The overlay for showing the mark's position.")

(defvar-local mmv-is-mark-visible t
  "The overlay is visible only when this variable's value is t.")

(defun mmv-draw-mark (&rest _)
  "Make the mark's position stand out by means of a one-character-long overlay.
   If the value of variable `mmv-is-mark-visible' is nil, the mark will be
   invisible."
  (unless mmv-mark-overlay
    (setq mmv-mark-overlay (make-overlay 0 0 nil t))
    (overlay-put mmv-mark-overlay 'face 'mmv-face))
  (let ((mark-position (mark t)))
    (cond
     ((null mark-position) (delete-overlay mmv-mark-overlay))
     ((and (< mark-position (point-max))
           (not (eq ?\n (char-after mark-position))))
      (overlay-put mmv-mark-overlay 'after-string nil)
      (move-overlay mmv-mark-overlay mark-position (1+ mark-position)))
     (t
                                        ; This branch is called when the mark is at the end of a line or at the
                                        ; end of the buffer. We use a bit of trickery to avoid the higlight
                                        ; extending from the mark all the way to the right end of the frame.
      (overlay-put mmv-mark-overlay 'after-string
                   (propertize " " 'face (overlay-get mmv-mark-overlay 'face)))
      (move-overlay mmv-mark-overlay mark-position mark-position)))))

(add-hook 'pre-redisplay-functions #'mmv-draw-mark)

;;(defun mmv-toggle-mark-visibility ()
;;  "Toggles the mark's visiblity and redraws it (whether invisible or visible)."
;;  (interactive)
;;  (setq mmv-is-mark-visible (not mmv-is-mark-visible))
;;  (if mmv-is-mark-visible
;;      (set-face-attribute 'mmv-face nil :background (face-background 'match nil t) :foreground "white")
;;    (set-face-attribute 'mmv-face nil :background 'unspecified :foreground 'unspecified))
;;  (mmv-draw-mark))
