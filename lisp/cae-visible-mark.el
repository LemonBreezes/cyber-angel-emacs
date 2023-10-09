;;; lisp/cae-visible-mark.el -*- lexical-binding: t; -*-

;; I copied this from somewhere and I don't remember where. But also, I prefer
;; this code to `visible-mark-mode'. I don't remember why I liked this solution
;; more.

(defface cae-visible-mark-face
  '((t :inherit match))
  "Face used for showing the mark's position.")

(defvar-local cae-visible-mark-overlay nil
  "The overlay for showing the mark's position.")

(defvar-local cae-visible-mark-is-mark-visible t
  "The overlay is visible only when this variable's value is t.")

(defun cae-visible-mark-draw-mark (&rest _)
  "Make the mark's position stand out by means of a one-character-long overlay.
   If the value of variable `cae-visible-mark-is-mark-visible' is nil, the mark will be
   invisible."
  (unless cae-visible-mark-overlay
    (setq cae-visible-mark-overlay (make-overlay 0 0 nil t))
    (overlay-put cae-visible-mark-overlay 'face 'cae-visible-mark-face))
  (let ((mark-position (mark t)))
    (cond
     ((null mark-position) (delete-overlay cae-visible-mark-overlay))
     ((and (< mark-position (point-max))
           (not (eq ?\n (char-after mark-position))))
      (overlay-put cae-visible-mark-overlay 'after-string nil)
      (move-overlay cae-visible-mark-overlay mark-position (1+ mark-position)))
     (t
                                        ; This branch is called when the mark is at the end of a line or at the
                                        ; end of the buffer. We use a bit of trickery to avoid the higlight
                                        ; extending from the mark all the way to the right end of the frame.
      (overlay-put cae-visible-mark-overlay 'after-string
                   (propertize " " 'face (overlay-get cae-visible-mark-overlay 'face)))
      (move-overlay cae-visible-mark-overlay mark-position mark-position)))))

(add-hook 'pre-redisplay-functions #'cae-visible-mark-draw-mark)

(defun cae-visible-mark-toggle-mark-visibility ()
  "Toggles the mark's visiblity and redraws it (whether invisible or visible)."
  (interactive)
  (setq cae-visible-mark-is-mark-visible (not cae-visible-mark-is-mark-visible))
  (if cae-visible-mark-is-mark-visible
      (set-face-attribute 'cae-visible-mark-face nil :background (face-background 'match nil t) :foreground "white")
    (set-face-attribute 'cae-visible-mark-face nil :background 'unspecified :foreground 'unspecified))
  (cae-visible-mark-draw-mark))
