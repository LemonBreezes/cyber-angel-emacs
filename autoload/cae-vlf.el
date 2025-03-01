;;; autoload/cae-vlf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-files--ask-about-large-file-vlf (size op-type filename offer-raw)
  "Like `files--ask-user-about-large-file', but with support for `vlf'."
  (require 'vlf-setup)
  (if (eq vlf-application 'dont-ask)
      (progn (vlf filename) (error ""))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) op-type)))
      (if (not offer-raw)
          (if (y-or-n-p prompt) nil 'abort)
        (let ((choice
               (car
                (read-multiple-choice
                 prompt '((?y "yes")
                          (?n "no")
                          (?l "literally")
                          (?v "vlf"))
                 (files--ask-user-about-large-file-help-text
                  op-type (funcall byte-count-to-string-function size))))))
          (cond ((eq choice ?y) nil)
                ((eq choice ?l) 'raw)
                ((eq choice ?v)
                 (vlf filename)
                 (error ""))
                (t 'abort)))))))

;;;###autoload
(defun cae-vlf-update-linum ()
  "Update the line number offset."
  (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            +vlf-cumulative-linenum))))

;;;###autoload
(defun cae-vlf-next-chunk-or-start ()
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))

;;;###autoload
(defun cae-vlf-last-chunk-or-end ()
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))
