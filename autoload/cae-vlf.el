;;; autoload/cae-vlf.el -*- lexical-binding: t; -*-

(defvar cae-vlf-cumulative-linenum nil
  "Alist mapping file positions to cumulative line numbers.")

;;;###autoload
(defun cae-files--ask-about-large-file-vlf (size op-type filename offer-raw)
  "Handle large file opening with VLF support.
Similar to `files--ask-user-about-large-file', but adds VLF as an option.

SIZE is the file size in bytes.
OP-TYPE is a string describing the operation (e.g., \"open\").
FILENAME is the name of the file being processed.
OFFER-RAW if non-nil, offer a \"literal\" option."
  (require 'vlf-setup)
  (if (eq vlf-application 'dont-ask)
      (progn 
        (vlf filename) 
        (error "Opened file in VLF mode"))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) 
                          op-type)))
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
                 (error "Opened file in VLF mode"))
                (t 'abort)))))))

;;;###autoload
(defun cae-vlf-update-linum ()
  "Update line number offset for VLF mode.
Calculates and sets the appropriate line number offset based on
the current chunk position in the file."
  (let ((linenum-offset (alist-get vlf-start-pos cae-vlf-cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos cae-vlf-cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            cae-vlf-cumulative-linenum))))

;;;###autoload
(defun cae-vlf-next-chunk-or-start ()
  "Navigate to the next chunk or to the start if at the end.
If at the end of the file, jump to the first chunk, otherwise
move to the next batch of data."
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))

;;;###autoload
(defun cae-vlf-last-chunk-or-end ()
  "Navigate to the previous chunk or to the end if at the start.
If at the beginning of the file, jump to the end, otherwise
move to the previous batch of data."
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))
