;;; autoload/cae-tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ping-status ()
  (interactive)
  (let ((buffer (generate-new-buffer "*internet*")))
    (make-process
     :name "internet"
     :connection-type 'pipe
     :buffer buffer
     :command (list "ping" "-q" "-c" "1" "8.8.8.8")
     :sentinel `(lambda (p e)
                  (with-current-buffer ',buffer
                    (goto-char (point-min))
                    (if (or (search-forward "unreachable" nil t)
                            (search-forward "errors" nil t))
                        (progn (unless (string-match "ping-status" (format "%s" timer-list))
                                 (cae-run-with-timer 5 5 "cae-ping-status"
                                                     'cae-ping-status)
                                 (message "no internet")))
                      (when (string-match "ping-status" (format "%s" timer-list))
                        (cancel-function-timers 'ping-status))
                      (message "internet working"))
                    (kill-buffer))))))

;;;###autoload
(defun cae-handwritten-pdf-imagemagick ()
  "Create handwritten PDF of current buffer using ImageMagick."
  (interactive)
  (let* ((text (buffer-string))
         (temp-txt (make-temp-file "/tmp/emacs-handwritten-" nil ".txt"))
         (temp-pdf (concat (file-name-sans-extension temp-txt) ".pdf"))
         (magick-cmd (if (executable-find "magick") "magick" "convert"))
;; Get just the font family name, not the full path
         (font-name (or (car (seq-filter (lambda (f) (string-match-p "comic\\|sans\\|handwriting" f)) 
                                        (split-string (shell-command-to-string "fc-list :family") "\n")))
                       "DejaVu-Sans"))
         ;; Clean up font name to get just the family
         (clean-font (if (string-match "^\\([^:]+\\)" font-name)
                         (match-string 1 font-name)
                       "DejaVu-Sans")))
    (with-temp-file temp-txt
      (insert text))
    (when (executable-find magick-cmd)
      (let ((cmd (format "%s -font \"%s\" -pointsize 16 -fill black -size 800x600 caption:@%s %s" 
                         magick-cmd clean-font temp-txt temp-pdf)))
        (message "Running: %s" cmd)
        (shell-command cmd))
      (if (file-exists-p temp-pdf)
          (find-file temp-pdf)
        (error "PDF creation failed"))
      (delete-file temp-txt))
    (unless (executable-find magick-cmd)
      (error "ImageMagick 'magick' or 'convert' command not found"))))

;;;###autoload
(defun cae-handwritten-pdf-latex ()
  "Create handwritten PDF of current buffer using LaTeX with calligra font."
  (interactive)
  (let* ((text (buffer-string))
         (temp-tex (make-temp-file "/tmp/emacs-handwritten-" nil ".tex"))
         (temp-pdf (concat (file-name-sans-extension temp-tex) ".pdf")))
    (with-temp-file temp-tex
      (insert "\\documentclass{article}\n")
      (insert "\\usepackage[T1]{fontenc}\n")
      (insert "\\usepackage{calligra}\n")
      (insert "\\usepackage{geometry}\n")
      (insert "\\geometry{margin=1in}\n")
      (insert "\\renewcommand{\\familydefault}{\\sfdefault}\n")
      (insert "\\begin{document}\n")
      (insert "{\\calligra\n")
      (insert (replace-regexp-in-string "[{}]" "\\\\\\&" text))
      (insert "\n}\n")
      (insert "\\end{document}\n"))
    (when (executable-find "pdflatex")
      (shell-command (format "cd /tmp && pdflatex -interaction=nonstopmode %s" (file-name-nondirectory temp-tex)))
      (find-file temp-pdf)
      (delete-file temp-tex)
      (delete-file (concat (file-name-sans-extension temp-tex) ".log"))
      (delete-file (concat (file-name-sans-extension temp-tex) ".aux")))
    (unless (executable-find "pdflatex")
      (error "pdflatex command not found"))))

;;;###autoload
(defun cae-handwritten-pdf-ghostscript ()
  "Create handwritten PDF of current buffer using Ghostscript with custom font."
  (interactive)
  (let* ((text (buffer-string))
         (temp-ps (make-temp-file "/tmp/emacs-handwritten-" nil ".ps"))
         (temp-pdf (concat (file-name-sans-extension temp-ps) ".pdf")))
    (with-temp-file temp-ps
      (insert "%!PS-Adobe-3.0\n")
      (insert "%%Creator: Emacs\n")
      (insert "%%Pages: 1\n")
      (insert "%%EndComments\n")
      (insert "<< /PageSize [612 792] >> setpagedevice\n")
      (insert "/Helvetica findfont 16 scalefont setfont\n")
      (insert "72 700 moveto\n")
      (insert (format "(%s) show\n" (replace-regexp-in-string "[()]" "\\\\\\&" text)))
      (insert "showpage\n")
      (insert "%%EOF\n"))
    (when (executable-find "gs")
      (shell-command (format "gs -q -dNOPAUSE -dBATCH -sOutputFile=%s -sDEVICE=pdfwrite %s -c quit" temp-pdf temp-ps))
      (find-file temp-pdf)
      (delete-file temp-ps))
    (unless (executable-find "gs")
      (error "Ghostscript 'gs' command not found"))))
