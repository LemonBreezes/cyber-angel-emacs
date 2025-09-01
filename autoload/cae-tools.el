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
(defun cae-handwritten-pdf-latex ()
  "Create handwritten PDF of current buffer using LaTeX with readable handwriting font."
  (interactive)
  (let* ((text (buffer-string))
         (temp-tex (make-temp-file "/tmp/emacs-handwritten-" nil ".tex"))
         (temp-pdf (concat (file-name-sans-extension temp-tex) ".pdf")))
    (with-temp-file temp-tex
      (insert "\\documentclass{article}\n")
      (insert "\\usepackage[T1]{fontenc}\n")
      (insert "\\usepackage{geometry}\n")
      (insert "\\geometry{margin=1in}\n")
      (insert "\\usepackage{mathpazo}\n")  ; Palatino font, readable and elegant
      (insert "\\usepackage{tgtermes}\n")   ; Times Roman alternative
      (insert "\\renewcommand{\\familydefault}{\\rmdefault}\n")
      (insert "\\linespread{1.15}\n")      ; Slightly increased line spacing for readability
      (insert "\\begin{document}\n")
      (insert (replace-regexp-in-string "[{}]" "\\\\\\&" text))
      (insert "\\end{document}\n"))
    (when (executable-find "pdflatex")
      (shell-command (format "cd /tmp && pdflatex -interaction=nonstopmode %s" (file-name-nondirectory temp-tex)))
      (find-file temp-pdf)
      (delete-file temp-tex)
      (delete-file (concat (file-name-sans-extension temp-tex) ".log"))
      (delete-file (concat (file-name-sans-extension temp-tex) ".aux")))
    (unless (executable-find "pdflatex")
      (error "pdflatex command not found"))))
