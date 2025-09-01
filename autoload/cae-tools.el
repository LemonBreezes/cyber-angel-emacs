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
  "Create handwritten PDF of current buffer using LaTeX with readable handwriting font.
Supports whitespace markup:
  [[vspace:1cm]] - adds 1cm vertical space
  [[smallskip]] - adds small vertical space
  [[medskip]] - adds medium vertical space
  [[bigskip]] - adds large vertical space"
  (interactive)
  (let* ((text (buffer-string))
         (temp-tex (make-temp-file "/tmp/emacs-handwritten-" nil ".tex"))
         (temp-pdf (concat (file-name-sans-extension temp-tex) ".pdf"))
         ;; Process whitespace markup
         (processed-text (replace-regexp-in-string 
                          "\\[\\[vspace:\\([^]]+\\)\\]\\]" 
                          "\\\\vspace{\\1}" 
                          (replace-regexp-in-string 
                           "\\[\\[smallskip\\]\\]" 
                           "\\\\smallskip"
                           (replace-regexp-in-string 
                            "\\[\\[medskip\\]\\]" 
                            "\\\\medskip"
                            (replace-regexp-in-string 
                             "\\[\\[bigskip\\]\\]" 
                             "\\\\bigskip"
                             text))))))
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
      (insert (replace-regexp-in-string "[{}]" "\\\\\\&" processed-text))
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
(defun cae-insert-latex-whitespace (type)
  "Insert LaTeX whitespace markup at point.
TYPE can be 'vspace, 'smallskip, 'medskip, or 'bigskip."
  (interactive (list (completing-read "Whitespace type: " 
                                      '("vspace" "smallskip" "medskip" "bigskip"))))
  (cond
   ((string= type "vspace")
    (let ((amount (read-string "Amount (e.g., 1cm, 0.5in): " "1cm")))
      (insert (format "[[vspace:%s]]" amount))))
   (t
    (insert (format "[[%s]]" type)))))
