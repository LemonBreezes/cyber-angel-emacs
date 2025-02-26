;;; cae/eshell/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;;; Core Settings
;; Basic eshell configuration and module setup

;; Add electric slash module for path completion
(after! esh-module
  (add-to-list 'eshell-modules-list 'eshell-elecslash))
(autoload 'eshell-elecslash-initialize "em-elecslash")
(add-hook 'eshell-mode-hook #'eshell-elecslash-initialize)

;; Expand abbreviations before parsing input
(defadvice! cae-eshell-expand-abbrev-a (&rest _)
  :before #'eshell-send-input
  (expand-abbrev))

;; Don't leave unbalanced delimiters when killing input
(defadvice! cae-eshell-kill-input-with-delimiters-a ()
  :after #'eshell-kill-input
  (let ((beg (pos-bol))
        (end (pos-eol)))
    (while (and (not (eq (point) end))
                (condition-case _
                    (scan-sexps beg end)
                  (scan-error t)))
      (delete-char 1)))
  (unless (eq (char-syntax (char-before)) ?\s)
    (insert-char ?\s)))

;;; Terminal Integration (eat)
;; Configuration for the Emacs Advanced Terminal

(use-package! eat
  :defer t 
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :config
  ;; Do not let EAT override TERM
  (setq eat-term-name (lambda () eshell-term-name)
        eat-enable-yank-to-terminal t)

  ;; Temporarily disable some modes when EAT is active
  (add-hook 'eat--eshell-process-running-mode-hook
            (defun cae-eshell-disable-modes-in-eat-h ()
              (let ((modes '(corfu-mode eldoc-mode)))
                (dolist (mode modes)
                  (when (boundp mode)
                    (let ((mode-var (intern (concat "cae-eshell--" (symbol-name mode) "-enabled-p"))))
                      (make-local-variable mode-var)
                      (if eat--eshell-process-running-mode
                          (progn (set mode-var (symbol-value mode))
                                 (funcall mode -1))
                        (when (symbol-value mode-var)
                          (funcall mode 1)
                          (set mode-var nil)))))))))


  ;; Add hint for exiting char mode
  (defadvice! cae-eat-eshell-print-char-mode-hint-a ()
    :after #'eat-eshell-char-mode
    (message "Type M-RET/C-M-m to exit char mode.")))

;;; Command Handling
;; Command execution and redirection

;; Restore original emacs command behavior
(when (symbol-function #'eshell/emacs)
  (setf (symbol-function #'eshell/e)
        (symbol-function #'eshell/emacs))
  (unintern 'eshell/emacs obarray))

;; Colorize ansi escape sequences in exported buffers
(advice-add #'eshell-output-object-to-target :around #'cae-eshell-ansi-buffer-output)

;; Parse buffer redirection >#buf and >#.
(after! esh-arg
  (add-hook 'eshell-parse-argument-hook #'cae-eshell-syntax-buffer-redirect))

;;; History Management
;; History filtering and configuration

(after! em-hist
  (setq eshell-history-size nil
        eshell-history-append t
        eshell-hist-ignoredups 'erase)
  (add-to-list 'eshell-expand-input-functions
               #'eshell-expand-history-references))

;; Filter trivial commands from history
(defun cae-eshell-input-filter (str)
  "Filter some trivial commands from the input history."
  (not (or (string-blank-p str)
           (equal "cd" str)
           (string-prefix-p "cd " str)
           (string-prefix-p " "