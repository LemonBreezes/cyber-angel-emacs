;;; completion/corfu/+yas-capf.el -*- lexical-binding: t; -*-

;;
;;; Capf for yasnippet. Inspired by @elken's yas-capf.
(defcustom yas-capf-lookup-by 'key
  "The method used to lookup candidates."
  :type '(choice
          (const :tag "Key" key)
          (const :tag "Name" name))
  :group 'yasnippet)

(defun yas-capf--exit (_ status)
  "Actually expand the snippet, if STATUS is \"finished\"."
  (when (string= "finished" status)
    (yas-expand)))

(defconst yas-capf--buffer-name "*yas-capf expansion*")

(defun yas-capf--get-doc (cand)
  (when-let ((template (get-text-property 0 'yas-template cand))
             (maj-mode major-mode)
             (buf (get-buffer-create yas-capf--buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (funcall maj-mode)
      (yas-minor-mode)
      (save-excursion
        (yas-expand-snippet template)
        (when (yas-active-snippets)
          (let ((i 0))
            (while (not (yas-next-field-will-exit-p 1))
              (setq i (+ i 1))
              (insert (format "<%d>" i))
              (yas-next-field)))
          (insert "<END>"))
        (font-lock-ensure))
      (insert "Expands to:" ?\n ?\n)
      (current-buffer))))

(defconst yas-capf--properties
  (list :annotation-function (lambda (_) "(Snippet)")
        :company-kind (lambda (_) 'snippet)
        :company-doc-buffer #'yas-capf--get-doc
        :exit-function #'yas-capf--exit
        :exclusive 'no))
"Return a list of extra properties for text at BEG through END."

(defun yas-capf--list (input)
  "Use INPUT to compute and filter the initial table."
  (when-let* ((templates (yas--all-templates (yas--get-snippet-tables major-mode)))
              (special-req (yas--require-template-specific-condition-p))
              (init-table (mapcar
                           (lambda (template)
                             (let ((key (yas--template-key template))
                                   (name (yas--template-name template))
                                   (condition (yas--template-condition template)))
                               (when (and (string-prefix-p input key)
                                          (yas--template-can-expand-p condition special-req))
                                 (let ((key-or-name (pcase yas-capf-lookup-by
                                                      ('key key)
                                                      ('name name))))
                                   (propertize key-or-name
                                               'yas-template template)))))
                           templates)))
    (cons (apply-partially #'string-prefix-p input)
          init-table)))

(defun yas-capf (&optional interactive)
  "Complete with yasnippet at point.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive #'yas-capf)
    (when (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+")
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        `(,beg ,end
          ,(cape--table-with-properties
            (cape--cached-table beg end #'yas-capf--list)
            :category 'yasnippet)
          ,@yas-capf--properties)))))
