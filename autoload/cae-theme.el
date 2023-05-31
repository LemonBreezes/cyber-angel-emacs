;;; autoload/cae-theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-theme-customize-faces-h (theme)
  (after! org
    (set-face-attribute 'org-ellipsis nil
                        :inherit '(shadow default)
                        :weight 'normal)
    (set-face-attribute 'org-document-title nil
                        :height 1.2))
  (after! company
    (set-face-attribute 'company-preview-common nil
                        :inherit 'shadow
                        :background 'unspecified)
    (set-face-attribute 'company-preview nil
                        :inherit 'shadow
                        :background 'unspecified))
  (after! lsp-ui
    (set-face-attribute 'lsp-ui-doc-background nil
                        :background (face-background 'tooltip))))
