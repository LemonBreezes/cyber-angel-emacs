
;;; lisp/cae-evil.el -*- lexical-binding: t; -*-
;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(map! :map general-override-mode-map
      :desc "<leader>" :nmv "DEL" #'doom-leader-map
      :desc "<leader>" :nmv "<backspace>" #'doom-leader-map)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))

;; Bind `better-jumper-jump-forward' to TAB in terminal Emacs since can't dicern
;; between TAB and C-i there.
(unless (cae-display-graphic-p)
  (add-hook! (prog-mode conf-mode text-mode)
    (evil-local-set-key 'motion (kbd "TAB") #'better-jumper-jump-forward)))

;; Restore Emacs keybindings which Doom overrides. `expand-region-fast-keys'
;; and `C-x C-=' make these keybindings redundant.
(map! "C--" #'negative-argument
      "M--" #'negative-argument
      "M-=" #'count-words-region)

;; Unbind text scaling functions. We use C-x C-=.
(map! :n "C--" nil
      :n "C-+" nil
      :n "C-=" nil
      :n "M-C-=" nil
      :n "M-C--" nil)

;; Define a leader key for switching to popup windows.
(unless (lookup-key evil-window-map "e")
  (map! :map evil-window-map
        "e" #'+popup/other))

;; Isearch is better in `Info-mode'
(map! :map Info-mode-map
      :m "/" #'isearch-forward-regexp
      :m "?" #'isearch-backward-regexp)

(when (modulep! :editor evil +hybrid)
  ;; Return Isearch to Evil
  (map! :m "C-r" #'isearch-backward
        :m "C-s" #'isearch-forward
        :n "C-r" nil
        :n "U" #'evil-redo)

  ;; Use Emacs keybindings in Evil insert state.
  (setq evil-disable-insert-state-bindings nil
        evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key! :keymaps +default-minibuffer-maps
    "C-a" nil
    "C-r" nil
    "C-u" nil
    "C-v" nil
    "C-w" nil
    "C-z" nil))

(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

;; ;; Unmap `C-d` and `C-u` in Evil since we use the `<prior>` and `<next>` keys
;; ;; instead.
;;(map! :m "C-d" nil
;;      :m "C-u" nil)

(after! evil-snipe
  (setq evil-snipe-scope 'visible))
(setq evil-ex-substitute-global t
      evil-move-cursor-back nil
      evil-vsplit-window-right t
      evil-split-window-below t)

;; Jump to the end of the prompt when entering insert state in a terminal.
(add-hook 'evil-insert-state-entry-hook
          (cae-defun cae-goto-end-of-prompt-h ()
            (cond ((and (bound-and-true-p comint-last-prompt)
                        (not (eq (point) (point-max))))
                   (goto-char (point-max)))
                  ;; eshell
                  ((and (bound-and-true-p eshell-last-output-end)
                        (not (>= (point) eshell-last-output-end)))
                   (goto-char (point-max))))))

;; God mode support
;;(after! god-mode
;;  (which-key-enable-god-mode-support)
;;  (setq god-mode-enable-function-key-translation nil
;;        god-mode-alist '((nil . "C-") ("g" . "M-") ("G" . "C-M-")))
;;  (setf (alist-get 'escape god-mode-sanitized-key-alist)
;;        "<escape>")
;;  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
;;  (global-set-key (kbd "C-x C-2") #'split-window-below)
;;  (global-set-key (kbd "C-x C-3") #'split-window-right)
;;  (global-set-key (kbd "C-x C-0") #'delete-window))
;;(map! :n "s" #'evil-execute-in-god-state
;;      :map god-local-mode-map
;;      "C-h C-k" #'god-mode-describe-key)
;;(after! evil-god-state
;;  (add-hook 'post-command-hook
;;            (cae-defun cae-god-state-exit ()
;;              (run-at-time
;;               0.0 nil
;;               (lambda ()
;;                 (unless (or (not (eq evil-state 'god))
;;                             (eq last-command #'evil-execute-in-god-state))
;;                   (evil-god-state-bail))))))
;;
;;  ;; Monkey-patch `evil-visual-state-activate-hook' to not clobber `god-state'
;;  ;; when the mark is active.
;;  (defun evil-visual-activate-hook (&optional _command)
;;    "Enable Visual state if the region is activated."
;;    (unless (evil-visual-state-p)
;;      (evil-delay nil
;;          ;; the activation may only be momentary, so re-check
;;          ;; in `post-command-hook' before entering Visual state
;;          '(unless (or (evil-visual-state-p)
;;                       (evil-insert-state-p)
;;                       (evil-emacs-state-p)
;;                       (evil-god-state-p))
;;             (when (and (region-active-p)
;;                        (not deactivate-mark))
;;               (evil-visual-state)))
;;        'post-command-hook nil t
;;        "evil-activate-visual-state")))
;;
;;  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))

(map! :map help-map "bn" #'cae-show-normal-state-bindings)

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
