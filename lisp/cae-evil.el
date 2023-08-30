;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))

;; Bind `better-jumper-jump-forward' to TAB in terminal Emacs since can't dicern
;; between TAB and C-i there.
(unless (cae-display-graphic-p)
  (define-key evil-motion-state-map (kbd "TAB") nil)
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

;; Restore these Vim keybindings which Doom overrides.
(map! :i "C-e" #'evil-copy-from-below)

;; Define a leader key for switching to popup windows.
(unless (lookup-key evil-window-map "e")
  (map! :map evil-window-map
        "e" #'+popup/other))
(unless (lookup-key evil-window-map "~")
  (map! :map evil-window-map
        "~" #'+popup/raise))

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

(after! evil-embrace
  (setq evil-embrace-show-help-p t)
  (cl-pushnew ?\C-f evil-embrace-evil-surround-keys)
  ;; I prefer using `C-f' for prefix functions everywhere over a DWIM-style `f'.
  (remove-hook! (lisp-mode emacs-lisp-mode clojura-mode racket-mode hy-mode)
    #'+evil-embrace-lisp-mode-hook-h))

(use-package! evil-owl
  :hook (doom-first-input . evil-owl-mode))

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

(map! :map help-map "bn" #'cae-show-normal-state-bindings)

(map! :prefix "C-x"
      :i "C-c" #'copilot-complete
      :i "C-f" #'cape-file
      :i "C-s" #'yasnippet-capf
      :i "C-l" #'cape-line
      :i "C-d" (cape-interactive-capf (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
