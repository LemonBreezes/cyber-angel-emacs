
;;; lisp/cae-evil.el -*- lexical-binding: t; -*-
;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(map! :desc "<leader>" :nmv "DEL" #'doom-leader-map
      :desc "<leader>" :nmv "<backspace>" #'doom-leader-map)

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
  "C-z" nil)

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

(after! evil-easymotion
  ;; This hack fixes `evilem-motion-next-line' on my Emacs. I think that command
  ;; breaks because I'm using Emacs 30.
  (evilem-make-motion
   evilem-motion-forward-line #'forward-line
   :pre-hook (setq evil-this-type 'line)
   :bind ((temporary-goal-column (current-column))
          (line-move-visual nil)))
  (advice-add #'evilem-motion-next-line :override #'evilem-motion-forward-line))

;; Jump to the end of the prompt when entering insert state in a terminal.
(add-hook 'evil-insert-state-entry-hook
          (cae-defun cae-goto-end-of-prompt-h ()
            (cond ((and (bound-and-true-p comint-last-prompt)
                        (not (eq (point) (point-max))))
                   (goto-char (point-max)))
                  ;; eshell
                  ((and (bound-and-true-p eshell-last-output-end)
                        (not (eq (point) eshell-last-output-end)))
                   (goto-char eshell-last-output-end)))))


;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
