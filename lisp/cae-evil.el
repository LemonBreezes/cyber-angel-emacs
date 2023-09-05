;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))

;; Bind `better-jumper-jump-forward' to TAB in terminal Emacs since can't dicern
;; between TAB and C-i there.
(unless (cae-display-graphic-p)
  (after! evil
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (add-hook! (prog-mode conf-mode text-mode)
      (evil-local-set-key 'motion (kbd "TAB") #'better-jumper-jump-forward))))

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
(after! evil
  (unless (lookup-key evil-window-map "e")
    (map! :map evil-window-map
          "e" #'+popup/other))
  (unless (lookup-key evil-window-map "~")
    (map! :map evil-window-map
          "~" #'+popup/raise)))

;; Isearch is better in `Info-mode'
(after! info
  (map! :map Info-mode-map
        :m "/" #'isearch-forward-regexp
        :m "?" #'isearch-backward-regexp))

(when (modulep! :editor evil +hybrid)
  ;; Return Isearch to Evil
  (map! :m "C-r" #'isearch-backward
        :m "C-s" #'isearch-forward
        :n "C-r" nil
        :n "U" #'evil-redo)

  ;; Use Emacs keybindings in Evil insert state.
  (setq evil-disable-insert-state-bindings nil)
  (after! evil
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (define-key evil-insert-state-map
      (read-kbd-macro evil-toggle-key) 'evil-emacs-state))
  (define-key! :keymaps +default-minibuffer-maps
    "C-a" nil
    "C-r" nil
    "C-u" nil
    "C-v" nil
    "C-w" nil
    "C-z" nil))

(after! evil-easymotion
  (map! :map evilem-map
        "RET" #'cae-avy-embark-act-on-region
        "TAB" #'avy-goto-word-1))

(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

(after! evil-snipe
  (setq evil-snipe-scope 'visible))
(setq evil-ex-substitute-global t
      evil-move-cursor-back nil
      evil-vsplit-window-right t
      evil-split-window-below t
      evil-v$-excludes-newline t)

(remove-hook! (lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
  #'+evil-embrace-lisp-mode-hook-h)
(defadvice! cae-embrace-init-pairs-a (&rest args)
  :after #'embrace--setup-defaults
  (embrace-add-pair-regexp ?\C-f "([^ ]+ " ")" 'embrace-with-prefix-function
                         (embrace-build-help "(function " ")")))
(after! evil-embrace
  (setq evil-embrace-show-help-p t))

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

(after! comint
  (map! :map comint-mode-map
        :i "C-d" #'cae-comint-delchar-or-maybe-eof))
(after! evil-collection
  (setq evil-collection-key-blacklist
        (append (when (modulep! :config default)
                  (list doom-leader-key doom-localleader-key
                        doom-leader-alt-key))
                (when (modulep! :editor multiple-cursors)
                  '("gz"))
                '("<escape>" "[" "]")))
  (map! :n "]p" #'cae-unimpaired-paste-below
        :n "[p" #'cae-unimpaired-paste-above
        :n "] DEL" #'+evil/insert-newline-below
        :n "[ DEL" #'+evil/insert-newline-above
        :m "[6" #'cae-unimpaired-b64-encode
        :m "]6" #'cae-unimpaired-b64-decode))
(setq evil-org-key-theme '(navigation insert textobjects
                           additional calendar shift todo heading))

(after! help
  (map! :map help-map "bn" #'cae-show-normal-state-bindings))
(when (modulep! :private corfu)
  (map! (:prefix "C-x"
         :i "C-c" #'copilot-complete
         :i "C-f" #'cape-file
         :i "C-s" #'yasnippet-capf
         :i "C-l" #'cape-line
         :i "C-d" (cape-interactive-capf (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword))
         :i "C-n" nil
         :i "C-p" nil)))

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
