;;; editor.el -*- lexical-binding: t; -*-

(load! "lisp/cae-repeat" doom-user-dir)
(load! "lisp/cae-vlf" doom-user-dir)
(load! "lisp/cae-restore-point" doom-user-dir)
(load! "lisp/cae-multiple-cursors" doom-user-dir)

;; `vimish-fold' persists folds by saving the overlay region `(point) (mark)'.
;; This is problematic because it means that a fold can be broken by an
;; external file change. Still, I use this for personal Org files and the
;; like.
(when (modulep! :editor fold)
  (add-hook 'doom-first-file-hook #'vimish-fold-global-mode)
  (setq vimish-fold-indication-mode 'right-fringe))

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Treat all themes as safe.
(setq custom-safe-themes t)

;; Skip some buffers with `previous-buffer'
(setq switch-to-prev-buffer-skip
      (lambda (_win buf _bury-or-kill)
        (or (get-buffer-window buf)
            (doom-unreal-buffer-p buf))))

(setq delete-active-region t)         ;makes `d' delete region in Meow.

;; Allow us to undo deleting frames.
(undelete-frame-mode +1)

;; Not sure if this does anything noticeable yet.
(setq jit-lock-defer-time 0)

;; BUG Enabling this can cause read-only errors when trying to copy text!
;;(when (fboundp #'kill-ring-deindent-mode)
;;  (add-hook 'doom-first-input-hook #'kill-ring-deindent-mode))

(after! paren
  ;; BUG NEVER TURN THIS VARIABLE ON. IT WILL BREAK LISPY, SMARTPARENS,
  ;; EVERYTHING!!!
  (setq show-paren-context-when-offscreen nil) ; <--
  (setq show-paren-ring-bell-on-mismatch nil))

(cae-advice-add #'doom/kill-this-buffer-in-all-windows :around #'doom-set-jump-a)
(cae-advice-add #'kill-this-buffer :around #'doom-set-jump-a)

;; Reduce error spam in scratch buffers.
(add-hook 'doom-scratch-buffer-hook #'+emacs-lisp--flycheck-non-package-mode)

(setq find-sibling-rules
      '(("\\([^/]+\\)\\.org\\'" "~/org/.archive/\\\\1.org\\'")))

;; Query buffers for a diff before killing them.
(defun cae-ask-kill-buffer ()
  "Ask to diff, save or kill buffer"
  (if (and (buffer-file-name)
           (buffer-modified-p))
      (let (cae-diff-window)
        (prog1
            (cl-loop
             for ch =
             (read-key "(k)ill buffer, (d)iff buffer, (s)ave buffer, (q)uit?")
             if (or (eq ch ?k) (eq ch ?K))
             return t
             if (or (eq ch ?d) (eq ch ?D))
             do (setq cae-diff-window (diff-buffer-with-file))
             if (or (eq ch ?s) (eq ch ?S))
             return (progn (save-buffer) t)
             if (memq ch '(?q ?Q))
             return nil)
          (when cae-diff-window
            (delete-window cae-diff-window)
            (setq cae-diff-window nil))))
    t))

(add-to-list 'kill-buffer-query-functions #'cae-ask-kill-buffer)

;; Automatically re-indent after commenting.
(cae-advice-add #'comment-or-uncomment-region :after #'indent-region)

;; Allow remembering risky variables.
(cae-advice-add 'risky-local-variable-p :override #'ignore)

;; Kill process buffers without asking.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Do not automatically continue comments.
(advice-remove #'newline-and-indent
               #'+default--newline-indent-and-continue-comments-a)

;; Pop mark multiple times with `C-u C-SPC C-SPC ...'.
(setq set-mark-command-repeat-pop t)

(setq search-whitespace-regexp ".*?"
      search-default-mode #'char-fold-to-regexp
      isearch-lax-whitespace t
      isearch-wrap-pause 'no-ding
      isearch-lazy-count t
      isearch-repeat-on-direction-change t
      isearch-allow-motion t
      isearch-allow-scroll t
      isearch-yank-on-move 'shift
      isearch-motion-changes-direction t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil    ; Using the suffix for counting matches
                                        ; is better but does not work with
                                        ; `isearch-mb'.
      lazy-highlight-cleanup nil
      ;; The default search ring size is 16, which is too small considering
      ;; that we can fuzzy search the history with Consult.
      search-ring-max 200
      regexp-search-ring-max 200)
(add-hook! 'doom-escape-hook :depth -1
  (defun cae-clean-up-lazy-highlight-h ()
    (lazy-highlight-cleanup t)))
(when (fboundp #'+evil-disable-ex-highlights-h)
  (add-hook 'doom-escape-hook #'+evil-disable-ex-highlights-h -1))

;; I think it's better to not enable this. Doom already eagerly cleans up
;; popup and Magit buffers.
;;(cae-defadvice! cae-midnight-clean-buffer-list-delay-a (name) :before-until
;;#'midnight-clean-buffer-list-delay (parent-mode-is-derived-p
;;(buffer-local-value 'major-mode name) 'exwm-mode)) (cae-run-with-idle-timer
;;600 nil "midnight-mode" #'midnight-mode +1) (after! midnight (setq
;;clean-buffer-list-kill-regexps '("\\`\\*.*\\*\\'")
;;clean-buffer-list-delay-special 7200) (add-to-list
;;'clean-buffer-list-kill-never-buffer-names doom-fallback-buffer-name))

(after! outline
  (setq outline-minor-mode-use-buttons t))

(after! ispell
  (setq ispell-quietly t
        ispell-dictionary "en_US"
        ispell-help-in-bufferp 'electric)
  (when-let* ((nixos-aspell-dir "/run/current-system/sw/lib/aspell")
              (_ (file-exists-p nixos-aspell-dir)))
    (setq ispell-aspell-data-dir nixos-aspell-dir
          ispell-aspell-dict-dir nixos-aspell-dir)))

(after! vline
  (setq vline-idle-time 0.1))

(after! undo-fu
  (setq undo-fu-allow-undo-in-region t))

;; Hide commands in M-x which do not work in the current mode. Vertico
;; commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package! avy
  :defer t :init
  (cae-defadvice! cae-avy-use-post-style-a (oldfun &rest args)
    :around #'avy-goto-end-of-line
    (let ((avy-style 'post))
      (apply oldfun args)))
  :config
  (setq avy-timeout-seconds 0.4
        avy-all-windows t
        avy-keys (cae-keyboard-remap
                  '(?a ?s ?d ?f ?g
                    ;; Removed ?\; because it's not on default Android
                    ;; touchscreen keyboards.
                    ?h ?j ?k ?l))
        avy-background nil
        avy-single-candidate-jump t
        avy-escape-chars '(?\C-g ?\C-\[ ?\s)
        avy-dispatch-alist
        (cae-keyboard-remap
         '((?x . avy-action-kill-move) (?X . avy-action-kill-stay)
           (?t . avy-action-teleport) (?m . avy-action-mark)
           (?n . avy-action-copy) (?y . avy-action-yank)
           (?Y . avy-action-yank-line) (?i . avy-action-ispell)
           (?z . avy-action-zap-to-char)))
        avy-styles-alist '((avy-isearch . pre)
                           (ace-link-man . pre)
                           (avy-goto-end-of-line . post)
                           (avy-kill-ring-save-region . pre)
                           (avy-kill-region . pre)
                           (avy-copy-region . pre)
                           (avy-move-region . pre))))

(use-package! embrace
  :defer t :init
  ;; Respect popup rules with the Embrace help popup.
  (cae-defadvice! +cae-embrace-use-popup-a (oldfun help-string)
    :around #'embrace--show-help-buffer
    (cl-letf (((symbol-function #'display-buffer-in-side-window)
               (symbol-function #'display-buffer)))
      (funcall oldfun help-string)))
  :config
  (after! evil-embrace
    (setq evil-embrace-show-help-p t))
  (setq embrace-show-help-p t))

(use-package! abbrev
  :defer t :config
  (setq-default abbrev-mode t
                save-abbrevs 'silently)
  (setq abbrev-suggest t)
  (map! :map edit-abbrevs-mode-map
        [remap save-buffer] #'abbrev-edit-save-buffer)
  (map! :map abbrev-map "e" #'edit-abbrevs)
  (cae-advice-add #'abbrev-edit-save-buffer :after #'edit-abbrevs-redefine))

(use-package! ibuffer
  :defer t :config
  (setq ibuffer-always-show-last-buffer t
        ibuffer-human-readable-size t)
  (after! ibuffer-ext
    (add-to-list 'ibuffer-never-show-predicates #'doom-unreal-buffer-p)))

(use-package! yank-indent
  :defer t :init (add-hook 'doom-first-buffer-hook #'global-yank-indent-mode))

(use-package! file-info
  :defer t :init
  (map! :leader :desc "Show file info" "fi" #'file-info-show)
  :config
  ;; See the `:cae vc' module for further configuration.
  (setq file-info-include-headlines t
        file-info-max-value-length 100))

(use-package! titlecase
  :defer t :init
  (after! embark
    (define-key embark-region-map "T" #'titlecase-region)
    (define-key embark-heading-map "T" #'titlecase-line)
    (define-key embark-sentence-map "T" #'titlecase-sentence)))

(after! outline
  (after! which-key
    (which-key-add-keymap-based-replacements outline-minor-mode-map
      "C-c @" "outline")))

(use-package! embark
  :defer t :config
  (after! vertico
    (define-key vertico-map (kbd "C-z") #'cae-embark-act-with-completing-read))
  (cae-advice-add #'embark-completing-read-prompter :around
                  #'cae-bind-C-z-to-abort-a))

(use-package! logos
  :defer t :init
  (map! [remap forward-page] #'logos-forward-page-dwim
        [remap backward-page] #'logos-backward-page-dwim
        [remap narrow-to-page] #'cae-narrow-to-page)
  :config
  (setq logos-outlines-are-pages t))

(use-package! parrot
  :defer t :init
  ;; Wrangle parrot into being fully lazy-loaded.
  (autoload #'parrot-party-while-process "parrot")
  (autoload #'parrot--todo-party "parrot")
  (autoload #'parrot--magit-push-filter "parrot")
  (cae-defadvice! cae-modeline-gac-party-on-push-a (buffer)
    :after #'gac-push
    (when-let* ((proc (get-buffer-process "*git-auto-push*")))
      (parrot-party-while-process proc)))
  (add-hook 'org-after-todo-state-change-hook #'parrot--todo-party)
  (cae-advice-add 'magit-run-git-async :around #'parrot--magit-push-filter)
  :config
  (setq parrot-num-rotations 3
        parrot-animate-on-load nil
        parrot-animate 'hide-static
        parrot-party-on-magit-push t
        parrot-party-on-org-todo-states '("DONE")
        parrot-type 'nyan)
  (parrot-mode -1))

(use-package! string-inflection
  :defer t
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix-map ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (defvaralias 'naming-convention-map 'doom-leader-naming\ convention-map)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~")
                'evil-operator-string-inflection)))

(use-package! beginend
  :defer t :init
  (add-hook 'doom-first-input-hook #'beginend-global-mode)
  ;; This patches around this function not being compatible with Evil when
  ;; `evil-move-beyond-eol' is `nil'. This should probably go into
  ;; `evil-collection'.
  (cae-defadvice! cae-beginend-goto-eol-a ()
    :before #'beginend-prog-mode-goto-end
    (goto-char (point-at-eol))))

(use-package! isearch-mb
  :defer t :after-call isearch-mode-hook :init
  (when (modulep! :editor evil)
    (after! evil
      ;; I prefer `isearch' combined with `isearch-mb'.
      (setopt evil-search-module 'isearch)
      ;; I have a setup in `config.el' where I cleanup the overlays in
      ;; `doom-escape-hook'. I prefer that because I use the overlays as a
      ;; quick way to highlight text.
      (cae-advice-add #'evil-flash-hook :override #'ignore)
      (cae-advice-add #'evil-clean-isearch-overlays :override #'ignore)
      (when evil-want-C-w-delete
        (map! :map isearch-mb-minibuffer-map
              "C-w" #'evil-delete-backward-word))))
  :config
  (isearch-mb--setup)
  (isearch-mb-mode +1)
  (dolist (cmd '(recenter-top-bottom reposition-window
                 scroll-right scroll-left isearch-yank-word
                 consult-isearch-history))
    (add-to-list 'isearch-mb--with-buffer cmd))
  (dolist (cmd '(anzu-isearch-query-replace anzu-isearch-query-replace-regexp
                 avy-isearch consult-line))
    (add-to-list 'isearch-mb--after-exit cmd))
  (define-key isearch-mb-minibuffer-map (kbd "M-j") #'avy-isearch)
  (when (modulep! :completion vertico)
    (map! :map isearch-mb-minibuffer-map
          [remap consult-history] #'consult-isearch-history)
    (define-key isearch-mb-minibuffer-map (kbd "M-s l") 'consult-line))
  (map! :map isearch-mb-minibuffer-map
        [remap isearch-query-replace] #'anzu-isearch-query-replace
        [remap isearch-query-replace-regexp] #'anzu-query-replace-regexp))

(use-package! edit-indirect
  :defer t :config
  (add-hook! 'edit-indirect-after-creation-hook
    (defun cae-edit-indirect-major-mode-fallback-h ()
      (when (eq major-mode 'fundamental-mode)
        (funcall (buffer-local-value
                  'major-mode
                  (overlay-buffer edit-indirect--overlay))))))
  (add-hook! 'edit-indirect-after-creation-hook :append
    (defun cae-edit-indirect-setup-defaults-h ()
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode -1)))))

(use-package! zop-to-char
  :defer t :init
  (map! [remap zap-to-char] #'zop-to-char
        [remap zap-up-to-char] #'zop-up-to-char)
  :config
  (setq zop-to-char-kill-keys '(?\C-m ?\C-k ?\C-w)))

(use-package! auto-sudoedit
  :defer t :init
  (autoload 'auto-sudoedit "auto-sudoedit")
  (defun cae-auto-sudoedit-lazy-h ()
    "When visiting a file that isn’t writable, load auto-sudoedit and run it."
    (unless (featurep 'auto-sudoedit)
      (let ((path (or buffer-file-name list-buffers-directory)))
        (when (and path (not (file-writable-p path)))
          (auto-sudoedit))))
    (remove-hook 'find-file-hook #'cae-auto-sudoedit-maybe-h)
    (remove-hook 'dired-mode-hook #'cae-auto-sudoedit-maybe-h))
  (add-hook 'find-file-hook #'cae-auto-sudoedit-lazy-h)
  (add-hook 'dired-mode-hook  #'cae-auto-sudoedit-lazy-h)
  :config
  (auto-sudoedit-mode +1))

(use-package! jinx
  :defer t :init
  (defun cae-jinx-ispell-localwords ()
    "Return a string of ispell's local words.

Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
    (require 'ispell)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (search-forward ispell-words-keyword nil t)
               collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
               finally return (mapconcat #'identity result " "))))
  (defun cae-jinx-add-ispell-localwords ()
    "Add ispell's local words to `jinx-local-words'."
    (let ((ispell-localwords (cae-jinx-ispell-localwords)))
      (setq jinx-local-words (concat jinx-local-words ispell-localwords))
      (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))
  (add-hook 'jinx-mode-hook #'cae-jinx-add-ispell-localwords)
  :config
  (cl-pushnew 'org-inline-src-block
              (alist-get 'org-mode jinx-exclude-faces))
  (map! "M-$" #'jinx-correct
        "C-M-$" #'jinx-languages)
  (setf (alist-get ?* jinx--save-keys) #'cae-jinx-save-as-ispell-localword)
  (cae-advice-add #'jinx--correct-replace :before #'cae-jinx-add-to-abbrev)
  (after! vertico-multiform
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)) t)))

;;Local Variables:
;;jinx-local-words: "downCase CamelCase iff uit sw fi"
;;End:
