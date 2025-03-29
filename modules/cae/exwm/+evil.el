;;; cae/exwm/+evil.el -*- lexical-binding: t; -*-

(evil-set-initial-state 'exwm-mode 'normal)
(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'exwm-mode))

(use-package! exwm-evil
  :defer t :init
  (autoload 'enable-exwm-evil-mode "exwm-evil" nil t)
  ;; Has to be enabled before `exwm-firefox-evil'.
  (add-hook 'exwm-manage-finish-hook #'enable-exwm-evil-mode 0)
  :config
  (cl-pushnew 'escape exwm-input-prefix-keys)

  ;; We will disable `C-c' in insert state.
  (define-key exwm-mode-map (kbd "C-c") nil))

(use-package! exwm-firefox-evil
  :defer t :init
  (add-hook 'exwm-manage-finish-hook #'exwm-firefox-evil-activate-if-firefox 1)
  :config
  (cl-pushnew 'escape exwm-input-prefix-keys)
  ;; We can use VIM keys with any browser that has compatible keybindings.
  (cl-loop for class in '("firefoxdeveloperedition"
                          "\"firefoxdeveloperedition\""
                          "IceCat"
                          "chromium-browser"
                          "Chromium"
                          "Google-chrome"
                          "Google-chrome-unstable"
                          "Chromium-bin-browser-chromium"
                          "librewolf-default")
           do (cl-pushnew class exwm-firefox-evil-firefox-class-name
                          :test #'string=))
  (setq-hook! 'exwm-firefox-evil-mode-hook bookmark-make-record-function #'cae-exwm-firefox-bookmark--make)

  ;; Automatically reenable `evil-normal-state' after following a link.
  (cae-advice-add #'exwm-firefox-core-focus-search-bar
                  :after
                  (defun cae-exwm-firefox-core-focus-search-bar-a ()
                    (add-hook 'exwm-update-title-hook #'exwm-firefox-core-hint-links-h)))
  (cae-advice-add #'exwm-firefox-core-tab-new
                  :after
                  (defun cae-exwm-firefox-core-tab-new-a ()
                    (add-hook 'exwm-update-title-hook #'cae-exwm-firefox-core-focus-search-bar-a-h)))

  (map! :map exwm-firefox-evil-mode-map
        :n "f"  #'exwm-firefox-core-hint-links ; Requires Link Hints add-on.
        :n "F"  #'exwm-firefox-core-hint-links-new-tab-and-switch
        :n "u"  #'exwm-firefox-core-tab-close-undo
        :n "U"  #'exwm-firefox-core-undo
        :n "/"  #'exwm-firefox-core-find ; Compatible with Chrome as well.

        ;; Do not accidentally send escape
        :n [remap exwm-firefox-core-cancel] #'exwm-evil-normal-state

        :after exwm-evil
        ;; These are more in line with Evil than the default
        :n "g0" #'exwm-firefox-core-tab-first
        :n "g$" #'exwm-firefox-core-tab-last
        :n "0"  #'exwm-evil-core-beginning-of-line
        :n "$"  #'exwm-evil-core-end-of-line
        :n "c"  #'exwm-evil-core-change
        ;; This way we can use prefix arguments with these commands.
        :n "j" #'exwm-evil-core-down
        :n "k" #'exwm-evil-core-up
        :n "h" #'exwm-evil-core-left
        :n "l" #'exwm-evil-core-right
        ;; Add zoom commands
        :n "+" #'exwm-evil-core-zoom-in
        :n "-" #'exwm-evil-core-zoom-out
        :n "=" #'exwm-evil-core-reset-zoom
        ;; Pass through some keybindings from Firefox
        :n "C-<next>"  #'exwm-firefox-core-tab-next
        :n "C-<prior>" #'exwm-firefox-core-tab-previous
        :n "<f6>" #'exwm-firefox-core-focus-search-bar))

(defvar cae-exwm-discord-mode-map (make-sparse-keymap))

(define-minor-mode cae-exwm-discord-mode
  "Minor mode for Discord."
  :init-value nil
  :keymap cae-exwm-discord-mode-map
  :global nil
  (if cae-exwm-discord-mode
      (progn (let ((map exwm-evil-mode-map))
               (make-variable-buffer-local 'exwm-evil-mode-map)
               (setq exwm-evil-mode-map cae-exwm-discord-mode-map)
               (set-keymap-parent cae-exwm-discord-mode-map map)))
    (kill-local-variable 'cae-exwm-discord-mode-map)))

(map! :map cae-exwm-discord-mode-map
      :n "J" (cmd! ()
                   (exwm-input--fake-key 'C-k)
                   (exwm-evil-insert))
      :n "C-k" (cmd! ()
                     (exwm-input--fake-key 'C-k)
                     (exwm-evil-insert)))

(add-hook! 'exwm-manage-finish-hook :depth 1 #'cae-exwm-load-special-bindings-h)
