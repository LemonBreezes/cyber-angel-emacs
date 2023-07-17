;;; private/exwm/+evil.el -*- lexical-binding: t; -*-

(after! evil
  (evil-set-initial-state 'exwm-mode 'normal)
  (evil-esc-mode -1))
(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'exwm-mode))

;; Fixes focus being lost from EXWM buffers when switching workspaces or
;; buffers. This seems to be a problem specific to Evil. and perhaps Doom. There
;; is probably a more elegant solution but it requires more digging.
(add-hook 'doom-switch-buffer-hook #'+exwm-refocus-application)
(add-hook 'doom-switch-window-hook #'+exwm-refocus-application)
(when (featurep! :ui workspaces)
  (add-hook 'persp-before-switch-functions #'+exwm-refocus-application))


(use-package exwm-evil
  :after exwm
  :config
  (exwm-evil-enable-mouse-workaround)
  (add-hook 'exwm-mode-hook #'enable-exwm-evil-mode)
  (add-hook 'exwm-manage-finish-hook #'enable-exwm-evil-mode)
  (add-hook 'doom-switch-buffer-hook #'enable-exwm-evil-mode)
  (cl-pushnew 'escape exwm-input-prefix-keys)

  ;; We will disable `C-c' in insert state.
  (define-key exwm-mode-map (kbd "C-c") nil)

  ;; At this point, we `garbage-collect' without hanging input for our
  ;; application.
  (advice-add #'exwm-evil-insert :after #'garbage-collect)

  (map! :map exwm-mode-map
        :localleader
        (:prefix ("d" . "debug")
         "l" #'xcb-debug:clear
         "m" #'xcb-debug:mark
         "t" #'exwm-debug)
        "f" #'exwm-layout-toggle-fullscreen
        "h" #'exwm-floating-hide
        "q" #'exwm-input-send-next-key
        "SPC" #'exwm-floating-toggle-floating
        "e" (cmd! (exwm-evil-send-key 1 'escape))
        "m" #'exwm-layout-toggle-mode-line))

(use-package! exwm-firefox-evil
  :after exwm
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

  (add-hook 'exwm-manage-finish-hook #'exwm-firefox-evil-activate-if-firefox)
  (add-hook 'doom-switch-buffer-hook #'exwm-firefox-evil-activate-if-firefox)

  (setq-hook! 'exwm-firefox-evil-mode-hook bookmark-make-record-function #'+exwm-firefox-bookmark--make)

  ;; Automatically reenable `evil-normal-state' after following a link.
  (advice-add #'exwm-firefox-core-focus-search-bar
              :after
              (defun +exwm-firefox-core-focus-search-bar-a ()
                (add-hook 'exwm-update-title-hook #'exwm-firefox-core-hint-links-h)))
  (advice-add #'exwm-firefox-core-tab-new
              :after
              (defun +exwm-firefox-core-tab-new-a ()
                (add-hook 'exwm-update-title-hook #'+exwm-firefox-core-focus-search-bar-a-h)))

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
