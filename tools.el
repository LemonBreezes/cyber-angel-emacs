;;; tools.el -*- lexical-binding: t; -*-

(when (modulep! :tools lsp)
  (load! "lisp/cae-lsp" doom-user-dir))

;; Disable Semantic.
(cae-defadvice! cae-semantic-disable-a (&rest _)
  :override #'semantic-mode
  ;; If something tries to enable semantic-mode, emit a backtrace.
  (backtrace))

;; Automatically allow envrc files after saving them.
(add-hook! 'envrc-file-mode-hook
  (defun cae-envrc-file-mode-setup ()
    (add-hook 'after-save-hook 'envrc-allow nil t)))

;; I use `w3m' because EWW is too slow.
(use-package! w3m
  :defer t :init
  (setq browse-url-secondary-browser-function #'w3m-browse-url)
  :config
  (setq w3m-user-agent
        (string-join
         '("Mozilla/5.0"
           "(Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
           "AppleWebKit/533.1""(KHTML, like Gecko)" "Version/4.0"
           "Mobile Safari/533.")
         " ")
        w3m-command-arguments '("-cookie" "-F"))
  (after! w3m-search
    (setq w3m-search-default-engine "duckduckgo"))
  (map! :map w3m-mode-map
        "o" #'ace-link-w3m))

;; Set up the default browser.
(after! browse-url
  (setq browse-url-secondary-browser-function
        (if (eq browse-url-secondary-browser-function
                #'browse-url-default-browser)
            #'eww-browse-url
          browse-url-secondary-browser-function)
        browse-url-browser-function
        (cond ((executable-find "termux-setup-storage")
               #'browse-url-xdg-open)
              ((getenv "SSH_TTY") browse-url-secondary-browser-function)
              (t #'browse-url-generic))
        browse-url-firefox-new-window-is-tab t)

  (defvar cae-generic-browser-name nil)
  (cond
   ((getenv "WSL_DISTRO_NAME")
    (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
          browse-url-generic-args '("/c" "start")))
   ((executable-find "firefox-beta")
    (setq browse-url-generic-program "firefox-beta"
          browse-url-generic-args '("--new-tab")
          cae-generic-browser-name "Firefox"))
   ((executable-find "firefox")
    (setq browse-url-generic-program "firefox"
          browse-url-generic-args '("--new-tab")
          cae-generic-browser-name "Firefox"))
   ((executable-find "firefox-bin")
    (setq browse-url-generic-program "firefox-bin"
          browse-url-generic-args '("--new-tab")
          cae-generic-browser-name "Firefox"))
   ((when-let* ((chrome (or (executable-find "chromium-bin-browser")
                            (executable-find "google-chrome-unstable")
                            (executable-find "google-chrome-stable"))))
      (setq browse-url-generic-program chrome
            browse-url-generic-args (when (eq (user-uid) 0)
                                      '("--no-sandbox"))
            cae-generic-browser-name "Chrome")))))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'doom-large-file-excluded-modes 'nov-mode)

(add-to-list 'auto-mode-alist '("/sway/.*config.*/" . i3wm-config-mode))
(add-to-list 'auto-mode-alist '("/sway/config\\'" . i3wm-config-mode))

;; Set up printers.
(after! lpr (setq printer-name "Brother_HL-2350DW"))
(after! ps-print (setq ps-printer-name "Brother_HL-2350DW"))
(after! pdf-misc
  (setq pdf-misc-print-program-executable (executable-find "lpr")))

;; Was reading this
;; https://github.com/link0ff/emacs-init/blob/1fc141e20092cc357f2c6021626635e8ac067b8c/emacs.custom.el.
(setq delete-by-moving-to-trash t
      ;; Careful with these settings! They can prevent errors and Emacs
      ;; freezing but they can also surprise you if you expect all your files
      ;; to be trashed when you delete them.
      remote-file-name-inhibit-delete-by-moving-to-trash t
      remote-file-name-inhibit-auto-save t
      remote-file-name-inhibit-auto-save-visited t
      ;; ---
      large-file-warning-threshold 100000000 ; ~100 MB
      yank-pop-change-selection t
      global-mark-ring-max 1024
      history-delete-duplicates t
      history-length t
      mark-ring-max 1024
      message-log-max t
      kill-ring-max 1024
      kill-whole-line t
      list-matching-lines-jump-to-current-line t
      mouse-prefer-closest-glyph t
      next-error-message-highlight 'keep
      read-char-by-name-sort 'code
      revert-buffer-quick-short-answers t
      scroll-error-top-bottom t
      scroll-preserve-screen-position t
      shift-select-mode 'permanent
      track-eol t
      visual-order-cursor-movement t
      what-cursor-show-names t)

;; https://idiomdrottning.org/show-trailing-whitespace
;; `show-trailing-whitespace' is my friend.
(setq-hook! (text-mode prog-mode conf-mode)
  show-trailing-whitespace t)

(after! cus-edit
  (setq custom-buffer-done-kill t))

(after! saveplace
  (setq save-place-limit nil))

(after! grep
  (setq grep-use-headings t
        grep-program "rg"))

(after! tar-mode
  (setq tar-mode-show-date t))

(after! info
  (setq Info-fontify-maximum-menu-size t))

(after! descr-text
  (setq describe-char-unicodedata-file
        (concat cae-multi-data-dir "UnicodeData.txt")))

(after! smiley
  (setq smiley-style t))

(after! xref
  (setq xref-search-program 'ripgrep))

;; We use `corfu' and `vertico' instead of the built-in completions, but I
;; still have this here as my preferred defaults for the built-in completions.
(setq completion-auto-select 'second-tab
      completions-detailed t
      completions-format 'vertical
      completions-group t
      completions-group-sort 'alphabetical)

;; These are not used in Doom Emacs since we use `helpful' instead, but I have
;; them here as "better defaults" so-to-say.
(add-hook 'help-fns-describe-function-functions
          #'shortdoc-help-fns-examples-function)
(after! help
  (setq help-enable-symbol-autoload t
        help-enable-completion-autoload t
        help-enable-symbol-autoload t
        help-window-select t
        help-clean-buttons t
        help-enable-variable-value-editing t))

(add-hook 'bookmark-bmenu-mode-hook #'cae-bookmark-extra-keywords)
(after! bookmark
  (setq bookmark-bmenu-file-column 50
        bookmark-watch-bookmark-file nil))

(after! auth-source
  (setq auth-source-cache-expiry nil
        auth-sources (list (concat doom-user-dir "secrets/authinfo"))
        auth-source-gpg-encrypt-to nil))

(after! password-cache
  (setq password-cache-expiry nil))

(after! compile
  ;; The `first-error' value stops scrolling at the first warning too, which I
  ;; don't like.
  (setq compilation-scroll-output t)
  ;; Testing to see if this fixes some issues with compilation buffer escape
  ;; sequences not being colorized.
  (remove-hook 'compilation-filter-hook #'comint-truncate-buffer))

(after! tramp
  (setq tramp-use-scp-direct-remote-copying t
        tramp-allow-unsafe-temporary-files t)
  (dolist (path '("~/.guix-profile/bin" "~/.guix-profile/sbin"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))
    (add-to-list 'tramp-remote-path path)))

(after! marginalia
  ;; Use `embark-file-map' for `ffap-menu'.
  (add-to-list 'marginalia-prompt-categories '("\\<find file\\>" . file)))
(add-hook 'vertico-mode-hook #'vertico-multiform-mode)
(add-hook 'vertico-mode-hook #'vertico-mouse-mode)
(remove-hook 'vertico-mode-hook #'vertico-posframe-mode)
(after! vertico-multiform
  (let* ((posframe-supported-p (and (modulep! :completion vertico +childframe)
                                    (or (cae-display-graphic-p)
                                        (> emacs-major-version 30))))
         (wide-frame-p (>= (frame-width) 120))
         (default-view (if wide-frame-p
                           'posframe
                         'buffer))
         (new-entries `((embark-keybinding . (, (if wide-frame-p
                                                    'grid 'buffer)))
                        (consult-grep . (, (if wide-frame-p
                                               'posframe
                                             'buffer)))
                        (imenu . (, (if wide-frame-p
                                        (if posframe-supported-p
                                            '(posframe grid) '(grid))
                                      'buffer)))
                        (consult-location . (, (if wide-frame-p
                                                   'posframe
                                                 'buffer)))
                        (file . (, default-view))
                        (t . (, default-view)))))
    (dolist (entry new-entries)
      (let ((key (car entry))
            (value (cdr entry)))
        ;; Merge the new value with existing values, ensuring idempotency
        (setf (alist-get key vertico-multiform-categories)
              (cl-delete-duplicates
               (append (alist-get key vertico-multiform-categories)
                       value)
               :test #'equal))))
    ;; The order matters. We reverse to give priority to earlier entries.
    (setq vertico-multiform-categories (nreverse vertico-multiform-categories))
    ;; Ensure `execute-extended-command' has the correct view
    (let* ((execute-command-view default-view)
           (existing-views (alist-get 'execute-extended-command
                                      vertico-multiform-commands))
           (cleaned-views (cl-set-difference existing-views
                                             '(flat vertical posframe grid
                                               reverse unobtrusive))))
      (setf (alist-get 'execute-extended-command vertico-multiform-commands)
            (cl-delete-duplicates
             (cons execute-command-view cleaned-views)
             :test #'equal)))))

(after! spell-fu
  (add-to-list 'spell-fu-faces-exclude 'message-header-other)
  (add-to-list 'spell-fu-faces-exclude 'org-property-value)
  (add-to-list 'spell-fu-faces-exclude 'message-header-to)
  (setq spell-fu-word-delimit-camel-case t)
  (setq spell-fu-faces-exclude
        (delq 'font-lock-string-face spell-fu-faces-include)))

(after! spell-fu
  (add-hook! 'nxml-mode-hook
    (defun cae-disable-spell-fu-h ()
      (spell-fu-mode -1))))

(use-package! 0x0
  :defer t :init
  (map! "C-x U" #'0x0-dwim)
  (after! embark
    (define-key embark-region-map (kbd "U") '0x0-dwim)))

(after! consult-yasnippet
  (setq consult-yasnippet-use-thing-at-point t
        consult-yasnippet-always-overwrite-thing-at-point t))
