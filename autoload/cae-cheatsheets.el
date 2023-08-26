;;; autoload/cae-cheatsheets.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-embark-collect-cheatsheet-hydra/body "autoload/cae-cheatsheets" nil t)
(defhydra cae-embark-collect-cheatsheet-hydra (:color pink :foreign-keys run)
  ("a" embark-act "Act" :column "Act")
  ("A" embark-act-all "Act on all" :column "Act")
  ("E" embark-export "Export" :column "Act")
  ;;("S" tabulated-list-sort "Sort" :column "Navigate")
  ("m" embark-collect-mark "Mark" :column "Act")
  ("s" isearch-forward "Search forward" :column "Navigate")
  ;;("{" outline-previous-heading "Previous heading" :column "Navigate")
  ;;("}" outline-next-heading "Next heading" :column "Navigate")
  ("u" embark-collect-unmark "Unmark" :column "Act")
  ("U" embark-collect-unmark-all "Unmark all" :column "Act")
  ("t" embark-collect-toggle-marks "Toggle marks" :column "Act")
  ("M-a" embark-collect-direct-action-minor-mode "Toggle direct action" :column "Act")
  ;;("M-<left>" tabulated-list-previous-column "Previous column" :column "Navigate")
  ;;("M-<right>" tabulated-list-next-column "Next column" :column "Navigate")
  ("q" nil "Exit" :exit t :column nil))

;;;###autoload (autoload 'cae-vertico-cheatsheet-hydra/body "autoload/cae-cheatsheets" nil t)
(defhydra cae-vertico-cheatsheet-hydra (:color pink :foreign-keys run)
  ("C-c ;" embark-export "Export")
  ("C-c C-e" +vertico/embark-export-write "Export writable")
  ("C-c C-l" embark-collect "Collect")
  ("q" nil "Exit" :exit t))

;;;###autoload (autoload 'cae-debugger-cheatsheet "autoload/cae-cheatsheets" nil t)
(defun cae-debugger-cheatsheet () (interactive))
(hercules-def
 :toggle-funs #'cae-debugger-cheatsheet
 :keymap 'debugger-mode-map
 :package 'debug)

;;;###autoload (autoload 'cae-edebug-cheatsheet "autoload/cae-cheatsheets" nil t)
(defun cae-edebug-cheatsheet () (interactive))
(hercules-def
 :toggle-funs #'cae-edebug-cheatsheet
 :keymap 'edebug-mode-map
 :package 'edebug)

;;;###autoload (autoload 'cae-macrostep-cheatsheet "autoload/cae-cheatsheets" nil t)
(defun cae-macrostep-cheatsheet () (interactive))
(hercules-def
 :toggle-funs #'cae-macrostep-cheatsheet
 :keymap 'macrostep-mode-keymap
 :package 'macrostep)

;;;###autoload (autoload 'cae-symbol-overlay-cheatsheet "autoload/cae-cheatsheets" nil t)
(defun cae-symbol-overlay-cheatsheet () (interactive))
(hercules-def
 :toggle-funs #'cae-symbol-overlay-cheatsheet
 :keymap 'symbol-overlay-map
 :package 'symbol-overlay)

;;;###autoload (autoload 'cae-gnus-group-cheatsheet/body "autoload/cae-cheatsheets" nil t)
(defhydra cae-gnus-group-cheatsheet (:color blue)
  "
[_A_] Remote groups (A A) [_g_] Refresh
[_L_] Local groups        [_\\^_] List servers
[_c_] Mark all read       [_m_] Compose new mail
[_G_] Search mails (G G) [_#_] Mark mail
"
  ("A" gnus-group-list-active)
  ("L" gnus-group-list-all-groups)
  ("c" gnus-topic-catchup-articles)
  ("G" dianyou-group-make-nnir-group)
  ("g" gnus-group-get-new-news)
  ("^" gnus-group-enter-server-mode)
  ("m" gnus-group-new-mail)
  ("#" gnus-topic-mark-topic)
  ("q" nil))

;;;###autoload (autoload 'cae-gnus-summary-cheatsheet/body "autoload/cae-cheatsheets" nil t"
(defhydra cae-gnus-summary-cheatsheet (:color blue)
  "
[_s_] Show thread   [_F_] Forward (C-c C-f)
[_h_] Hide thread   [_e_] Resend (S D e)
[_n_] Refresh (/ N) [_r_] Reply
[_!_] Mail -> disk  [_R_] Reply with original
[_d_] Disk -> mail  [_w_] Reply all (S w)
[_c_] Read all      [_W_] Reply all with original (S W)
[_#_] Mark          [_G_] Search mails
"
  ("s" gnus-summary-show-thread)
  ("h" gnus-summary-hide-thread)
  ("n" gnus-summary-insert-new-articles)
  ("F" gnus-summary-mail-forward)
  ("!" gnus-summary-tick-article-forward)
  ("d" gnus-summary-put-mark-as-read-next)
  ("c" gnus-summary-catchup-and-exit)
  ("e" gnus-summary-resend-message-edit)
  ("R" gnus-summary-reply-with-original)
  ("r" gnus-summary-reply)
  ("W" gnus-summary-wide-reply-with-original)
  ("w" gnus-summary-wide-reply)
  ("#" gnus-topic-mark-topic)
  ("G" dianyou-group-make-nnir-group)
  ("q" nil))

;;;###autoload (autoload 'cae-gnus-article-cheatsheet/body "autoload/cae-cheatsheets" nil t"
(defhydra cae-gnus-article-cheatsheet (:color blue)
  "
[_o_] Save attachment        [_F_] Forward
[_v_] Play video/audio       [_r_] Reply
[_d_] CLI to download stream [_R_] Reply with original
[_b_] Open external browser  [_w_] Reply all (S w)
[_f_] Click link/button      [_W_] Reply all with original (S W)
[_g_] Focus link/button
"
  ("F" gnus-summary-mail-forward)
  ("r" gnus-article-reply)
  ("R" gnus-article-reply-with-original)
  ("w" gnus-article-wide-reply)
  ("W" gnus-article-wide-reply-with-original)
  ("o" gnus-mime-save-part)
  ("v" my-w3m-open-with-mplayer)
  ("d" my-w3m-download-rss-stream)
  ("b" my-w3m-open-link-or-image-or-url)
  ("f" w3m-lnum-follow)
  ("g" w3m-lnum-goto)
  ("q" nil))
