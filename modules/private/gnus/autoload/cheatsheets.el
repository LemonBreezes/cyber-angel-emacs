;;; private/gnus/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-gnus-group-cheatsheet/body "private/misc-applications/autoload/cae-cheatsheets" nil t)
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
  ("q" nil)
  ("<f6>" nil))

;;;###autoload (autoload 'cae-gnus-summary-cheatsheet/body "private/misc-applications/autoload/cae-cheatsheets" nil t"
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
  ("q" nil)
  ("<f6>" nil))

;;;###autoload (autoload 'cae-gnus-article-cheatsheet/body "private/misc-applications/autoload/cae-cheatsheets" nil t"
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
  ("q" nil)
  ("<f6>" nil))
