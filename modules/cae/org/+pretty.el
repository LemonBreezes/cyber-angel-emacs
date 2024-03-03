;;; private/org/+pretty.el -*- lexical-binding: t; -*-

(use-package! org-modern
  :unless (cae-tty-disable-unicode-p)
  :defer t :init
  (after! org
    (require 'org-modern)
    (global-org-modern-mode +1))
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-priority '((?A . "⚑")
                              (?B . "⬆")
                              (?C . "■")
                              (?D . "⬇")
                              (?E . "⛾"))
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("embed" . "↪️")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo))
  (after! spell-fu
    (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist))))

;;(when (modulep! :ui ligatures)
;;  (appendq! +ligatures-extra-symbols
;;            (list :list_property "∷"
;;                  :em_dash       "—"
;;                  :ellipses      "…"
;;                  :arrow_right   "→"
;;                  :arrow_left    "←"
;;                  :arrow_lr      "↔"
;;                  :properties    "⚙"
;;                  :end           "∎"
;;                  :priority_a    #("⚑" 0 1 (face nerd-icons-red))
;;                  :priority_b    #("⬆" 0 1 (face nerd-icons-orange))
;;                  :priority_c    #("■" 0 1 (face nerd-icons-yellow))
;;                  :priority_d    #("⬇" 0 1 (face nerd-icons-green))
;;                  :priority_e    #("❓" 0 1 (face nerd-icons-blue))))
;;
;;  (defadvice! +org-init-appearance-h--no-ligatures-a ()
;;    :after #'+org-init-appearance-h
;;    (set-ligatures! 'org-mode nil)
;;    (set-ligatures! 'org-mode
;;                    :list_property "::"
;;                    :em_dash       "---"
;;                    :ellipsis      "..."
;;                    :arrow_right   "->"
;;                    :arrow_left    "<-"
;;                    :arrow_lr      "<->"
;;                    :properties    ":PROPERTIES:"
;;                    :end           ":END:"
;;                    :priority_a    "[#A]"
;;                    :priority_b    "[#B]"
;;                    :priority_c    "[#C]"
;;                    :priority_d    "[#D]"
;;                    :priority_e    "[#E]")))
