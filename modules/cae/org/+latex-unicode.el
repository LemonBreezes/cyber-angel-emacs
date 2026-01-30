;;; cae/org/+lattie-unicode.el -*- lexical-binding: t; -*-

(require 'lattie)
(require 'lattie-unicode)

(setq lattie--latex-header-file
      (expand-file-name "unicode-latex.tex" doom-cache-dir))

(lattie-unicode--update-header-and-abbrevs
 '((?≤ "\\leq" "leq")
   (?≥ "\\geq" "geq")
   (?≠ "\\neq" "neq")
   (?≅ "\\cong" "iso" "cong")
   (?≃ "\\simeq" "heq" "simeq")
   (?≣ "\\equiv" "equiv")
   (?⟺ "\\Leftrightarrow" "iff")
   (?∗ "*")

   (?𝒜 "\\mathscr{A}" "sa")
   (?𝒞 "\\mathscr{C}" "sc")
   (?𝒟 "\\mathscr{D}" "sd")
   (?𝒢 "\\mathscr{G}" "sg")
   (?𝒥 "\\mathscr{J}" "sj")
   (?𝒦 "\\mathscr{K}" "sk")
   (?𝒩 "\\mathscr{N}" "sn")
   (?𝒪 "\\mathscr{O}" "so")
   (?𝒫 "\\mathscr{P}" "sp")
   (?𝒬 "\\mathscr{Q}" "sq")
   (?𝒮 "\\mathscr{S}" "ss")
   (?𝒯 "\\mathscr{T}" "st")
   (?𝒰 "\\mathscr{U}" "su")
   (?𝒱 "\\mathscr{V}" "sv")
   (?𝒲 "\\mathscr{W}" "sw")
   (?𝒳 "\\mathscr{X}" "sx")
   (?𝒴 "\\mathscr{Y}" "sy")
   (?𝒵 "\\mathscr{Z}" "sz")

   (?𝔸 "\\mathbb{A}" "wa")
   (?𝔹 "\\mathbb{B}" "wb")
   (?ℂ "\\mathbb{C}" "wc")
   (?𝔻 "\\mathbb{D}" "wd")
   (?𝔼 "\\mathbb{E}" "we")
   (?𝔽 "\\mathbb{F}" "wf")
   (?𝔾 "\\mathbb{G}" "wg")
   (?ℍ "\\mathbb{H}" "wh")
   (?𝕀 "\\mathbb{I}" "wi")
   (?𝕁 "\\mathbb{J}" "wj")
   (?𝕂 "\\mathbb{K}" "wk")
   (?𝕃 "\\mathbb{L}" "wl")
   (?𝕄 "\\mathbb{M}" "wm")
   (?ℕ "\\mathbb{N}" "wn")
   (?𝕆 "\\mathbb{O}" "wo")
   (?ℙ "\\mathbb{P}" "wp")
   (?ℚ "\\mathbb{Q}" "wq")
   (?ℝ "\\mathbb{R}" "wr")
   (?𝕊 "\\mathbb{S}" "ws")
   (?𝕋 "\\mathbb{T}" "wt")
   (?𝕌 "\\mathbb{U}" "wu")
   (?𝕍 "\\mathbb{V}" "wv")
   (?𝕎 "\\mathbb{W}" "ww")
   (?𝕏 "\\mathbb{X}" "wx")
   (?𝕐 "\\mathbb{Y}" "wy")
   (?ℤ "\\mathbb{Z}" "wz")
   ;; (?𝟘 "\\mathbb{0}" "w0")
   (?𝟙 "\\mathbb{1}" "w1")
   (?𝟚 "\\mathbbm{2}" "w2")
   (?𝟛 "\\mathbb{3}" "w3")
   (?𝕒 "\\mathbb{a}" "wA")
   (?𝕓 "\\mathbb{b}" "wB")
   (?𝕔 "\\mathbb{c}" "wC")
   (?𝕕 "\\mathbb{d}" "wD")
   (?𝕖 "\\mathbb{e}" "wE")
   (?𝕗 "\\mathbb{f}" "wF")
   (?𝕘 "\\mathbb{g}" "wG")
   (?𝕙 "\\mathbb{h}" "wH")
   (?𝕚 "\\mathbb{i}" "wI")
   (?𝕛 "\\mathbb{j}" "wJ")
   (?𝕜 "\\mathbb{k}" "wK")
   (?𝕝 "\\mathbb{l}" "wL")
   (?𝕞 "\\mathbb{m}" "wM")
   (?𝕟 "\\mathbb{n}" "wN")
   (?𝕠 "\\mathbb{o}" "wO")
   (?𝕡 "\\mathbb{p}" "wP")
   (?𝕢 "\\mathbb{q}" "wQ")
   (?𝕣 "\\mathbb{r}" "wR")
   (?𝕤 "\\mathbb{s}" "wS")
   (?𝕥 "\\mathbb{t}" "wT")
   (?𝕦 "\\mathbb{u}" "wU")
   (?𝕧 "\\mathbb{v}" "wV")
   (?𝕨 "\\mathbb{w}" "wW")
   (?𝕩 "\\mathbb{x}" "wX")
   (?𝕪 "\\mathbb{y}" "wY")
   (?𝕫 "\\mathbb{z}" "wZ")

   (?𝐀 "\\mathbf{A}" "ba")
   (?𝐁 "\\mathbf{B}" "bb")
   (?𝐂 "\\mathbf{C}" "bc")
   (?𝐃 "\\mathbf{D}" "bd")
   (?𝐄 "\\mathbf{E}" "be")
   (?𝐅 "\\mathbf{F}" "bf")
   (?𝐆 "\\mathbf{G}" "bg")
   (?𝐇 "\\mathbf{H}" "bh")
   (?𝐈 "\\mathbf{I}" "bi")
   (?𝐉 "\\mathbf{J}" "bj")
   (?𝐊 "\\mathbf{K}" "bk")
   (?𝐋 "\\mathbf{L}" "bl")
   (?𝐌 "\\mathbf{M}" "bm")
   (?𝐍 "\\mathbf{N}" "bn")
   (?𝐎 "\\mathbf{O}" "bo")
   (?𝐏 "\\mathbf{P}" "bp")
   (?𝐐 "\\mathbf{Q}" "bq")
   (?𝐑 "\\mathbf{R}" "br")
   (?𝐒 "\\mathbf{S}" "bs")
   (?𝐓 "\\mathbf{T}" "bt")
   (?𝐔 "\\mathbf{U}" "bu")
   (?𝐕 "\\mathbf{V}" "bv")
   (?𝐖 "\\mathbf{W}" "bw")
   (?𝐗 "\\mathbf{X}" "bx")
   (?𝐘 "\\mathbf{Y}" "by")
   (?𝐙 "\\mathbf{Z}" "bz")
   (?𝐚 "\\mathbf{a}" "bA")
   (?𝐛 "\\mathbf{b}" "bB")
   (?𝐜 "\\mathbf{c}" "bC")
   (?𝐝 "\\mathbf{d}" "bD")
   (?𝐞 "\\mathbf{e}" "bE")
   (?𝐟 "\\mathbf{f}" "bF")
   (?𝐠 "\\mathbf{g}" "bG")
   (?𝐡 "\\mathbf{h}" "bH")
   (?𝐢 "\\mathbf{i}" "bI")
   (?𝐣 "\\mathbf{j}" "bJ")
   (?𝐤 "\\mathbf{k}" "bK")
   (?𝐥 "\\mathbf{l}" "bL")
   (?𝐦 "\\mathbf{m}" "bM")
   (?𝐧 "\\mathbf{n}" "bN")
   (?𝐨 "\\mathbf{o}" "bO")
   (?𝐩 "\\mathbf{p}" "bP")
   (?𝐪 "\\mathbf{q}" "bQ")
   (?𝐫 "\\mathbf{r}" "bR")
   (?𝐬 "\\mathbf{s}" "bS")
   (?𝐭 "\\mathbf{t}" "bT")
   (?𝐮 "\\mathbf{u}" "bU")
   (?𝐯 "\\mathbf{v}" "bV")
   (?𝐰 "\\mathbf{w}" "bW")
   (?𝐱 "\\mathbf{x}" "bX")
   (?𝐲 "\\mathbf{y}" "bY")
   (?𝐳 "\\mathbf{z}" "bZ")

   (?𝔄 "\\mathfrak{A}" "fa")
   (?𝔅 "\\mathfrak{B}" "fb")
   (?ℭ "\\mathfrak{C}" "fc")
   (?𝔇 "\\mathfrak{D}" "fd")
   (?𝔈 "\\mathfrak{E}" "fe")
   (?𝔉 "\\mathfrak{F}" "ff")
   (?𝔊 "\\mathfrak{G}" "fg")
   (?𝔍 "\\mathfrak{J}" "fj")
   (?𝔎 "\\mathfrak{K}" "fk")
   (?𝔏 "\\mathfrak{L}" "fl")
   (?𝔐 "\\mathfrak{M}" "fm")
   (?𝔑 "\\mathfrak{N}" "fn")
   (?𝔒 "\\mathfrak{O}" "fo")
   (?𝔓 "\\mathfrak{P}" "fp")
   (?𝔔 "\\mathfrak{Q}" "fq")
   (?𝔖 "\\mathfrak{S}" "fs")
   (?𝔗 "\\mathfrak{T}" "ft")
   (?𝔘 "\\mathfrak{U}" "fu")
   (?𝔙 "\\mathfrak{V}" "fv")
   (?𝔚 "\\mathfrak{W}" "fw")
   (?𝔛 "\\mathfrak{X}" "fx")
   (?𝔜 "\\mathfrak{Y}" "fy")
   (?𝔞 "\\mathfrak{a}" "fA")
   (?𝔟 "\\mathfrak{b}" "fB")
   (?𝔠 "\\mathfrak{c}" "fC")
   (?𝔡 "\\mathfrak{d}" "fD")
   (?𝔢 "\\mathfrak{e}" "fE")
   (?𝔣 "\\mathfrak{f}" "fF")
   (?𝔤 "\\mathfrak{g}" "fG")
   (?𝔥 "\\mathfrak{h}" "fH")
   (?𝔦 "\\mathfrak{i}" "fI")
   (?𝔧 "\\mathfrak{j}" "fJ")
   (?𝔨 "\\mathfrak{k}" "fK")
   (?𝔩 "\\mathfrak{l}" "fL")
   (?𝔪 "\\mathfrak{m}" "fM")
   (?𝔫 "\\mathfrak{n}" "fN")
   (?𝔬 "\\mathfrak{o}" "fO")
   (?𝔭 "\\mathfrak{p}" "fP")
   (?𝔮 "\\mathfrak{q}" "fQ")
   (?𝔯 "\\mathfrak{r}" "fR")
   (?𝔰 "\\mathfrak{s}" "fS")
   (?𝔱 "\\mathfrak{t}" "fT")
   (?𝔲 "\\mathfrak{u}" "fU")
   (?𝔳 "\\mathfrak{v}" "fV")
   (?𝔴 "\\mathfrak{w}" "fW")
   (?𝔵 "\\mathfrak{x}" "fX")
   (?𝔶 "\\mathfrak{y}" "fY")
   (?𝔷 "\\mathfrak{z}" "fZ")

   (?𝓐 "\\mathcal{A}" "ca")
   (?𝓑 "\\mathcal{B}" "cb")
   (?𝓒 "\\mathcal{C}" "cc")
   (?𝓓 "\\mathcal{D}" "cd")
   (?𝓔 "\\mathcal{E}" "ce")
   (?𝓕 "\\mathcal{F}" "cf")
   (?𝓖 "\\mathcal{G}" "cg")
   (?𝓗 "\\mathcal{H}" "ch")
   (?𝓘 "\\mathcal{I}" "ci")
   (?𝓙 "\\mathcal{J}" "cj")
   (?𝓚 "\\mathcal{K}" "ck")
   (?𝓛 "\\mathcal{L}" "cl")
   (?𝓜 "\\mathcal{M}" "cm")
   (?𝓝 "\\mathcal{N}" "cn")
   (?𝓞 "\\mathcal{O}" "co")
   (?𝓟 "\\mathcal{P}" "cp")
   (?𝓠 "\\mathcal{Q}" "cq")
   (?𝓡 "\\mathcal{R}" "cr")
   (?𝓢 "\\mathcal{S}" "cs")
   (?𝓣 "\\mathcal{T}" "ct")
   (?𝓤 "\\mathcal{U}" "cu")
   (?𝓥 "\\mathcal{V}" "cv")
   (?𝓦 "\\mathcal{W}" "cw")
   (?𝓧 "\\mathcal{X}" "cx")
   (?𝓨 "\\mathcal{Y}" "cy")
   (?𝓩 "\\mathcal{Z}" "cz")

   (?𝛼 "\\alpha" "ga")
   (?𝛽 "\\beta" "gb")
   (?𝛿 "\\delta" "gd")
   (?𝜀 "\\varepsilon" "ge")
   (?𝜙 "\\phi" "gf")
   (?𝛾 "\\gamma" "gg")
   (?𝜂 "\\eta" "gh")
   (?𝜅 "\\kappa" "gk")
   (?λ "\\lambda" "gl")
   (?𝜇 "\\mu" "gm")
   (?𝜈 "\\nu" "gn")
   (?𝜔 "\\omega" "go")
   (?π "\\pi" "gp")
   (?𝜃 "\\theta" "gq")
   (?𝜌 "\\rho" "gr")
   (?𝜎 "\\sigma" "gs")
   (?𝜏 "\\tau" "gt")
   (?𝜐 "\\upsilon" "gu")
   (?𝜉 "\\xi" "gw")
   (?𝜒 "\\chi" "gx")
   (?𝛹 "\\psi" "gy")
   (?𝜁 "\\zeta" "gz")

   (?Δ "\\Delta" "gD")
   (?Φ "\\Phi" "gF")
   (?Γ "\\Gamma" "gG")
   (?Λ "\\Lambda" "gL")
   (?Ω "\\Omega" "gO")
   (?Π "\\Pi" "gP")
   (?Θ "\\Theta" "gQ")
   (?Σ "\\Sigma" "gS")
   (?Υ "\\Upsilon" "gU")
   (?Ξ "\\Xi" "gW")
   (?Ψ "\\Psi" "gY")

   (?∞ "\\infty" "infty")
   (?𝜕 "\\partial" "ggd" "pp")
   (?𝜖 "\\epsilon" "gge")
   (?𝜑 "\\varphi" "ggf")

   (?ℓ "\\ell" "ell")
   (?∫ "\\int" "int")
   (?⨖ "\\sqint" "sqint")
   (?∮ "\\oint" "oint")
   (?⨑ "\\varointclockwise" "ointclockwise")
   (?∱ "\\varointctrclockwise" "ointclockwise")
   (?∬ "\\iint" "iint")
   (?∭ "\\iiint" "iiint")
   (?⨌ "\\iiint" "iiiint")
   (?∈ "\\in" "in")
   (?⊗ "\\otimes" "ot")
   (?× "\\times" "ox")
   (?∘ "\\circ" "oc")
   (?⮽ "\\boxtimes" "boxtimes" "boxprod")
   (?⧄ "\\squarediv" "lift" "lifts")
   (?⋯ "\\cdots" "cdots")
   (?… "\\dots" "dots")
   (?· "\\cdot" "cdot")
   (?⋄ "\\diamond" "dmnd" "diamond")
   (?ꞏ "\\bullet" "blt" "bullet")
   (?∅ "\\varnothing" "empty")
   (?⊆ "\\subseteq" "sse")
   (?∐ "\\coprod" "coprod")
   (?∏ "\\prod" "prod")
   (?⨿ "\\amalg" "amalg" "cop")
   (?∀ "\\forall" "forall" "fa")
   (?∃ "\\exists" "exists" "ex")
   (?♭ "\\flat" "flat" "flt")
   (?♯ "\\sharp" "sharp" "shrp")
   (?⋔ "\\pitchfork" "cotens" "cotns" "ctns" "cotensor" "pitchfork")
   (?∪ "\\cup" "union" "cup")
   (?≔ "\\coloneqq" "defn" "define")
   (?⊣ "\\dashv" "-|")
   (?⊢ "\\vdash" "|-")
   (?⊥ "\\perp" "perp")
   (?⊤ "\\top" "top")
   (?↓ "\\downarrow" "ard" "downarrow")
   (?⊸ "\\multimap" "mmap" )
   (?⟜ "\\multimapinv" "rmmap")

   (?▷ "\\vartriangleright" "|>")
   (?◁ "\\vartriangleleft" "<|")
   ;; (?→ "\\rightarrow" "->")             ; →
   ;; (?⟶ "\\longrightarrow" "-->")        ; ⟶
   (?→ "\\rightarrow" "->")             ; →
   (?⟶ "\\longrightarrow" "-->")        ; ⟶
   (?⇒ "\\Rightarrow" "=>")
   (?⟹ "\\Longrightarrow" "==>")
   (? "\\twoheadrightarrow" "->>")     ; ↠
   (? "\\rightarrowtail" ">->")        ; ↪
   (?← "\\leftarrow" "<-")
   (?⟼ "\\mapsto" "|->")
   (?⋀ "\\bigwedge" "band")
   (?⋁ "\\bigvee" "bor")
   (?∧ "\\vee" "sand")
   (?∨ "\\wedge" "sor")
   )
 '(("geq" "\\geq")
   ("sk" "\\Sk")
   ("ob" "\\Ob")
   ("set" "\\Set")
   ;; ("sset" "\\sSet")
   ("hom" "\\Hom")
   ("Hom" "\\Hom")
   ("uhom" "\\uHom")
   ("ima" "\\Ima")
   ("id" "\\id")
   ("ar" "\\ar")
   ("psh" "\\Psh")
   ("rfib" "\\RFib")
   ("hc" "\\hc")
   ("path" "\\Path")
   ("grp" "\\Group")
   ("cat" "\\Cat")
   ("grph" "\\Grph" "graph")
   ("N" "\\N")
   ("proc" "\\Proc")
   ("stacks" "\\Stacks")
   ("terms" "\\Terms")
   ("un" "\\Un")
   ("fun" "\\Fun")
   ("nat" "\\Nat" "natural")
   ("and" "\\textnormal{ and }")
   ("if" "\\textnormal{ if }")
   ("for" "\\textnormal{ for }")
   ("scat" "\\Cat_{Δ}" "simplicial category")
   ("scats" nil "simplicial categories")
   ("sset" "\\Set_{Δ}" "simplicial set")
   ("ssets" nil "simplicial sets")
   ("sfun" nil "simplicial functor")
   ("sing" "\\Sing")
   )
 (string-join '("\\DeclareMathOperator{\\Hom}{Hom}"
                "\\DeclareMathOperator{\\sSet}{sSet}"
                "\\DeclareMathOperator{\\Ob}{Ob}"
                "\\DeclareMathOperator{\\Mor}{Mor}"
                "\\DeclareMathOperator{\\Cat}{Cat}"
                "\\DeclareMathOperator{\\Chu}{Chu}"
                "\\DeclareMathOperator{\\ev}{ev}"
                "\\DeclareMathOperator{\\tw}{tw}"
                "\\DeclareMathOperator{\\Fun}{Fun}"
                "\\DeclareMathOperator{\\Set}{Set}"
                "\\DeclareMathOperator{\\Proc}{Proc}"
                "\\DeclareMathOperator{\\Stacks}{Stacks}"
                "\\DeclareMathOperator{\\Terms}{Terms}"
                "\\DeclareMathOperator{\\Sk}{Sk}"
                "\\DeclareMathOperator{\\pr}{pr}"
                "\\DeclareMathOperator{\\op}{op}"
                "\\DeclareMathOperator{\\lax}{lax}"
                "\\DeclareMathOperator{\\Ar}{Ar}"
                "\\DeclareMathOperator{\\Grph}{Grph}"
                "\\DeclareMathOperator{\\Nat}{Nat}"
                "\\DeclareMathOperator{\\N}{N}"
                "\\DeclareMathOperator{\\dom}{dom}"
                "\\DeclareMathOperator{\\cod}{cod}"
                "\\DeclareMathOperator{\\Ima}{Im}"
                "\\DeclareMathOperator{\\id}{id}"
                "\\DeclareMathOperator{\\RFib}{RFib}"
                "\\DeclareMathOperator{\\Psh}{Psh}"
                "\\DeclareMathOperator{\\hc}{hc}"
                "\\DeclareMathOperator{\\Path}{Path}"
                "\\DeclareMathOperator{\\Group}{Group}"
                "\\DeclareMathOperator{\\Un}{Un}"
                "\\DeclareMathOperator{\\Sing}{Sing}"
                "\\DeclareMathOperator{\\uHom}{\\underline{Hom\\kern-.05em}\\kern.1em\}"
                "\\newlength\\squareheight
  \\setlength\\squareheight{6.75pt}"
                "\\newcommand\\squareslash{\\tikz{\\draw (0,0) rectangle (\\squareheight,\\squareheight);\\draw(0,0) -- (\\squareheight,\\squareheight)}}
  \\DeclareMathOperator\\squarediv{\\>\\squareslash\\>}"
                "\\newcommand{\\plus}{+}"
                ;; Colimit & Limit commands from
                ;; https://tex.stackexchange.com/questions/284059/new-command-for-filtered-colimits-and-limits
                "\\makeatletter"
                "\\newcommand{\\colim@}[2]{%"
                "  \\vtop{\\m@th\\ialign{##\\cr"
                "    \\hfil$#1\\operator@font colim$\\hfil\\cr"
                "    \\noalign{\\nointerlineskip\\kern1.5\\ex@}#2\\cr"
                "    \\noalign{\\nointerlineskip\\kern-\\ex@}\\cr}}%"
                "}"
                "\\newcommand{\\colim}{%"
                "  \\mathop{\\mathpalette\\colim@{\\rightarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                "}"
                "\\renewcommand{\\varprojlim}{%"
                "  \\mathop{\\mathpalette\\varlim@{\\leftarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                "}"
                "\\renewcommand{\\varinjlim}{%"
                "  \\mathop{\\mathpalette\\varlim@{\\rightarrowfill@\\scriptscriptstyle}}\\nmlimits@"
                "}"
                "\\makeatother"

                "\\newcommand*\\circled[1]{\\,\\tikz[baseline=(char.base)]{"
                "\\node[shape=circle,fill,inner sep=0pt] (char) {\\textcolor{white}{\\textit{#1}}};}\\,}"

                ;; Making theorem environments
                "\\newtheorem{definition}{Definition}"
                "\\newtheorem{theorem}{Theorem}"
                "\\newtheorem{lemma}{Lemma}"
                "\\newtheorem{remark}{Remark}"
                "\\newtheorem{examples}{Example}"
                "\\newtheorem{remark}{Remark}"
                "\\newenvironment{proof}{\\paragraph{Proof:}}{\\hfill$\\square$}"
                "\\numberwithin{theorem}{section}"
                "\\numberwithin{definition}{section}"
                "\\numberwithin{lemma}{section}"
                "\\numberwithin{remark}{section}"
                )
              "\n"))

(setq lattie-unicode--deprecated-char-alist
      '(
        (? ?→)
        (? ?⟶)
        (? ?↠)
        (? ?↪)
        ))
