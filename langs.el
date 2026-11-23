;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Language pack. Autoload only. Same faces. Open a file → highlight.

(defun my/add-ft (pattern mode)
  (when (and (fboundp mode) (stringp pattern))
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun my/drop-ft (pattern)
  "Remove an auto-mode entry whose car equals PATTERN."
  (setq auto-mode-alist
        (delq nil
              (mapcar (lambda (c)
                        (and (not (equal (car-safe c) pattern)) c))
                      auto-mode-alist))))

(defvar-local my/rich-filled nil
  "Non-nil when this buffer already has the shared extra highlighters.")

(defun my/string-rules (comment)
  "One-line strings that have a closing quote. COMMENT is not a string."
  (let ((rules nil))
    (unless (equal comment "\"")
      (push '("\"\\(?:\\\\.\\|[^\"\n]\\)*\"" 0 'font-lock-string-face) rules))
    (unless (equal comment "'")
      (push '("'\\(?:\\\\.\\|[^'\n]\\)*'" 0 'font-lock-string-face) rules))
    (unless (equal comment "`")
      (push '("`\\(?:\\\\.\\|[^`\n]\\)*`" 0 'font-lock-string-face) rules))
    (nreverse rules)))

(defun my/face-fg (face)
  "Foreground of FACE, or nil when it has none."
  (cond ((and (symbolp face) (facep face))
         (face-attribute face :foreground nil 'default))
        ((and (consp face) (symbolp (car face)) (facep (car face)))
         (face-attribute (car face) :foreground nil 'default))
        (t nil)))

(defun my/kept-face-p (face)
  "Non-nil when FACE is a string, comment, or doc color that must stay."
  (let ((name (cond ((symbolp face) (symbol-name face))
                    ((and (consp face) (symbolp (car face))) (symbol-name (car face)))
                    (t ""))))
    (and (not (string-match-p "operator\\|-op\\'" name))
         (string-match-p "string\\|comment\\|\\bdoc\\b\\|regexp" name))))

(defun my/bare-face-p (face)
  "Non-nil when FACE is missing, a variable, or still plain text."
  (or (null face)
      (eq face 'font-lock-variable-name-face)
      (and (consp face) (memq 'font-lock-variable-name-face face)
           (not (my/kept-face-p face)))
      (let ((fg (my/face-fg face)))
        (and (stringp fg)
             (member (downcase fg) '("#f0f0f0" "#ffffff"))))))

(defvar-local my/syn-pos nil)
(defvar-local my/syn-state nil)

(defun my/syntax-protected-p (pos)
  "Non-nil when POS is inside a string or a comment.
Reads syntax already computed for this pass, and does not
start another fontification."
  (save-excursion
    (save-match-data
      (let ((syntax-propertize-function nil)
            (parse-sexp-lookup-properties t))
        (when (or (null my/syn-pos) (< pos my/syn-pos))
          (setq my/syn-pos (point-min)
                my/syn-state nil))
        (setq my/syn-state (parse-partial-sexp my/syn-pos pos nil nil my/syn-state))
        (setq my/syn-pos pos)
        (or (nth 3 my/syn-state) (nth 4 my/syn-state))))))

(defun my/upgrade (subexp face)
  "FACE when this match is bare. Strings, comments and keywords stay."
  (let ((have (get-text-property (match-beginning subexp) 'face)))
    (cond ((or (my/syntax-protected-p (match-beginning subexp))
               (my/kept-face-p have)
               (not (my/bare-face-p have)))
           have)
          (t face))))

(defun my/as-function (subexp)
  "Function color. A keyword, type, string or comment stays as it is."
  (let ((have (get-text-property (match-beginning subexp) 'face)))
    (cond ((or (my/syntax-protected-p (match-beginning subexp))
               (my/kept-face-p have))
           have)
          ((and (symbolp have)
                (string-match-p "keyword\\|\\btype\\|constant\\|preprocessor"
                                (symbol-name have)))
           have)
          ((memq have '(font-lock-keyword-face font-lock-type-face
                        font-lock-constant-face font-lock-preprocessor-face))
           have)
          (t 'font-lock-function-name-face))))

(defun my/paint (subexp face)
  "FACE unless this match is a string or a comment."
  (let ((have (get-text-property (match-beginning subexp) 'face)))
    (if (or (my/syntax-protected-p (match-beginning subexp))
            (my/kept-face-p have))
        have face)))

(defun my/as-decorator (subexp)
  "Function color for a decorator name. Strings and comments stay."
  (let ((have (get-text-property (match-beginning subexp) 'face)))
    (if (or (my/syntax-protected-p (match-beginning subexp))
            (my/kept-face-p have))
        have 'font-lock-function-name-face)))

(defun my/mark-bare-string (limit)
  "Match one quoted string whose characters are still bare."
  (let (found)
    (while (and (not found) (< (point) limit))
      (if (re-search-forward
           "\\(?:\"\\(?:\\\\.\\|[^\"\n]\\)*\"\\|'\\(?:\\\\.\\|[^'\n]\\)*'\\|`\\(?:\\\\.\\|[^`\n]\\)*`\\)"
           limit t)
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (bare t))
            (while (and bare (< beg end))
              (unless (my/bare-face-p (get-text-property beg 'face))
                (setq bare nil))
              (setq beg (1+ beg)))
            (if bare
                (setq found t)
              (goto-char (1+ (match-beginning 0)))))
        (goto-char limit)))
    found))

(defconst my/fill-rules-head
  '(("\\(?:^\\|[^[:alnum:]_]\\)\\(0[xX][0-9A-Fa-f]+\\|0[bB][01]+\\|[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[eE][+-]?[0-9]+\\)?[fFdDlLmM]?\\)\\(?:[^[:alnum:]_]\\|$\\)"
     1 (my/paint 1 'font-lock-number-face) t)
    ("\\_<\\(?:true\\|false\\|null\\|nil\\|undefined\\)\\_>"
     0 (my/paint 0 'font-lock-constant-face) t)
    ("@\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
     1 (my/as-function 1) t)
    ("\\(\\_<[[:alpha:]_][[:alnum:]_]*\\)[ \t]*("
     1 (my/as-function 1) t)
    ("\\.\\(\\_<\\(?:\\sw\\|\\s_\\)+\\_>\\)"
     1 (my/upgrade 1 'font-lock-property-name-face) t))
  "Numbers, calls and fields. Strings, comments and keywords keep their color.")

(defconst my/cased-code-rules
  '(("\\_<[A-Z][A-Z0-9_]+\\_>"
     0 (my/upgrade 0 'font-lock-constant-face) t))
  "ALL_CAPS constants.
Skipped when the mode ignores case, or every word turns gold.")

(defconst my/fill-rules-tail
  '(("[[:alpha:]_][[:alnum:]_]*"
     (0 (unless (or (get-text-property (match-beginning 0) 'face)
                    (my/syntax-protected-p (match-beginning 0)))
          'font-lock-variable-name-face)
        keep)))
  "Every remaining word outside strings and comments.")

(defun my/fill-rules (&optional fold)
  "Patterns that finish a language the way Cursor colors it.
FOLD means the mode matches keywords in either case, so ALL_CAPS is not safe."
  (append my/fill-rules-head
          (unless fold my/cased-code-rules)
          my/fill-rules-tail))

(defvar my/face-stamp 0
  "Face count at the last palette pass. A later mode adds faces and runs it again.")

(defun my/cursor-recolor-faces ()
  "Put every language face on the Cursor palette. UI faces stay as they are."
  (let ((n (length (face-list))))
    (when (> n my/face-stamp)
      (dolist (face (face-list))
        (unless (get face 'my/cursor-colored)
          (let ((name (symbol-name face)))
            (put face 'my/cursor-colored t)
            (unless (string-match-p
                     "\\`\\(font-lock-\\|rainbow-\\|show-paren\\|custom-\\|line-number\\|mode-line\\|header-line\\|completions-\\|eldoc-\\|button\\|variable-pitch\\|diff-\\|flymake-\\|hl-\\|fringe\\|cursor\\|region\\|default\\|shadow\\|link\\|tooltip\\|minibuffer\\)"
                     name)
              (let ((color
                     (cond
                      ((string-match-p "operator\\|-op\\'" name) "#FFFFFF")
                      ((string-match-p "string" name) "#AA4477")
                      ((string-match-p "comment\\|\\bdoc\\b\\|line-number\\|interactive-output"
                                       name)
                       "#335566")
                      ((string-match-p "function\\|method" name) "#BB9955")
                      ((string-match-p "tuareg-font-lock-error\\|tuareg-font-double-semicolon"
                                       name)
                       "#E34671")
                      ((string-match-p "extension-node\\|pragma\\|preprocessor" name) "#77DD77")
                      ((string-match-p "fstar-universe" name) "#77BBDD")
                      ((string-match-p "keyword\\|builtin\\|annotation\\|macro\\|governing\\|multistage\\|fstar-structure"
                                       name)
                       "#779977")
                      ((string-match-p "\\bnumber\\b" name) "#FFDD88")
                      ((string-match-p "constant\\|boolean\\|constructor" name) "#FF8899")
                      ((string-match-p "property\\|attribute\\|field" name) "#7777AA")
                      ((string-match-p "bracket\\|delimiter\\|punctuation" name) "#FFFFFF")
                      ((string-match-p "variable\\|parameter" name) "#7799CC")
                      ((string-match-p "type\\|class" name) "#77BBDD"))))
                ;; A face that already uses this palette keeps its color.
                ;; Otherwise `class` in PHP would turn from keyword to type.
                (when (and color
                           (not (my/cursor-hex-p face))
                           (not (my/same-hex face color)))
                  (if (string-match-p "comment\\|\\bdoc\\b" name)
                      (set-face-attribute face nil :foreground color :italic t)
                    (set-face-attribute face nil :foreground color))))))))
      (setq my/face-stamp n))))

(defun my/same-hex (face color)
  (let ((fg (my/face-fg face)))
    (and (stringp fg) (string-equal (downcase fg) (downcase color)))))

(defun my/cursor-hex-p (face)
  "Non-nil when FACE is already one of the Cursor colors."
  (let ((fg (my/face-fg face)))
    (and (stringp fg)
         (member (downcase fg)
                 '("#779977" "#bb9955" "#ffdd88" "#ff8899" "#7799cc"
                   "#77dd77" "#7777aa" "#335566" "#aa4477" "#77bbdd"
                   "#e34671" "#f8c762" "#88c0d0" "#81a1c1" "#87a6c4")))))

(defun my/cursor-fill ()
  "Paint tokens the major mode left bare, in Cursor's colors."
  (condition-case err
      (when (and (not my/rich-filled)
                 (not (minibufferp))
                 (not (eq major-mode 'fundamental-mode))
                 (not (derived-mode-p 'special-mode))
                 (not (bound-and-true-p so-long-minor-mode))
                 (< (buffer-size) 400000)
                 (or font-lock-defaults (derived-mode-p 'prog-mode)))
        (setq my/rich-filled t)
        ;; A few ESS dialects build keywords from an unset variable.
        ;; Leave their own highlighting in place.
        (unless (and (boundp 'ess-font-lock-keywords)
                     (null ess-font-lock-keywords))
          (unless font-lock-defaults
            (setq-local font-lock-defaults '(nil)))
          (my/cursor-recolor-faces)
          (let ((rules (my/fill-rules font-lock-keywords-case-fold-search)))
            ;; A mode that already treats " as a string does not need a
            ;; second scan. That scan walks every character.
            (unless (eq (char-syntax ?\") ?\")
              (push '(my/mark-bare-string (0 'font-lock-string-face t)) rules))
            ;; Prose keeps its own words. Code also colors bare identifiers.
            (when (derived-mode-p 'text-mode)
              (setq rules (butlast rules)))
            (setq rules (append rules (my/popular-rules)))
            (font-lock-add-keywords nil rules 'append))
          ;; The first fontify has not painted anything yet, so the new
          ;; rules are already in that pass. A later mode change still
          ;; has old faces, and those need one flush.
          (when (text-property-not-all (point-min) (point-max) 'face nil)
            (font-lock-flush))))
    (error (message "cursor-fill: %s" err))))
(add-hook 'after-change-major-mode-hook #'my/cursor-fill t)
(advice-add 'font-lock-set-defaults :after #'my/cursor-fill)

(defun my/kw-rule (words face)
  "One keyword rule. Strings and comments keep their color."
  (list (concat "\\_<" (regexp-opt words t) "\\_>")
        0 `(my/paint 0 ',face) t))

(defun my/popular-rules ()
  "Extra patterns for languages whose own mode misses a common form.
Compiled regexps only. Empty for every other language."
  (cond
   ((derived-mode-p 'python-mode 'python-ts-mode)
    (list
     '("\\_<class\\_>" 0 (my/paint 0 'font-lock-keyword-face) t)
     '("^\\s-*\\_<\\(match\\|case\\)\\_>" 1 (my/paint 1 'font-lock-keyword-face) t)
     '("@[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)" 1 (my/as-decorator 1) t)))
   ((derived-mode-p 'js-mode 'js-jsx-mode 'rjsx-mode 'js-ts-mode)
    (list
     (my/kw-rule '("from" "as" "of") 'font-lock-keyword-face)
     '("#\\([A-Za-z_][A-Za-z0-9_]*\\)"
       1 (my/upgrade 1 'font-lock-property-name-face) t)))
   ((derived-mode-p 'typescript-mode 'typescript-tsx-mode
                    'typescript-ts-mode 'tsx-ts-mode)
    (list
     (my/kw-rule '("from" "as" "of" "satisfies" "keyof" "infer")
                 'font-lock-keyword-face)
     '("#\\([A-Za-z_][A-Za-z0-9_]*\\)"
       1 (my/upgrade 1 'font-lock-property-name-face) t)))
   ((derived-mode-p 'ruby-mode 'ruby-ts-mode)
    (list
     '("@\\{1,2\\}\\([A-Za-z_][A-Za-z0-9_]*\\)"
       1 (my/paint 1 'font-lock-variable-name-face) t)
     '("\\(?:^\\|[^[:alnum:]_]\\):\\([A-Za-z_][A-Za-z0-9_?!]*\\)"
       1 (my/paint 1 'font-lock-constant-face) t)))
   ((derived-mode-p 'emacs-lisp-mode 'lisp-mode 'lisp-interaction-mode
                    'scheme-mode 'clojure-mode 'fennel-mode)
    (list
     '("\\(?:^\\|[^[:alnum:]_]\\):\\{1,2\\}\\([A-Za-z_][A-Za-z0-9_:+-]*\\)"
       1 (my/paint 1 'font-lock-constant-face) t)
     '("(\\s-*\\(\\_<[[:alpha:]_][[:alnum:]_]*\\_>\\)"
       1 (my/as-function 1) t)))
   ((derived-mode-p 'erlang-mode)
    (list
     '("\\_<[a-z][A-Za-z0-9_]*\\_>"
       0 (my/upgrade 0 'font-lock-constant-face) t)))
   ((derived-mode-p 'csharp-mode 'csharp-ts-mode)
    (list
     '("\\[\\s-*\\([A-Z][A-Za-z0-9_.]*\\)"
       1 (my/as-decorator 1) t)))
   ((derived-mode-p 'css-mode 'css-ts-mode)
    (list
     '("--\\([A-Za-z_][A-Za-z0-9_-]*\\)"
       1 (my/upgrade 1 'font-lock-property-name-face) t)))
   ((derived-mode-p 'yaml-mode 'yaml-ts-mode)
    (list
     '("^[ \t]*\\(?:- \\)?\\([A-Za-z_][A-Za-z0-9_-]*\\)[ \t]*:"
       1 (my/upgrade 1 'font-lock-property-name-face) t)))))

(defun my/rich-keywords (comment words &optional extra fold)
  "Strings, comments, keywords, then the shared Cursor fill.
FOLD is non-nil when KEYWORDS match in either case."
  (append
   (my/string-rules comment)
   (when (and (stringp comment) (> (length comment) 0))
     (list (list (concat (regexp-quote comment) ".*")
                 0 'font-lock-comment-face 'keep)))
   (unless (member comment '("#" "##"))
     '(("^\\s-*#\\s-*[A-Za-z_][A-Za-z0-9_]*" 0 'font-lock-preprocessor-face keep)))
   (list (cons (concat "\\_<" (regexp-opt words t) "\\_>")
               'font-lock-keyword-face))
   extra
   (my/fill-rules fold)))

(defun my/own-mode (mode name comment words &optional fold)
  "Give MODE its own highlighting. Skip if MODE already exists.
Strings, comments, numbers and calls are included.
The keyword regexp is built the first time the mode runs.
FOLD matches those keywords in either case."
  (unless (or (fboundp mode) (null words))
    (eval
     `(define-derived-mode ,mode prog-mode ,name
        ,(format "%s source." name)
        (setq-local comment-start ,comment
                    comment-end ""
                    indent-tabs-mode nil
                    tab-width 4
                    my/rich-filled t)
        (modify-syntax-entry ?\" ".")
        (modify-syntax-entry ?\' ".")
        (modify-syntax-entry ?\` ".")
        (setq-local font-lock-defaults
                    (list (my/rich-keywords ,comment ',words nil ,fold) nil ,fold)))
     t)
    t))

(defun my/define-lite (mode name comment words)
  "Highlight-only major mode. Skip if MODE already exists."
  (ignore-errors (my/own-mode mode name comment words)))


(defconst my/erabasic-keywords
  '("PRINT" "PRINTL" "PRINTW" "PRINTV" "PRINTS" "PRINTFORM" "PRINTFORML"
    "PRINTFORMW" "PRINTFORMV" "PRINTFORMS" "PRINTC" "PRINTLC" "PRINTSINGLE"
    "DRAWLINE" "CUSTOMDRAWLINE" "REUSELASTLINE" "CLEARLINE" "WAIT" "TWAIT"
    "RESTART" "QUIT" "RETURN" "RETURNF" "GOTO" "JUMP" "CALL" "CALLF"
    "CALLFORM" "CALLFORMF" "TRYCALL" "TRYCALLFORM" "TRYCCALL" "CATCH"
    "ENDCATCH" "BEGIN" "BREAK" "CONTINUE" "REPEAT" "REND" "FOR" "NEXT"
    "WHILE" "WEND" "SIF" "IF" "ELSEIF" "ELSE" "ENDIF" "SELECTCASE" "CASE"
    "CASEELSE" "CASEIS" "ENDSELECT" "VARSET" "VARSIZE" "SPLIT" "POWER"
    "SQRT" "ABS" "SIGN" "LIMIT" "INRANGE" "RAND" "STRLEN" "STRLENS"
    "SUBSTRING" "STRFIND" "REPLACE" "UNICODE" "TOSTR" "TOINT" "ISNUMERIC"
    "LOADDATA" "SAVEDATA" "CHKDATA" "SETCOLOR" "RESETCOLOR" "SETBGCOLOR"
    "FONTSTYLE" "FONTBOLD" "SETFONT" "BAR" "HTML_PRINT" "ADDCHARA"
    "DELCHARA" "GETCHARA" "EXISTCSV" "FINDCHARA" "INPUT" "INPUTS"
    "ONEINPUT" "TINPUT" "MASTER" "PLAYER" "TARGET" "ASSI" "CHARANUM"
    "CFLAG" "TALENT" "BASE" "MAXBASE" "ABL" "EXP" "MARK" "PALAM" "JUEL"
    "EQUIP" "FLAG" "TFLAG" "ITEM" "TIMES"))

(define-derived-mode erabasic-mode prog-mode "ERABasic"
  "Emuera / era* game scripts (.erb .erh)."
  (setq-local comment-start ";"
              indent-tabs-mode nil
              tab-width 4)
  (setq-local my/rich-filled t)
  (modify-syntax-entry ?\" ".")
  (modify-syntax-entry ?\' ".")
  (modify-syntax-entry ?\` ".")
  (setq-local font-lock-defaults
              (list (my/rich-keywords
                     ";" my/erabasic-keywords
                     '(("^#[A-Za-z_]+" 0 'font-lock-preprocessor-face keep)
                       ("^@[A-Za-z0-9_]+" 0 'font-lock-function-name-face keep)
                       ("^\\$[A-Za-z0-9_]+" 0 'font-lock-constant-face keep)))
                    nil nil)))

(my/define-lite 'shaderlab-mode "ShaderLab" "//"
                '("Shader" "Properties" "SubShader" "Pass" "Tags" "Fallback"
                  "CustomEditor" "Category" "LOD" "Blend" "ZWrite" "ZTest"
                  "Cull" "Offset" "ColorMask" "Lighting" "Material" "SetTexture"
                  "GrabPass" "UsePass" "Name" "Fog" "Stencil" "CGPROGRAM"
                  "ENDCG" "HLSLPROGRAM" "ENDHLSL" "CGINCLUDE" "HLSLINCLUDE"
                  "Range" "Float" "Int" "Color" "Vector" "2D" "3D" "Cube"
                  "BindChannels" "AlphaTest" "SeparateSpecular" "Queue"
                  "RenderType" "IgnoreProjector" "ForceNoShadowCasting"))
(my/define-lite 'vex-mode "VEX" "//"
                '("vector" "vector2" "vector4" "matrix" "matrix3" "bsdf"
                  "dict" "string" "int" "float" "void" "export" "return"
                  "if" "else" "for" "while" "do" "break" "continue"
                  "illuminate" "illuminance" "gather" "foreach" "struct"
                  "const" "varying" "uniform"))
(my/define-lite 'rsl-mode "RSL" "//"
                '("surface" "displacement" "light" "volume" "imager"
                  "varying" "uniform" "output" "color" "point" "vector"
                  "normal" "matrix" "string" "float" "void" "if" "else"
                  "for" "while" "return" "illuminance" "solar" "extern"))
(my/define-lite 'agal-mode "AGAL" "#"
                '("mov" "add" "sub" "mul" "div" "rcp" "min" "max" "frc"
                  "sqt" "rsq" "pow" "log" "exp" "nrm" "sin" "cos" "crs"
                  "dp3" "dp4" "abs" "neg" "sat" "m33" "m44" "m34" "kil"
                  "tex" "sge" "slt" "sgn" "seq" "sne" "if" "els" "eif"))
(my/define-lite 'holyc-mode "HolyC" "//"
                '("U0" "U8" "U16" "U32" "U64" "I8" "I16" "I32" "I64" "F64"
                  "Bool" "public" "extern" "class" "union" "if" "else"
                  "for" "while" "do" "switch" "case" "default" "goto"
                  "return" "break" "continue" "sizeof" "NULL" "TRUE" "FALSE"))
(my/define-lite 'hare-mode "Hare" "//"
                '("use" "export" "fn" "let" "const" "def" "type" "struct"
                  "union" "enum" "if" "else" "for" "return" "break"
                  "continue" "defer" "match" "switch" "case" "yield"
                  "null" "void" "bool" "int" "uint" "i8" "i16" "i32" "i64"
                  "u8" "u16" "u32" "u64" "f32" "f64" "size" "uintptr" "str"
                  "true" "false" "alloc" "free"))
(my/define-lite 'chapel-lite-mode "Chapel" "//"
                '("module" "proc" "iter" "var" "const" "param" "type"
                  "class" "record" "union" "enum" "if" "then" "else"
                  "for" "forall" "coforall" "cobegin" "begin" "sync"
                  "atomic" "return" "yield" "break" "continue" "where"
                  "domain" "range" "locale" "config" "export" "extern"
                  "inline" "override" "ref" "in" "out" "inout" "true" "false"))
(my/define-lite 'ballerina-mode "Ballerina" "//"
                '("import" "as" "function" "public" "isolated" "service"
                  "resource" "listener" "client" "type" "record" "object"
                  "class" "enum" "const" "final" "var" "map" "json" "xml"
                  "error" "return" "returns" "if" "else" "while" "foreach"
                  "match" "check" "checkpanic" "fail" "transaction" "worker"
                  "from" "where" "select" "let" "in" "true" "false" "null"))
(my/define-lite 'rego-mode "Rego" "#"
                '("package" "import" "default" "as" "with" "else" "not"
                  "some" "every" "contains" "if" "in" "set" "object"
                  "true" "false" "null"))
(my/define-lite 'pkl-mode "Pkl" "//"
                '("module" "amends" "extends" "import" "as" "open" "class"
                  "typealias" "function" "const" "hidden" "local" "abstract"
                  "external" "if" "else" "when" "for" "in" "let" "read"
                  "throw" "trace" "new" "super" "this" "outer" "true"
                  "false" "null"))
(my/define-lite 'bicep-mode "Bicep" "//"
                '("targetScope" "param" "var" "resource" "module" "output"
                  "existing" "for" "in" "if" "true" "false" "null" "using"
                  "metadata" "type" "func" "import" "with" "as"))
(my/define-lite 'promql-mode "PromQL" "#"
                '("sum" "min" "max" "avg" "group" "stddev" "stdvar" "count"
                  "count_values" "bottomk" "topk" "quantile" "rate" "irate"
                  "increase" "delta" "idelta" "deriv" "predict_linear"
                  "histogram_quantile" "abs" "ceil" "floor" "clamp" "exp"
                  "ln" "log2" "log10" "sqrt" "round" "and" "or" "unless"
                  "by" "without" "on" "ignoring" "group_left" "group_right"
                  "bool" "offset"))
(my/define-lite 'prql-mode "PRQL" "#"
                '("prql" "from" "derive" "select" "filter" "aggregate"
                  "sort" "take" "join" "group" "window" "into" "let"
                  "func" "case" "true" "false" "null" "this" "side"))
(my/define-lite 'gherkin-mode "Gherkin" "#"
                '("Feature" "Background" "Scenario" "Examples" "Given"
                  "When" "Then" "And" "But" "Rule" "Example" "Outline"
                  "ScenarioOutline"))
(my/define-lite 'ahk-mode "AutoHotkey" ";"
                '("if" "else" "return" "Loop" "Break" "Continue" "Goto"
                  "Gosub" "global" "local" "static" "class" "extends"
                  "MsgBox" "Send" "SendInput" "Click" "Sleep" "WinActivate"
                  "WinWait" "SetTimer" "Hotkey" "Gui" "true" "false"))
(my/define-lite 'autoit-mode "AutoIt" ";"
                '("If" "Then" "Else" "ElseIf" "EndIf" "For" "Next" "While"
                  "WEnd" "Do" "Until" "Switch" "Case" "EndSwitch" "Func"
                  "EndFunc" "Return" "Global" "Local" "Const" "Dim"
                  "True" "False" "And" "Or" "Not" "MsgBox" "Sleep"))
(my/define-lite 'raku-mode "Raku" "#"
                '("use" "unit" "module" "class" "role" "grammar" "has"
                  "method" "sub" "submethod" "my" "our" "constant" "enum"
                  "if" "elsif" "else" "unless" "given" "when" "for"
                  "while" "until" "repeat" "return" "next" "last" "redo"
                  "gather" "take" "try" "CATCH" "True" "False" "Nil"))
(my/define-lite 'my-r-mode "R" "#"
                '("function" "if" "else" "for" "while" "repeat" "next"
                  "break" "in" "TRUE" "FALSE" "NULL" "NA" "Inf" "NaN"
                  "library" "require" "source" "return" "invisible"
                  "data.frame" "list" "ggplot" "aes" "mutate" "filter"
                  "summarise" "group_by"))
(my/define-lite 'basic-mode "BASIC" "'"
                '("If" "Then" "Else" "ElseIf" "End" "If" "For" "To" "Next"
                  "While" "Wend" "Do" "Loop" "Function" "Sub" "Dim" "As"
                  "Public" "Private" "Class" "Module" "Imports" "New"
                  "True" "False" "Nothing" "Return" "Exit" "Select" "Case"))
(my/define-lite 'coq-mode "Coq" ";"
                '("Require" "Import" "Export" "From" "Theorem" "Lemma"
                  "Definition" "Inductive" "Fixpoint" "Proof" "Qed" "Defined"
                  "intros" "induction" "simpl" "rewrite" "apply" "exact"
                  "destruct" "unfold" "reflexivity" "omega" "lia" "Admitted"
                  "Variable" "Hypothesis" "Context" "Section" "End" "Notation"
                  "Record" "Class" "Instance" "match" "with" "end" "fun"
                  "forall" "exists" "Type" "Prop" "Set"))
(my/define-lite 'pov-mode "POV-Ray" "//"
                '("camera" "light_source" "sphere" "box" "plane" "cylinder"
                  "cone" "torus" "blob" "union" "intersection" "difference"
                  "merge" "texture" "pigment" "normal" "finish" "interior"
                  "media" "color" "rgb" "rgbt" "look_at" "location"
                  "include" "declare" "if" "else" "end" "macro" "local"))
(my/define-lite 'kdl-lite-mode "KDL" "//"
                '("true" "false" "null" "inf" "-inf" "nan"))
(my/define-lite 'avro-idl-mode "Avro IDL" "//"
                '("protocol" "record" "enum" "error" "fixed" "array" "map"
                  "union" "throws" "import" "idl" "schema" "java" "string"
                  "bytes" "int" "long" "float" "double" "boolean" "null"
                  "void" "oneway" "decimal" "date" "timestamp_ms"))
(my/define-lite 'gemtext-mode "Gemtext" "#"
                '("https" "gemini"))
(my/define-lite 'ceylon-mode "Ceylon" "//"
                '("import" "class" "interface" "object" "given" "value"
                  "function" "void" "return" "if" "else" "for" "while"
                  "switch" "case" "else" "try" "catch" "finally" "throw"
                  "extends" "satisfies" "shared" "formal" "actual" "default"
                  "late" "variable" "true" "false" "null" "this" "super"
                  "outer" "package" "module" "alias"))

(defun my/head-chunk ()
  (buffer-substring-no-properties
   (point-min) (min (point-max) (+ (point-min) 4000))))

(defun my/mode-m ()
  "ObjC vs MATLAB/Octave for .m"
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((string-match-p "#import\\|@interface\\|@implementation\\|@property" chunk)
          (objc-mode))
         ((and (fboundp 'wolfram-mode)
               (string-match-p "(\\*\\|BeginPackage\\|::usage\\|Needs\\[" chunk))
          (wolfram-mode))
         ((or (string-match-p "^\\s-*\\(%\\|function\\>\\)" chunk)
              (and (fboundp 'matlab-is-matlab-file)
                   (matlab-is-matlab-file)))
          (matlab-mode))
         (t (objc-mode))))
    (error (objc-mode))))

(defun my/mode-v ()
  "Verilog vs Coq for .v"
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((string-match-p "\\<\\(Require Import\\|Theorem\\|Lemma\\|Inductive\\|Qed\\)\\>" chunk)
          (coq-mode))
         (t (verilog-mode))))
    (error (verilog-mode))))

(defconst my/racket-keywords
  '("define" "define-values" "define-syntax" "define-syntax-rule"
    "define-for-syntax" "define/contract" "lambda" "let" "let*"
    "letrec" "letrec-values" "let-values" "let/cc" "let/ec"
    "begin" "begin0" "if" "cond" "case" "match" "match-define"
    "match-let" "match-let*" "when" "unless" "and" "or" "set!"
    "quote" "quasiquote" "unquote" "unquote-splicing"
    "require" "provide" "module" "module+" "module*"
    "struct" "define-struct" "for" "for/list" "for/vector"
    "for/fold" "for/hash" "for/and" "for/or" "for/sum"
    "for*" "for*/list" "for*/fold" "do"
    "parameterize" "with-handlers" "raise" "error"
    "class" "send" "new" "super-new" "init" "field" "inherit"
    "public" "private" "override" "augment" "pubment"
    "contract" "values" "thread" "place" "delay" "force"
    "printf" "displayln")
  "Racket forms highlighted on top of Scheme syntax.")

(unless (fboundp 'racket-mode)
  (define-derived-mode racket-mode scheme-mode "Racket"
    "Racket source. Scheme syntax, plus Racket forms."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<" (regexp-opt my/racket-keywords t) "\\_>")
        . font-lock-keyword-face)
       ("^#lang\\_>" . font-lock-preprocessor-face)
       ("#\\(?:px\\|rx\\|hash\\(?:eqv?\\)?\\|reader\\|s\\)\\_>"
        . font-lock-builtin-face))
     'append)))
(add-to-list 'magic-mode-alist '("\\`#lang " . racket-mode))



(my/own-mode 'ada-mode "Ada" "--"
             '("abort" "abs" "abstract" "accept" "access" "aliased" "all" "and"
               "array" "at" "begin" "body" "case" "constant" "declare" "delay"
               "delta" "digits" "do" "else" "elsif" "end" "entry" "exception"
               "exit" "for" "function" "generic" "goto" "if" "in" "interface"
               "is" "limited" "loop" "mod" "new" "not" "null" "of" "or" "others"
               "out" "overriding" "package" "pragma" "private" "procedure"
               "protected" "raise" "range" "record" "rem" "renames" "requeue"
               "return" "reverse" "select" "separate" "some" "subtype"
               "synchronized" "tagged" "task" "terminate" "then" "type" "until"
               "use" "when" "while" "with" "xor"))
(my/own-mode 'agda2-mode "Agda" "--"
             '("data" "where" "module" "open" "import" "mutual" "abstract"
               "private" "postulate" "record" "field" "constructor" "infix"
               "infixl" "infixr" "let" "in" "if" "then" "else" "with" "rewrite"
               "forall" "Set" "public"))
(my/own-mode 'lean4-mode "Lean" "--"
             '("theorem" "lemma" "def" "example" "axiom" "inductive" "structure"
               "class" "instance" "namespace" "section" "end" "variable"
               "universe" "open" "export" "import" "if" "then" "else" "match"
               "with" "fun" "let" "have" "show" "from" "by" "calc" "where"))
(my/own-mode 'fstar-mode "F*" "(*"
             '("val" "let" "type" "module" "open" "assume" "assert" "effect"
               "unfold" "inline" "logic" "total" "private" "abstract" "noeq"
               "new_effect" "layered_effect" "reifiable" "reflectable"))
(my/own-mode 'idris-mode "Idris" "--"
             '("data" "where" "module" "import" "public" "export" "mutual"
               "interface" "implementation" "record" "constructor" "namespace"
               "total" "partial" "covering" "do" "let" "in" "if" "then" "else"
               "case" "of" "with" "rewrite" "impossible" "postulate"))
(my/own-mode 'julia-mode "Julia" "#"
             '("module" "baremodule" "function" "macro" "struct" "mutable"
               "abstract" "primitive" "type" "import" "using" "export" "if"
               "elseif" "else" "for" "while" "begin" "end" "let" "do" "try"
               "catch" "finally" "return" "break" "continue" "global" "local"
               "const" "quote" "where" "in" "true" "false"))
(my/own-mode 'fsharp-mode "F#" "//"
             '("let" "rec" "module" "namespace" "open" "type" "member"
               "interface" "inherit" "override" "abstract" "default" "val"
               "new" "if" "then" "else" "elif" "match" "with" "when" "function"
               "fun" "do" "done" "for" "while" "yield" "return" "async" "use"
               "mutable" "and" "of" "in" "to" "downto" "true" "false"))
(my/own-mode 'hlsl-mode "HLSL" "//"
             '("cbuffer" "tbuffer" "struct" "void" "return" "if" "else" "for"
               "while" "discard" "clip" "float" "float2" "float3" "float4"
               "int" "uint" "half" "bool" "matrix" "Texture2D" "SamplerState"
               "SV_Position" "SV_Target" "register" "numthreads"))
(my/own-mode 'glsl-mode "GLSL" "//"
             '("attribute" "varying" "in" "out" "inout" "uniform" "layout"
               "void" "return" "if" "else" "for" "while" "discard" "vec2"
               "vec3" "vec4" "mat4" "sampler2D" "gl_Position" "gl_FragColor"))
(my/own-mode 'wgsl-mode "WGSL" "//"
             '("fn" "var" "let" "const" "struct" "if" "else" "for" "while"
               "loop" "return" "switch" "case" "override" "enable" "alias"
               "vec2" "vec3" "vec4" "mat4x4" "texture_2d" "@vertex" "@fragment"
               "@compute" "@group" "@binding"))
(my/own-mode 'nix-mode "Nix" "#"
             '("let" "in" "inherit" "with" "import" "rec" "if" "then" "else"
               "assert" "or" "builtins"))
(my/own-mode 'terraform-mode "Terraform" "#"
             '("resource" "data" "variable" "output" "locals" "module"
               "provider" "terraform" "backend" "for_each" "count" "dynamic"
               "content" "lifecycle"))
(my/own-mode 'qml-mode "QML" "//"
             '("import" "property" "signal" "function" "id" "anchors" "states"
               "transitions" "Item" "Rectangle" "Text" "Image" "MouseArea"))
(my/own-mode 'solidity-mode "Solidity" "//"
             '("pragma" "import" "contract" "interface" "library" "function"
               "modifier" "event" "struct" "enum" "mapping" "public" "private"
               "internal" "external" "view" "pure" "payable" "returns" "memory"
               "storage" "calldata" "if" "else" "for" "while" "return"
               "require" "assert" "revert" "emit" "constructor"))
(my/own-mode 'prisma-mode "Prisma" "//"
             '("model" "enum" "type" "generator" "datasource" "provider"
               "relation" "fields" "references" "unique" "default" "map"))
(my/own-mode 'cython-mode "Cython" "#"
             '("cdef" "cpdef" "ctypedef" "cimport" "include" "nogil" "gil"
               "inline" "public" "readonly" "extern" "struct" "union" "enum"
               "fused" "def" "class" "if" "else" "for" "while" "return"))
(my/own-mode 'odin-mode "Odin" "//"
             '("package" "import" "proc" "struct" "enum" "union" "distinct"
               "using" "transmute" "cast" "if" "else" "when" "for" "switch"
               "case" "defer" "return" "break" "continue" "or_return" "foreign"))
(my/own-mode 'gleam-mode "Gleam" "//"
             '("import" "pub" "fn" "type" "let" "assert" "case" "if" "todo"
               "panic" "use" "as" "const" "opaque"))
(my/own-mode 'lfe-mode "LFE" ";"
             '("defun" "defmacro" "defmodule" "defrecord" "let" "case" "if"
               "cond" "when" "receive" "try" "catch" "progn" "lambda" "cons"))
(my/own-mode 'dune-mode "Dune" ";"
             '("library" "executable" "executables" "test" "tests" "install"
               "rule" "alias" "env" "include" "package" "public_name"
               "libraries" "modules" "preprocess" "pps" "names"))
(my/own-mode 'sed-mode "Sed" "#"
             '("sed"))
(my/own-mode 'reason-mode "Reason" "//"
             '("let" "rec" "module" "open" "type" "if" "else" "switch" "when"
               "fun" "for" "while" "and" "as" "in" "of" "exception" "try"
               "catch" "external" "include"))
(my/own-mode 'rescript-mode "ReScript" "//"
             '("let" "rec" "module" "open" "type" "if" "else" "switch" "when"
               "fun" "for" "while" "and" "as" "in" "of" "exception" "try"
               "catch" "external" "include" "@module" "@val"))
(my/own-mode 'powershell-mode "PowerShell" "#"
             '("function" "if" "else" "elseif" "foreach" "for" "while" "switch"
               "return" "param" "begin" "process" "end" "filter" "throw" "try"
               "catch" "finally" "class" "enum" "using" "module"))
(my/own-mode 'csharp-mode "C#" "//"
             '("using" "namespace" "class" "struct" "interface" "enum" "record"
               "public" "private" "protected" "internal" "static" "void" "if"
               "else" "for" "foreach" "while" "return" "new" "var" "async"
               "await" "try" "catch" "finally" "throw" "get" "set"))
(my/own-mode 'svelte-mode "Svelte" "//"
             '("script" "style" "if" "else" "each" "await" "then" "catch"
               "key" "slot" "export" "let" "const" "function" "import" "from"))
(my/own-mode 'wat-mode "WebAssembly" ";;"
             '("module" "func" "param" "result" "local" "export" "import"
               "call" "block" "loop" "if" "else" "end" "br" "br_if" "i32"
               "i64" "f32" "f64" "return" "drop"))
(my/own-mode 'fish-mode "Fish" "#"
             '("function" "if" "else" "end" "for" "while" "switch" "case"
               "return" "set" "and" "or" "not" "begin" "argparse" "command"
               "source" "status"))
(my/own-mode 'nasm-mode "NASM" ";"
             '("section" "global" "extern" "bits" "db" "dw" "dd" "dq" "resb"
               "equ" "times" "align" "mov" "push" "pop" "call" "ret" "jmp"
               "je" "jne" "cmp" "add" "sub" "xor" "lea" "syscall"))
(my/own-mode 'dotenv-mode "Dotenv" "#"
             '("export"))
(my/own-mode 'gitignore-mode "Gitignore" "#"
             '("gitignore"))
(my/own-mode 'caddyfile-mode "Caddyfile" "#"
             '("http" "https" "reverse_proxy" "file_server" "encode"
               "handle" "handle_path" "route" "redir" "rewrite"
               "basicauth" "tls" "log" "header" "respond" "bind"
               "php_fastcgi" "try_files" "import" "root" "uri"
               "method" "abort" "error" "vars" "matcher"))
(my/own-mode 'pip-requirements-mode "Requirements" "#"
             '("-r" "-c" "-e" "--requirement" "--constraint"
               "--editable" "--index-url" "--extra-index-url"
               "--find-links" "--no-binary" "--only-binary"))
(my/own-mode 'opencl-mode "OpenCL" "//"
             '("kernel" "__kernel" "global" "__global" "local" "__local"
               "constant" "private" "read_only" "write_only" "get_global_id"
               "barrier" "void" "if" "else" "for" "while" "return"))
(my/own-mode 'slang-mode "Slang" "//"
             '("struct" "let" "var" "if" "else" "for" "while" "return" "void"
               "float" "float2" "float3" "float4" "Texture2D" "SamplerState"
               "RWTexture2D" "static" "const" "in" "out" "inout"))
(my/own-mode 'metal-mode "Metal" "//"
             '("kernel" "vertex" "fragment" "device" "constant" "thread"
               "threadgroup" "texture2d" "sampler" "using" "namespace"
               "struct" "class" "if" "else" "for" "return" "float2" "float3"
               "float4" "half" "uint"))
(my/own-mode 'jai-mode "Jai" "//"
             '("if" "else" "for" "while" "return" "struct" "enum" "union"
               "proc" "using" "defer" "ifx" "remove" "insert" "push" "pop"
               "#import" "#load" "#if" "#else"))
(my/own-mode 'carbon-mode "Carbon" "//"
             '("package" "import" "fn" "class" "interface" "let" "var" "if"
               "else" "for" "while" "return" "choice" "namespace" "library"
               "impl" "as" "where" "match"))
(my/own-mode 'nu-mode "Nushell" "#"
             '("def" "let" "mut" "if" "else" "for" "while" "return" "module"
               "use" "export" "alias" "hide" "overlay" "match"))
(my/own-mode 'godot-scene-mode "Godot" ";"
             '("gd_scene" "ext_resource" "sub_resource" "connection"
               "load_steps" "format" "uid" "parent" "script"))
(my/own-mode 'dhall-mode "Dhall" "--"
             '("let" "in" "if" "then" "else" "forall" "Type" "Kind" "assert"
               "merge" "Some" "None" "toMap" "Optional"))
(my/own-mode 'hy-mode "Hy" ";"
             '("defn" "defmacro" "defmodule" "let" "if" "when" "cond" "for"
               "while" "import" "require" "setv" "fn" "lambda" "do"))
(my/own-mode 'janet-mode "Janet" "#"
             '("def" "defn" "defmacro" "var" "if" "when" "cond" "for" "while"
               "loop" "import" "fn" "do" "let" "upscope" "module"))
(unless (fboundp 'renpy-mode)
  (define-derived-mode renpy-mode python-mode "Ren'Py"
    "Ren'Py script. Python syntax, plus Ren'Py statements."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<"
                 (regexp-opt
                  '("label" "jump" "call" "menu" "scene" "show" "hide"
                    "play" "stop" "image" "define" "default" "screen"
                    "transform" "style" "init" "python" "with" "pause")
                  t)
                 "\\_>")
        . font-lock-keyword-face))
     'append)))
(defconst my/luastg-keywords
  '("lstg" "Include" "DoFile" "LoadPack" "UnloadPack" "ExtractRes"
    "LoadTexture" "LoadImage" "LoadImageFromFile" "LoadImageGroup"
    "LoadAnimation" "LoadMusic" "LoadSound" "LoadPS" "LoadFont" "LoadTTF"
    "CheckRes" "RemoveResource" "SetImageState" "SetAnimationState"
    "SetFontState" "RenderRect" "Render4V" "SetOrtho" "SetPerspective"
    "SetFog" "SetViewport" "SetScissorRect" "SetZBufferEnable"
    "BeginScene" "EndScene" "Snapshot" "CreateRenderTarget"
    "PushRenderTarget" "PopRenderTarget" "PostEffect"
    "New" "Del" "Kill" "IsValid" "BoxCheck" "ColliCheck" "Angle" "Dist"
    "SetV" "GetV" "SetImgState" "DefaultRenderFunc"
    "KeyIsDown" "KeyIsPressed" "GetLastKey" "GetLastChar" "GetKeyState"
    "Set3D" "ResetPool" "SetBound" "PlaySound" "PlayMusic" "StopMusic"
    "PauseMusic" "ResumeMusic" "GetMusicState" "AfterFrame"
    "SetSuperPause" "AddSuperPause" "ParticleFire" "ParticleStop"
    "stage" "task" "Print" "Color" "import")
  "LuaSTG engine words. Lua itself stays with lua-mode.")
(unless (fboundp 'luastg-mode)
  (define-derived-mode luastg-mode lua-mode "LuaSTG"
    "LuaSTG script. Lua syntax, plus the engine API."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<" (regexp-opt my/luastg-keywords t) "\\_>")
       (0 font-lock-keyword-face t)))
     'prepend)))
(defconst my/luastg-magic-re
  (rx (or (seq (or bos (not (any "A-Za-z0-9_"))) "lstg.")
          (seq (or bos (not (any "A-Za-z0-9_"))) "require" (* space)
               (? "(" (* space)) (or "\"" "'") "lstg" (or "\"" "'"))
          (seq (or bos (not (any "A-Za-z0-9_"))) "stage."
               (or "New" "QuitGame" "SetTimer" "Set") (* space) "(")
          (seq (or bos (not (any "A-Za-z0-9_"))) "task.New" (* space) "(")
          (seq line-start (* space)
               (or "Include" "DoFile" "LoadTexture" "LoadImage"
                   "LoadImageGroup" "LoadImageFromFile" "LoadAnimation"
                   "LoadMusic" "LoadSound" "LoadPS" "LoadFont" "LoadTTF"
                   "KeyIsDown" "KeyIsPressed")
               (* space) "(")))
  "Matches a LuaSTG engine call.")
(defun my/luastg-script-p ()
  "Non-nil when this .lua file calls the LuaSTG engine."
  (condition-case nil
      (and buffer-file-name
           (string-match-p "\\.lua\\'" buffer-file-name)
           (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (re-search-forward my/luastg-magic-re
                                  (min (+ (point-min) 12000) (point-max))
                                  t))))
    (error nil)))
(add-to-list 'magic-mode-alist '(my/luastg-script-p . luastg-mode))
(unless (fboundp 'matlab-mode)
  (if (fboundp 'octave-mode)
      (define-derived-mode matlab-mode octave-mode "MATLAB"
        "MATLAB source.")
    (my/own-mode 'matlab-mode "MATLAB" "%"
                 '("function" "end" "if" "else" "elseif" "for" "while"
                   "switch" "case" "otherwise" "return" "break" "continue"))))



(my/own-mode 'gml-mode "GML" "//"
             '("var" "if" "else" "for" "while" "repeat" "do" "until" "switch"
               "case" "break" "continue" "return" "function" "with" "exit"
               "globalvar" "enum" "constructor" "new" "true" "false"))
(my/own-mode 'llvm-mode "LLVM" ";"
             '("define" "declare" "target" "datalayout" "triple" "ret" "br"
               "add" "sub" "mul" "load" "store" "alloca" "call" "icmp" "fcmp"
               "phi" "private" "internal" "external" "constant" "global"))
(my/own-mode 'mlir-mode "MLIR" "//"
             '("func" "module" "return" "call" "arith" "memref" "scf"
               "builtin" "attributes"))
(my/own-mode 'tablegen-mode "TableGen" "//"
             '("def" "class" "defm" "multiclass" "let" "foreach" "if" "then"
               "else" "field" "bit" "bits" "int" "string" "list" "dag" "code"
               "include"))
(my/own-mode 'capnp-mode "Cap'n Proto" "#"
             '("struct" "enum" "interface" "union" "const" "using" "import"
               "annotation"))
(my/own-mode 'flatbuffers-mode "FlatBuffers" "//"
             '("table" "struct" "enum" "union" "namespace" "root_type"
               "include" "attribute" "file_identifier"))
(unless (fboundp 'starlark-mode)
  (define-derived-mode starlark-mode python-mode "Starlark"
    "Starlark and Bazel."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<"
                 (regexp-opt
                  '("load" "rule" "attr" "provider" "aspect" "depset"
                    "select" "glob" "exports_files" "package" "config_setting"
                    "alias" "filegroup" "native" "fail" "struct" "module")
                  t)
                 "\\_>")
        0 font-lock-keyword-face t))
     'prepend)))
(defvar my/sparql-keywords
  '("PREFIX" "BASE" "SELECT" "DISTINCT" "REDUCED" "CONSTRUCT" "DESCRIBE"
    "ASK" "WHERE" "OPTIONAL" "UNION" "MINUS" "FILTER" "BIND" "VALUES"
    "GRAPH" "SERVICE" "LIMIT" "OFFSET" "ORDER" "GROUP" "HAVING" "INSERT"
    "DELETE" "WITH" "FROM" "NAMED" "EXISTS" "BOUND" "DATATYPE"
    "LANGMATCHES" "REGEX" "COALESCE" "SILENT" "USING" "CREATE" "DROP"
    "CLEAR" "LOAD" "COPY" "MOVE" "DESC")
  "SPARQL keywords. None of them is a single letter.")
(unless (fboundp 'sparql-mode)
  (define-derived-mode sparql-mode prog-mode "SPARQL"
    "SPARQL source."
    (setq-local comment-start "#"
                comment-end ""
                indent-tabs-mode nil
                tab-width 4)
    (setq-local my/rich-filled t)
    (modify-syntax-entry ?\" ".")
    (modify-syntax-entry ?\' ".")
    (modify-syntax-entry ?\` ".")
    (setq-local font-lock-defaults
                (list (my/rich-keywords "#" my/sparql-keywords nil t) nil t))))
(unless (fboundp 'apex-mode)
  (define-derived-mode apex-mode java-mode "Apex"
    "Salesforce Apex."))

(defun my/mode-cl ()
  "Common Lisp when the file looks like Lisp, otherwise OpenCL."
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((string-match-p "\\`\\s-*\\(?:;\\|(\\)\\|\\_<\\(?:defun\\|in-package\\|defpackage\\)\\_>" chunk)
          (lisp-mode))
         (t (opencl-mode))))
    (error (opencl-mode))))

(defun my/mode-ron ()
  "RON is not Rust, so it keeps the classic highlighter."
  (when (fboundp 'rust-mode)
    (rust-mode)))

(defun my/mode-erb ()
  "Ruby ERB vs ERABasic for .erb"
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((string-match-p "<%[=#-]?" chunk)
          (if (fboundp 'web-mode) (web-mode) (html-mode)))
         ((string-match-p "^[@#]\\|^PRINTFORM\\|^#DIM\\|^DRAWLINE" chunk)
          (erabasic-mode))
         (t (if (fboundp 'web-mode) (web-mode) (html-mode)))))
    (error (if (fboundp 'web-mode) (web-mode) (html-mode)))))

(my/own-mode 'sass-mode "Sass" "//"
             '("mixin" "include" "extend" "function" "return" "else"
               "each" "while" "media" "import" "forward" "charset"
               "error" "warn" "debug" "at-root" "if" "for" "use"))

(my/own-mode 'http-mode "HTTP" "#"
             '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS"
               "TRACE" "CONNECT")
             t)
(my/own-mode 'sln-mode "Solution" "#"
             '("Project" "EndProject" "Global" "EndGlobal"
               "GlobalSection" "EndGlobalSection" "ProjectSection"
               "EndProjectSection" "preProject" "postProject"))

(dolist (cell
         '(;; Web / templates
           ("\\.jsx\\'" . web-mode) ("\\.tsx\\'" . web-mode)
           ("\\.vue\\'" . web-mode) ("\\.svelte\\'" . svelte-mode)
           ("\\.astro\\'" . web-mode) ("\\.njk\\'" . web-mode)
           ("\\.ejs\\'" . web-mode) ("\\.hbs\\'" . web-mode)
           ("\\.mustache\\'" . web-mode) ("\\.liquid\\'" . web-mode)
           ("\\.twig\\'" . web-mode) ("\\.j2\\'" . web-mode)
           ("\\.jinja2?\\'" . web-mode) ("\\.blade\\.php\\'" . web-mode)
           ("\\.cshtml\\'" . web-mode) ("\\.razor\\'" . web-mode)
           ("\\.jsp\\'" . web-mode) ("\\.aspx?\\'" . web-mode)
           ("\\.html?\\'" . web-mode) ("\\.mjml\\'" . web-mode)
           ("\\.http\\'" . http-mode) ("\\.fxml\\'" . nxml-mode)
           ("\\.pug\\'" . pug-mode) ("\\.jade\\'" . pug-mode)
           ("\\.haml\\'" . haml-mode) ("\\.slim\\'" . slim-mode)
           ("\\.mdx\\'" . markdown-mode)
           ;; Script
           ("\\.mjs\\'" . js-mode) ("\\.cjs\\'" . js-mode)
           ("\\.mts\\'" . typescript-mode) ("\\.cts\\'" . typescript-mode)
           ("\\.ts\\'" . typescript-mode)
           ("\\.coffee\\'" . coffee-mode) ("\\.cson\\'" . coffee-mode)
           ("\\.lua\\'" . lua-mode) ("\\.luau\\'" . lua-mode) ("\\.luastg\\'" . luastg-mode)
           ("\\.moon\\'" . moonscript-mode)
           ("\\.ps1\\'" . powershell-mode) ("\\.psm1\\'" . powershell-mode)
           ("\\.psd1\\'" . powershell-mode) ("\\.t\\'" . perl-mode)
           ("\\.fish\\'" . fish-mode) ("\\.nu\\'" . nu-mode)
           ;; Data
           ("\\.jsonc\\'" . json-mode) ("\\.json5\\'" . json-mode)
           ("\\.jsonnet\\'" . jsonnet-mode) ("\\.libsonnet\\'" . jsonnet-mode)
           ("\\.jsonl\\'" . json-mode) ("\\.geojson\\'" . json-mode)
           ("\\.ipynb\\'" . json-mode)
           ("\\.webmanifest\\'" . json-mode) ("\\.har\\'" . json-mode)
           ("\\.gltf\\'" . json-mode) ("\\.arb\\'" . json-mode)
           ("\\.toml\\'" . conf-toml-mode) ("Pipfile\\'" . conf-toml-mode)
           ("\\.ini\\'" . conf-mode) ("\\.cfg\\'" . conf-mode)
           ("\\.conf\\'" . conf-mode) ("\\.editorconfig\\'" . conf-mode)
           ("\\.env\\'" . dotenv-mode) ("\\.env\\..+\\'" . dotenv-mode)
           ("\\.properties\\'" . conf-javaprop-mode)
           ("\\.csv\\'" . csv-mode) ("\\.tsv\\'" . tsv-mode)
           ("\\.graphql\\'" . graphql-mode) ("\\.gql\\'" . graphql-mode)
           ("\\.proto\\'" . protobuf-mode) ("\\.thrift\\'" . thrift-mode)
           ("\\.avdl\\'" . avro-idl-mode) ("\\.capnp\\'" . capnp-mode)
           ("\\.fbs\\'" . flatbuffers-mode)
           ;; Game / shader / GPU
           ("\\.gd\\'" . gdscript-mode) ("\\.gdshader\\'" . glsl-mode)
           ("\\.rpy\\'" . renpy-mode) ("\\.rpym\\'" . renpy-mode)
           ("\\.glsl\\'" . glsl-mode) ("\\.vert\\'" . glsl-mode)
           ("\\.frag\\'" . glsl-mode) ("\\.geom\\'" . glsl-mode)
           ("\\.comp\\'" . glsl-mode) ("\\.tesc\\'" . glsl-mode)
           ("\\.tese\\'" . glsl-mode)
           ("\\.hlsl\\'" . hlsl-mode) ("\\.hlsli\\'" . hlsl-mode)
           ("\\.fx\\'" . hlsl-mode) ("\\.fxh\\'" . hlsl-mode)
           ("\\.usf\\'" . hlsl-mode) ("\\.ush\\'" . hlsl-mode)
           ("\\.wgsl\\'" . wgsl-mode) ("\\.metal\\'" . metal-mode)
           ("\\.cu\\'" . cuda-mode) ("\\.cuh\\'" . cuda-mode)
           ("\\.cl\\'" . my/mode-cl) ("\\.slang\\'" . slang-mode)
           ("\\.spv\\'" . fundamental-mode)
           ("\\.tscn\\'" . godot-scene-mode) ("\\.tres\\'" . godot-scene-mode)
           ("\\.import\\'" . godot-scene-mode) ("\\.godot\\'" . godot-scene-mode)
           ("\\.unity\\'" . yaml-mode) ("\\.prefab\\'" . yaml-mode)
           ("\\.meta\\'" . yaml-mode) ("\\.asset\\'" . yaml-mode)
           ("\\.controller\\'" . yaml-mode) ("\\.anim\\'" . yaml-mode)
           ("\\.vmat\\'" . conf-mode) ("\\.vmf\\'" . conf-mode)
           ("\\.qml\\'" . qml-mode) ("\\.qmltypes\\'" . qml-mode)
           ("\\.gml\\'" . gml-mode) ("\\.ink\\'" . ink-mode)
           ("\\.p8\\'" . lua-mode) ("\\.scad\\'" . scad-mode)
           ;; Systems / low-level
           ("\\.rs\\'" . rust-mode) ("\\.go\\'" . go-mode)
           ("go\\.mod\\'" . go-dot-mod-mode) ("go\\.sum\\'" . conf-mode)
           ("\\.zig\\'" . zig-mode) ("\\.zon\\'" . zig-mode)
           ("\\.nim\\'" . nim-mode) ("\\.nims\\'" . nim-mode)
           ("\\.v\\'" . verilog-mode) ("\\.sv\\'" . verilog-mode)
           ("\\.svh\\'" . verilog-mode) ("\\.vhdl?\\'" . vhdl-mode)
           ("\\.odin\\'" . odin-mode) ("\\.d\\'" . d-mode)
           ("\\.di\\'" . d-mode) ("\\.cr\\'" . crystal-mode)
           ("\\.jai\\'" . jai-mode)
           ("\\.ll\\'" . llvm-mode) ("\\.mlir\\'" . mlir-mode)
           ("\\.td\\'" . tablegen-mode) ("\\.s\\'" . asm-mode)
           ("\\.S\\'" . asm-mode) ("\\.asm\\'" . nasm-mode)
           ("\\.nasm\\'" . nasm-mode) ("\\.masm\\'" . masm-mode)
           ("\\.wat\\'" . wat-mode)
           ("\\.wast\\'" . wat-mode)
           ;; Managed
           ("\\.cs\\'" . csharp-mode) ("\\.csx\\'" . csharp-mode)
           ("\\.kt\\'" . kotlin-mode)
           ("\\.kts\\'" . kotlin-mode)
           ("\\.scala\\'" . scala-mode) ("\\.sbt\\'" . scala-mode)
           ("\\.groovy\\'" . groovy-mode) ("\\.gradle\\'" . groovy-mode)
           ("Jenkinsfile\\'" . groovy-mode)
           ("\\.java\\'" . java-mode) ("\\.dart\\'" . dart-mode)
           ("\\.swift\\'" . swift-mode) ("\\.m\\'" . objc-mode)
           ("\\.mm\\'" . objc-mode) ("\\.hx\\'" . haxe-mode)
           ("\\.php\\'" . php-mode) ("\\.phtml\\'" . php-mode)
           ;; FP — Haskell family
           ("\\.hs\\'" . haskell-mode) ("\\.hs-boot\\'" . haskell-mode)
           ("\\.hsc\\'" . haskell-mode) ("\\.hsig\\'" . haskell-mode)
           ("\\.chs\\'" . haskell-c2hs-mode)
           ("\\.lhs\\'" . haskell-literate-mode)
           ("\\.lhs-boot\\'" . haskell-literate-mode)
           ("\\.cabal\\'" . haskell-cabal-mode)
           ("cabal\\.project\\'" . haskell-cabal-mode)
           ("cabal\\.project\\.local\\'" . haskell-cabal-mode)
           ("\\.ghci\\'" . ghci-script-mode)
           ("\\.ipkg\\'" . idris-ipkg-mode)
           ("\\.purs\\'" . purescript-mode)
           ("\\.idr\\'" . idris-mode) ("\\.lidr\\'" . idris-mode)
           ("\\.agda\\'" . agda2-mode) ("\\.lagda\\'" . agda2-mode)
           ("\\.lean\\'" . lean4-mode)
           ("\\.fst\\'" . fstar-mode) ("\\.fsti\\'" . fstar-mode)
           ;; ML family
           ("\\.ml\\'" . tuareg-mode) ("\\.mli\\'" . tuareg-interface-mode)
           ("\\.mll\\'" . tuareg-mode) ("\\.mly\\'" . tuareg-menhir-mode)
           ("\\.eliom\\'" . tuareg-mode) ("\\.eliomi\\'" . tuareg-interface-mode)
           ("\\.re\\'" . reason-mode) ("\\.rei\\'" . reason-mode)
           ("\\.res\\'" . rescript-mode) ("\\.resi\\'" . rescript-mode)
           ("\\(?:\\`\\|/\\)dune\\'" . dune-mode)
           ("dune-project\\'" . dune-mode)
           ("dune-workspace\\'" . dune-mode)
           ("\\.sml\\'" . sml-mode) ("\\.sig\\'" . sml-mode)
           ("\\.fun\\'" . sml-mode) ("\\.cm\\'" . sml-cm-mode)
           ("\\.mlb\\'" . sml-mode) ("\\.grm\\'" . sml-yacc-mode)
           ("\\.fs\\'" . fsharp-mode) ("\\.fsi\\'" . fsharp-mode)
           ("\\.fsx\\'" . fsharp-mode) ("\\.fsscript\\'" . fsharp-mode)
           ;; Lisp family
           ("\\.el\\'" . emacs-lisp-mode)
           ("\\.lisp\\'" . lisp-mode) ("\\.lsp\\'" . lisp-mode)
           ("\\.asd\\'" . lisp-mode)
           ("\\.scm\\'" . scheme-mode) ("\\.ss\\'" . scheme-mode)
           ("\\.rkt\\'" . racket-mode) ("\\.rktd\\'" . racket-mode)
           ("\\.rktl\\'" . racket-mode) ("\\.scrbl\\'" . racket-mode)
           ("\\.sld\\'" . scheme-mode) ("\\.sls\\'" . scheme-mode)
           ("\\.sps\\'" . scheme-mode)
           ("\\.clj\\'" . clojure-mode) ("\\.cljs\\'" . clojurescript-mode)
           ("\\.cljc\\'" . clojurec-mode) ("\\.cljd\\'" . clojuredart-mode)
           ("\\.edn\\'" . edn-mode) ("\\.bb\\'" . clojure-mode)
           ("\\.fnl\\'" . fennel-mode) ("\\.hy\\'" . hy-mode)
           ("\\.lfe\\'" . lfe-mode) ("\\.janet\\'" . janet-mode)
           ;; BEAM / Elm / others
           ("\\.ex\\'" . elixir-mode) ("\\.exs\\'" . elixir-mode)
           ("\\.heex\\'" . web-mode) ("\\.leex\\'" . web-mode)
           ("\\.eex\\'" . web-mode)
           ("\\.erl\\'" . erlang-mode) ("\\.hrl\\'" . erlang-mode)
           ("\\.app\\.src\\'" . erlang-mode) ("rebar\\.config\\'" . erlang-mode)
           ("\\.elm\\'" . elm-mode) ("\\.gleam\\'" . gleam-mode)
           ("\\.dhall\\'" . dhall-mode) ("\\.nix\\'" . nix-mode)
           ("\\.pro\\'" . prolog-mode)
           ;; Scripting / scientific
           ("\\.pyi\\'" . python-mode) ("\\.pyw\\'" . python-mode)
           ("\\.pyx\\'" . cython-mode) ("\\.pxd\\'" . cython-mode)
           ("\\.pxi\\'" . cython-mode) ("\\.jl\\'" . julia-mode)
           ("\\.rmd\\'" . markdown-mode) ("\\.qmd\\'" . markdown-mode)
           ("\\.oct\\'" . octave-mode)
           ("\\.nb\\'" . wolfram-mode) ("\\.wl\\'" . wolfram-mode)
           ("\\.stan\\'" . stan-mode) ("\\.gp\\'" . gnuplot-mode)
           ("\\.plt\\'" . gnuplot-mode) ("\\.gnu\\'" . gnuplot-mode)
           ("\\.f\\'" . fortran-mode) ("\\.for\\'" . fortran-mode)
           ("\\.f90\\'" . f90-mode) ("\\.f95\\'" . f90-mode)
           ("\\.f03\\'" . f90-mode) ("\\.f08\\'" . f90-mode)
           ("\\.ada\\'" . ada-mode) ("\\.adb\\'" . ada-mode)
           ("\\.ads\\'" . ada-mode) ("\\.carbon\\'" . carbon-mode) ("\\.pas\\'" . pascal-mode)
           ("\\.pp\\'" . pascal-mode) ("\\.dpr\\'" . pascal-mode)
           ("\\.cob\\'" . cobol-mode) ("\\.cbl\\'" . cobol-mode)
           ("\\.forth\\'" . forth-mode) ("\\.fth\\'" . forth-mode)
           ;; Infra / devops
           ("Dockerfile\\'" . dockerfile-mode)
           ("Dockerfile\\..+\\'" . dockerfile-mode)
           ("Containerfile\\'" . dockerfile-mode)
           ("Containerfile\\..+\\'" . dockerfile-mode)
           ("Caddyfile\\'" . caddyfile-mode)
           ("requirements\\(?:-.*\\)?\\.txt\\'" . pip-requirements-mode)
           ("docker-compose.*\\.ya?ml\\'" . yaml-mode)
           ("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)
           ("meson\\.build\\'" . meson-mode) ("meson\\.options\\'" . meson-mode)
           ("\\.nix\\'" . nix-mode) ("flake\\.lock\\'" . json-mode)
           ("\\.tf\\'" . terraform-mode) ("\\.tfvars\\'" . terraform-mode)
           ("\\.hcl\\'" . hcl-mode) ("\\.nomad\\'" . hcl-mode)
           ("\\.cue\\'" . cue-mode) ("\\.dhall\\'" . dhall-mode)
           ("Justfile\\'" . just-mode) ("justfile\\'" . just-mode)
           ("\\.just\\'" . just-mode) ("BUILD\\'" . starlark-mode)
           ("BUILD\\.bazel\\'" . starlark-mode) ("WORKSPACE\\'" . starlark-mode)
           ("\\.bzl\\'" . starlark-mode) ("\\.star\\'" . starlark-mode)
           ("\\.bazel\\'" . starlark-mode) ("\\.sky\\'" . starlark-mode)
           ("\\.mak\\'" . makefile-gmake-mode)
           ("Tiltfile\\'" . starlark-mode)
           ("\\.nginx\\'" . nginx-mode) ("nginx\\.conf\\'" . nginx-mode)
           ("\\.htaccess\\'" . apache-mode) ("httpd\\.conf\\'" . apache-mode)
           ("\\.service\\'" . systemd-mode) ("\\.socket\\'" . systemd-mode)
           ("\\.timer\\'" . systemd-mode) ("\\.mount\\'" . systemd-mode)
           ("\\.pp\\'" . puppet-mode) ("\\.epp\\'" . puppet-mode)
           ("\\.prisma\\'" . prisma-mode) ("\\.sol\\'" . solidity-mode)
           ("Vagrantfile\\'" . ruby-mode) ("Gemfile\\'" . ruby-mode)
           ("Rakefile\\'" . ruby-mode) ("Podfile\\'" . ruby-mode)
           ("Brewfile\\'" . ruby-mode) ("Guardfile\\'" . ruby-mode)
           ("Procfile\\'" . conf-mode) ("\\.jq\\'" . jq-mode)
           ;; Markup / docs
           ("\\.rst\\'" . rst-mode) ("\\.rest\\'" . rst-mode)
           ("\\.adoc\\'" . adoc-mode) ("\\.asciidoc\\'" . adoc-mode)
           ("\\.texi\\'" . texinfo-mode) ("\\.po\\'" . po-mode)
           ("\\.pot\\'" . po-mode) ("\\.dot\\'" . graphviz-dot-mode)
           ("\\.gv\\'" . graphviz-dot-mode) ("\\.mmd\\'" . mermaid-mode)
           ("\\.mermaid\\'" . mermaid-mode) ("\\.puml\\'" . plantuml-mode)
           ("\\.plantuml\\'" . plantuml-mode) ("\\.ly\\'" . lilypond-mode)
           ("\\.tex\\'" . latex-mode) ("\\.sty\\'" . latex-mode)
           ("\\.cls\\'" . latex-mode) ("\\.bib\\'" . bibtex-mode)
           ("\\.org\\'" . org-mode) ("\\.md\\'" . markdown-mode)
           ;; XML family
           ("\\.xaml\\'" . nxml-mode) ("\\.resx\\'" . nxml-mode)
           ("\\.csproj\\'" . nxml-mode) ("\\.fsproj\\'" . nxml-mode)
           ("\\.vbproj\\'" . nxml-mode) ("\\.vcxproj\\'" . nxml-mode)
           ("\\.sln\\'" . sln-mode)
           ("\\.props\\'" . nxml-mode) ("\\.targets\\'" . nxml-mode)
           ("\\.xslt?\\'" . nxml-mode) ("\\.xsd\\'" . nxml-mode)
           ("\\.wsdl\\'" . nxml-mode) ("\\.plist\\'" . nxml-mode)
           ("\\.svg\\'" . nxml-mode) ("\\.ui\\'" . nxml-mode)
           ("\\.glade\\'" . nxml-mode) ("\\.storyboard\\'" . nxml-mode)
           ("\\.xib\\'" . nxml-mode) ("\\.dae\\'" . nxml-mode)
           ("\\.launch\\'" . nxml-mode) ("\\.urdf\\'" . nxml-mode)
           ("\\.xml\\'" . nxml-mode)
           ;; Git / editor
           ("\\.gitignore\\'" . gitignore-mode)
           ("\\.dockerignore\\'" . gitignore-mode)
           ("\\.prettierignore\\'" . gitignore-mode)
           ("\\.eslintignore\\'" . gitignore-mode)
           ("\\.npmignore\\'" . gitignore-mode)
           ("\\.cursorignore\\'" . gitignore-mode)
           ("\\.npmrc\\'" . conf-mode) ("\\.yarnrc\\'" . conf-mode)
           ("\\.prettierrc\\'" . json-mode) ("\\.eslintrc\\'" . json-mode)
           ("\\.babelrc\\'" . json-mode) ("\\.stylelintrc\\'" . json-mode)
           ("\\.code-workspace\\'" . json-mode)
           ("\\.gitattributes\\'" . gitattributes-mode)
           ("\\.gitconfig\\'" . gitconfig-mode)
           ("\\.gitmodules\\'" . gitconfig-mode)
           ("\\.vim\\'" . vimrc-mode) ("\\.vimrc\\'" . vimrc-mode)
           ("\\.nvimrc\\'" . vimrc-mode)
           ;; Styles
           ("\\.scss\\'" . scss-mode) ("\\.sass\\'" . sass-mode)
           ("\\.less\\'" . less-css-mode) ("\\.styl\\'" . stylus-mode)
           ("\\.pcss\\'" . css-mode) ("\\.postcss\\'" . css-mode)
           ("\\.css\\'" . css-mode)
           ;; Other
           ("\\.vala\\'" . vala-mode) ("\\.vapi\\'" . vala-mode)
           ("\\.sql\\'" . sql-mode) ("\\.pgsql\\'" . sql-mode)
           ("\\.mysql\\'" . sql-mode)
           ("\\.kdl\\'" . kdl-lite-mode) ("\\.ron\\'" . my/mode-ron)
           ;; Extra languages. Highlight-only: skip ESS / Chapel / KDL packages
           ;; that spawn ctags, tree-sitter downloads, or an R process.
           ("\\.[rR]\\'" . my-r-mode) ("\\.Rprofile\\'" . my-r-mode)
           ("\\.awk\\'" . awk-mode) ("\\.gawk\\'" . awk-mode)
           ("\\.sed\\'" . sed-mode) ("\\.ahk\\'" . ahk-mode)
           ("\\.ah2\\'" . ahk-mode) ("\\.au3\\'" . autoit-mode)
           ("\\.raku\\'" . raku-mode) ("\\.rakumod\\'" . raku-mode)
           ("\\.rakudoc\\'" . raku-mode) ("\\.rakutest\\'" . raku-mode)
           ("\\.p6\\'" . raku-mode) ("\\.pl6\\'" . raku-mode)
           ("\\.pm6\\'" . raku-mode) ("\\.rego\\'" . rego-mode)
           ("\\.feature\\'" . feature-mode) ("\\.pkl\\'" . pkl-mode)
           ("\\.prql\\'" . prql-mode) ("\\.promql\\'" . promql-mode)
           ("\\.bicep\\'" . bicep-mode) ("\\.bicepparam\\'" . bicep-mode)
           ("\\.bal\\'" . ballerina-mode) ("\\.chpl\\'" . chapel-lite-mode)
           ("\\.ha\\'" . hare-mode) ("\\.HC\\'" . holyc-mode)
           ("\\.hc\\'" . holyc-mode) ("\\.vb\\'" . basic-mode)
           ("\\.bas\\'" . basic-generic-mode) ("\\.vbs\\'" . visual-basic-mode)
           ("\\.apex\\'" . apex-mode) ("\\.trigger\\'" . apex-mode)
           ("\\.ceylon\\'" . ceylon-mode) ("\\.m2\\'" . modula-2-mode)
           ("\\.def\\'" . modula-2-mode)
           ("\\.shader\\'" . shaderlab-mode) ("\\.vex\\'" . vex-mode)
           ("\\.vfl\\'" . vex-mode) ("\\.rsl\\'" . rsl-mode)
           ("\\.agal\\'" . agal-mode)
           ("\\.ps\\'" . ps-mode) ("\\.eps\\'" . ps-mode)
           ("\\.pov\\'" . pov-mode) ("\\.gmi\\'" . gemini-mode)
           ("\\.gemini\\'" . gemini-mode) ("\\.apib\\'" . markdown-mode)
           ("\\.bs\\'" . markdown-mode) ("\\.roff\\'" . nroff-mode)
           ("\\.man\\'" . nroff-mode) ("\\.groff\\'" . nroff-mode)
           ("\\.erh\\'" . erabasic-mode) ("\\.coq\\'" . coq-mode)
           ("\\.matlab\\'" . matlab-mode)))
  (my/add-ft (car cell) (cdr cell)))

;; haskell-mode autoloads "\\.[gh]s\\'" which also steals .gs (Gosu / Apps Script).
(my/drop-ft "\\.[gh]s\\'")
(my/drop-ft "\\.l[gh]s\\'")
(my/drop-ft "DESCRIPTION\\'")
(my/drop-ft "NAMESPACE\\'")
(my/drop-ft "CITATION\\'")
(my/drop-ft "\\.[Bb][Uu][Gg]\\'")
(my/drop-ft "\\.[Bb][Oo][Gg]\\'")
(my/drop-ft "\\.[Bb][Mm][Dd]\\'")
(my/drop-ft "/R/.*\\.q\\'")

(add-to-list 'auto-mode-alist '("\\.m\\'" . my/mode-m))
(add-to-list 'auto-mode-alist '("\\.v\\'" . my/mode-v))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . my/mode-erb))
(add-to-list 'auto-mode-alist '("\\.erh\\'" . erabasic-mode))
(when (fboundp 'tcl-mode)
  (my/add-ft "\\.tk\\'" 'tcl-mode)
  (my/add-ft "\\.tm\\'" 'tcl-mode))
(my/add-ft "\\.rq\\'" 'sparql-mode)
(my/add-ft "\\.sparql\\'" 'sparql-mode)

;;; Same-language fallbacks. A missing mode never borrows another language.
(unless (fboundp 'haskell-literate-mode)
  (dolist (p '("\\.lhs\\'" "\\.lhs-boot\\'"))
    (add-to-list 'auto-mode-alist (cons p 'haskell-mode))))
(unless (fboundp 'haskell-c2hs-mode)
  (add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-mode)))
(unless (fboundp 'tuareg-interface-mode)
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode)))
(unless (fboundp 'feature-mode)
  (add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode)))
(unless (fboundp 'visual-basic-mode)
  (add-to-list 'auto-mode-alist '("\\.vbs\\'" . basic-mode)))
(unless (fboundp 'gemini-mode)
  (add-to-list 'auto-mode-alist '("\\.gmi\\'" . gemtext-mode)))
(unless (fboundp 'clojurescript-mode)
  (my/add-ft "\\.cljs\\'" 'clojure-mode))
(unless (fboundp 'clojurec-mode)
  (my/add-ft "\\.cljc\\'" 'clojure-mode))
(unless (fboundp 'clojuredart-mode)
  (my/add-ft "\\.cljd\\'" 'clojure-mode))
(unless (fboundp 'edn-mode)
  (my/add-ft "\\.edn\\'" 'clojure-mode))
(unless (fboundp 'tsv-mode)
  (my/add-ft "\\.tsv\\'" 'csv-mode))
(unless (fboundp 'go-dot-mod-mode)
  (my/add-ft "go\\.mod\\'" 'go-mode))
(unless (fboundp 'basic-generic-mode)
  (my/add-ft "\\.bas\\'" 'basic-mode))

(dolist (cell '(("luajit" . lua-mode) ("lua" . lua-mode)
                ("python3" . python-mode) ("python" . python-mode)
                ("pypy" . python-mode) ("node" . js-mode)
                ("nodejs" . js-mode) ("ts-node" . typescript-mode)
                ("bash" . sh-mode) ("zsh" . sh-mode) ("fish" . fish-mode)
                ("ruby" . ruby-mode) ("perl" . perl-mode)
                ("php" . php-mode) ("gjs" . js-mode)
                ("racket" . racket-mode) ("guile" . scheme-mode)
                ("chicken" . scheme-mode) ("csi" . scheme-mode)
                ("sbcl" . lisp-mode) ("clisp" . lisp-mode)
                ("ccl" . lisp-mode) ("ecl" . lisp-mode)
                ("runghc" . haskell-mode) ("runhaskell" . haskell-mode)
                ("ghc" . haskell-mode) ("ghci" . haskell-mode)
                ("elixir" . elixir-mode) ("iex" . elixir-mode)
                ("erl" . erlang-mode) ("escript" . erlang-mode)
                ("ocaml" . tuareg-mode) ("ocamlrun" . tuareg-mode)
                ("clojure" . clojure-mode) ("lein" . clojure-mode)
                ("bb" . clojure-mode) ("idris" . idris-mode)
                ("idris2" . idris-mode) ("pwsh" . powershell-mode)
                ("awk" . awk-mode) ("gawk" . awk-mode) ("nawk" . awk-mode)
                ("Rscript" . my-r-mode) ("R" . my-r-mode)
                ("octave" . octave-mode) ("raku" . raku-mode)
                ("perl6" . raku-mode)))
  (when (fboundp (cdr cell))
    (add-to-list 'interpreter-mode-alist cell)))

(defconst my/renpy-keywords
  '("label" "jump" "call" "return" "menu" "scene" "show" "hide" "with"
    "play" "stop" "queue" "voice" "nvl" "window" "pause" "image" "define"
    "default" "transform" "screen" "style" "init" "python" "early"
    "layeredimage" "camera" "atlas" "extend" "audio" "music" "sound"
    "movie" "renpy" "as" "at" "behind" "onlayer" "zorder" "expression"
    "use" "has" "add" "textbutton" "imagebutton" "text" "frame" "vbox"
    "hbox" "grid" "fixed" "null" "key" "timer" "action" "hovered"
    "selected" "auto" "style_prefix" "translate" "old" "new"
    "Character" "DynamicCharacter" "nvl_narrator" "pass" "in" "from"
    "if" "elif" "else" "while" "for" "True" "False" "None"))

(defconst my/hlsl-keywords
  '("float2" "float3" "float4" "float2x2" "float3x3" "float4x4"
    "int2" "int3" "int4" "uint" "uint2" "uint3" "uint4" "half" "half2"
    "half3" "half4" "min16float" "min10float" "dword" "snorm" "unorm"
    "SamplerState" "SamplerComparisonState" "Texture1D" "Texture2D"
    "Texture3D" "TextureCube" "Texture2DArray" "RWTexture2D"
    "StructuredBuffer" "RWStructuredBuffer" "ByteAddressBuffer"
    "cbuffer" "tbuffer" "register" "packoffset" "discard" "clip"
    "SV_Position" "SV_TARGET" "SV_Target" "SV_Depth" "SV_VertexID"
    "SV_InstanceID" "SV_PrimitiveID" "SV_IsFrontFace" "SV_Coverage"
    "TEXCOORD" "NORMAL" "TANGENT" "BINORMAL" "COLOR" "BLENDWEIGHT"
    "BLENDINDICES" "POSITION" "PSIZE" "FOG"
    "lerp" "saturate" "mul" "normalize" "dot" "cross" "reflect"
    "refract" "length" "distance" "step" "smoothstep" "clamp" "abs"
    "frac" "floor" "ceil" "round" "rsqrt" "rcp" "ddx" "ddy" "tex2D"
    "tex2Dlod" "texCUBE" "Sample" "SampleLevel" "Load" "Gather"
    "AllMemoryBarrier" "GroupMemoryBarrier" "numthreads"
    "UnityObjectToClipPos" "UNITY_MATRIX_MVP" "UNITY_MATRIX_MV"
    "TRANSFORM_TEX" "UnpackNormal"))

(defconst my/glsl-keywords
  '("attribute" "varying" "in" "out" "inout" "uniform" "layout"
    "precision" "highp" "mediump" "lowp" "invariant" "smooth" "flat"
    "centroid" "sample" "patch" "subroutine" "shared" "coherent"
    "volatile" "restrict" "readonly" "writeonly" "noperspective"
    "vec2" "vec3" "vec4" "ivec2" "ivec3" "ivec4" "uvec2" "uvec3"
    "uvec4" "bvec2" "bvec3" "bvec4" "mat2" "mat3" "mat4" "mat2x3"
    "mat3x4" "sampler2D" "sampler3D" "samplerCube" "sampler2DShadow"
    "image2D" "uimage2D" "gl_Position" "gl_FragColor" "gl_FragCoord"
    "gl_VertexID" "gl_InstanceID" "gl_FrontFacing" "gl_PointSize"
    "texture" "texture2D" "textureCube" "normalize" "dot" "cross"
    "reflect" "refract" "mix" "clamp" "smoothstep" "step" "fract"
    "mod" "abs" "sign" "floor" "ceil" "length" "distance" "faceforward"
    "dFdx" "dFdy" "fwidth" "discard" "emitVertex" "endPrimitive"))

(defconst my/wasm-keywords
  '("module" "func" "param" "result" "local" "global" "memory" "table"
    "export" "import" "type" "call" "call_indirect" "return" "block"
    "loop" "if" "else" "end" "then" "br" "br_if" "br_table" "drop"
    "select" "i32" "i64" "f32" "f64" "v128" "mut" "offset" "align"
    "i32.const" "i32.add" "i32.sub" "i32.mul" "i32.load" "i32.store"
    "local.get" "local.set" "local.tee" "global.get" "global.set"
    "memory.size" "memory.grow" "unreachable" "nop" "data" "elem"
    "start"))

(defun my/add-kw (words)
  (font-lock-add-keywords
   nil `((,(concat "\\_<" (regexp-opt words t) "\\_>")
          . font-lock-keyword-face))
   'append))

(defvar-local my/lang-extras-done nil)

(defun my/lang-extras ()
  (ignore-errors
    (when (and buffer-file-name
               (not (eq my/lang-extras-done major-mode))
               (not (bound-and-true-p so-long-minor-mode))
               (< (buffer-size) 400000))
      (setq my/lang-extras-done major-mode)
      (let ((ext (downcase (or (file-name-extension buffer-file-name) ""))))
        (cond
         ((member ext '("rpy" "rpym")) (my/add-kw my/renpy-keywords))
         ((member ext '("hlsl" "hlsli" "fx" "fxh" "usf" "ush"))
          (my/add-kw my/hlsl-keywords))
         ((member ext '("glsl" "vert" "frag" "geom" "comp" "tesc" "tese"
                        "gdshader"))
          (my/add-kw my/glsl-keywords))
         ((member ext '("shader"))
          (my/add-kw my/glsl-keywords)
          (my/add-kw my/hlsl-keywords))
         ((member ext '("wat" "wast")) (my/add-kw my/wasm-keywords)))))))
(add-hook 'after-change-major-mode-hook #'my/lang-extras)

(with-eval-after-load 'gdscript-mode
  (setq gdscript-use-tab-indents nil
        gdscript-indent-offset 4
        gdscript-godot-executable "godot")
  (when (boundp 'gdscript-mode-map)
    (define-key gdscript-mode-map (kbd "<f5>") nil)
    (define-key gdscript-mode-map (kbd "<f6>") nil))
  (add-hook 'gdscript-mode-hook
            (lambda ()
              (remove-hook 'post-self-insert-hook
                           #'gdscript-electric-pair-string-delimiter t)
              (remove-hook 'post-self-insert-hook
                           #'gdscript-indent-post-self-insert-function t))))
(with-eval-after-load 'lua-mode (setq lua-indent-level 4))
(with-eval-after-load 'csharp-mode (setq csharp-mode-indent-offset 4))
(with-eval-after-load 'nasm-mode (setq nasm-basic-offset 4))
(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-opening nil
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight nil
        web-mode-enable-current-column-highlight nil
        web-mode-enable-engine-detection nil
        web-mode-content-types-alist
        '(("jsx" . "\\.jsx\\'") ("tsx" . "\\.tsx\\'")
          ("vue" . "\\.vue\\'") ("svelte" . "\\.svelte\\'"))))

;; Package modes stay highlight-and-indent. Settings apply after the
;; package loads, so they win over that package's own defaults.
;; Never run these even if the user later :set ft= to the package mode.
(dolist (fn '(kdl-install-tree-sitter-grammar
              chapel-load-tags chapel-build-tags chapel-format-buffer
              matlab-ts-grammar-install))
  (advice-add fn :override #'ignore))

(with-eval-after-load 'haskell-mode
  (setq haskell-mode-hook '(haskell-indentation-mode)
        haskell-tags-on-save nil
        haskell-stylish-on-save nil
        haskell-process-suggest-remove-import-lines nil
        haskell-process-auto-import-loaded-modules nil
        haskell-process-suggest-hoogle-imports nil
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-where-pre-offset 4
        haskell-indentation-where-post-offset 4))
(with-eval-after-load 'idris-mode
  (setq idris-mode-hook '(turn-on-idris-simple-indent)))
(with-eval-after-load 'fstar-mode
  (setq fstar-enabled-modules '(font-lock indentation comments literate)))
(with-eval-after-load 'purescript-mode
  (when (fboundp 'purescript-indentation-mode)
    (add-hook 'purescript-mode-hook #'purescript-indentation-mode)))
(with-eval-after-load 'dhall-mode
  (setq dhall-use-header-line nil
        dhall-format-at-save nil))
(with-eval-after-load 'elm-mode
  (setq elm-format-on-save nil
        elm-tags-on-save nil
        elm-sort-imports-on-save nil))
(with-eval-after-load 'tuareg
  (setq tuareg-highlight-all-operators t))
(with-eval-after-load 'ess
  (setq ess-ask-for-ess-directory nil
        ess-use-flymake nil
        ess-use-eldoc nil
        ess-use-company nil
        ess-use-auto-complete nil
        ess-r-package-auto-activate nil
        ess-use-ido nil
        ess-use-toolbar nil))
(with-eval-after-load 'ess-r-mode
  (setq ess-use-flymake nil
        ess-use-eldoc nil))
(with-eval-after-load 'matlab
  (setq matlab-verify-on-save-flag nil
        matlab-shell-ask-MATLAB-return nil
        matlab-show-mlint-warnings nil
        matlab-functions-have-end t))
(with-eval-after-load 'zig-mode
  (setq zig-format-on-save nil))
(with-eval-after-load 'chapel-mode
  (setq chapel-format-on-save nil
        chapel-use-ctags nil))


(defun my/treesit-p (lang)
  "Return non-nil when the compiled grammar LANG loads."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)))

(defun my/call-ts (classic ts lang)
  "Activate TS when LANG is installed, otherwise CLASSIC."
  (cond
   ((and (fboundp ts) (my/treesit-p lang)) (funcall ts))
   ((fboundp classic) (funcall classic))))

(defconst my/treesit-sources
  '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
    (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.4")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
    (css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
    (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.5.0")
    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "v0.23.1")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.3")
    (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.1.0")
    (heex "https://github.com/phoenixframework/tree-sitter-heex" "v0.7.0")
    (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
    (java "https://github.com/tree-sitter/tree-sitter-java" "v0.23.5")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.23.0")
    (json "https://github.com/tree-sitter/tree-sitter-json" "v0.23.0")
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua" "v0.2.0")
    (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.11" "php/src")
    (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc" "v0.1.4")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby" "v0.23.1")
    (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2")
    (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml" "v0.6.0")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
    (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "v0.7.0"))
  "Pinned grammar recipes. Opening a file never downloads one.")

(defconst my/treesit-remap
  '((python-mode python-ts-mode python)
    (js-mode js-ts-mode javascript)
    (javascript-mode js-ts-mode javascript)
    (js-jsx-mode js-ts-mode javascript)
    (js-json-mode json-ts-mode json)
    (typescript-mode typescript-ts-mode typescript)
    (c-mode c-ts-mode c)
    (c++-mode c++-ts-mode cpp)
    (java-mode java-ts-mode java)
    (rust-mode rust-ts-mode rust)
    (go-mode go-ts-mode go)
    (go-dot-mod-mode go-mod-ts-mode gomod)
    (ruby-mode ruby-ts-mode ruby)
    (css-mode css-ts-mode css)
    (json-mode json-ts-mode json)
    (yaml-mode yaml-ts-mode yaml)
    (lua-mode lua-ts-mode lua)
    (sh-mode bash-ts-mode bash)
    (shell-script-mode bash-ts-mode bash)
    (csharp-mode csharp-ts-mode c-sharp)
    (elixir-mode elixir-ts-mode elixir)
    (cmake-mode cmake-ts-mode cmake)
    (dockerfile-mode dockerfile-ts-mode dockerfile)
    (conf-toml-mode toml-ts-mode toml)
    (toml-mode toml-ts-mode toml))
  "Classic mode, tree-sitter mode, grammar. Applied only when the grammar loads.")

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (require 'treesit)
  (dolist (src my/treesit-sources)
    (unless (assoc (car src) treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist src)))
  (dolist (row my/treesit-remap)
    (let ((from (nth 0 row))
          (to (nth 1 row))
          (lang (nth 2 row)))
      (when (and (fboundp to) (my/treesit-p lang))
        (add-to-list 'major-mode-remap-alist (cons from to)))))
  (when (and (fboundp 'c-or-c++-ts-mode)
             (my/treesit-p 'c)
             (my/treesit-p 'cpp))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode)))
  (when (and (fboundp 'php-ts-mode)
             (my/treesit-p 'php)
             (my/treesit-p 'phpdoc)
             (my/treesit-p 'html)
             (my/treesit-p 'javascript)
             (my/treesit-p 'jsdoc)
             (my/treesit-p 'css))
    (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode)))
  (when (my/treesit-p 'html)
    (my/add-ft "\\.html?\\'" 'html-ts-mode))
  (when (my/treesit-p 'tsx)
    (my/add-ft "\\.tsx\\'" 'tsx-ts-mode))
  (when (my/treesit-p 'javascript)
    (my/add-ft "\\.jsx\\'" 'js-ts-mode))
  (when (my/treesit-p 'heex)
    (my/add-ft "\\.heex\\'" 'heex-ts-mode)))

(defun my/cmake-ts-safe ()
  "Drop a CMake query this grammar cannot compile.
Level 4 asks for anonymous punctuation nodes the grammar does not have,
and fontifying the buffer then signals."
  (setq treesit-font-lock-feature-list
        (mapcar (lambda (features)
                  (remq 'misc-punctuation features))
                treesit-font-lock-feature-list))
  (treesit-font-lock-recompute-features))
(add-hook 'cmake-ts-mode-hook #'my/cmake-ts-safe)

;;; Extra languages. Each mode is its own. A missing package never borrows another language.
(defvar my/extra-ft-cells nil)
(defvar my/extra-help-rows nil)
(dolist (row
         '(
           (pony-mode "Pony" "//" "pony" ("\\.pony\\'") ("actor" "be" "fun" "new" "let" "var" "if" "else" "then" "for" "while" "try" "end" "class" "type" "trait" "iso" "ref" "recover" "match" "object" "embed" "use" "return" "true" "false"))
           (roc-mode "Roc" "#" "roc" ("\\.roc\\'") ("app" "packages" "imports" "provides" "module" "exposes" "import" "if" "then" "else" "when" "is" "expect" "dbg" "crash" "return"))
           (mojo-mode "Mojo" "#" "mojo" ("\\.mojo\\'") ("fn" "def" "struct" "var" "let" "if" "else" "elif" "for" "while" "return" "raise" "from" "import" "alias" "trait" "owned" "inout" "raises"))
           (c3-mode "C3" "//" "c3" ("\\.c3\\'") ("module" "import" "fn" "struct" "enum" "union" "fault" "def" "macro" "if" "else" "for" "while" "return" "switch" "case" "defer" "try" "catch"))
           (vale-mode "Vale" "//" "vale" ("\\.vale\\'") ("struct" "interface" "fn" "func" "if" "else" "while" "foreach" "ret" "mut" "imm" "inl" "abstract" "impl" "export"))
           (inko-mode "Inko" "#" "inko" ("\\.inko\\'") ("class" "trait" "object" "def" "fn" "let" "mut" "if" "else" "while" "for" "return" "import" "module" "self" "throw" "try"))
           (lobster-mode "Lobster" "//" "lobster" ("\\.lobster\\'") ("def" "class" "struct" "if" "else" "while" "for" "return" "let" "var" "nil" "fn" "program" "import" "switch" "case"))
           (nelua-mode "Nelua" "--" "nelua" ("\\.nelua\\'") ("local" "function" "if" "then" "else" "elseif" "end" "for" "while" "do" "return" "switch" "case" "require" "global" "defer"))
           (vlang-mode "V" "//" "vlang" ("\\.vsh\\'") ("module" "import" "fn" "struct" "enum" "interface" "if" "else" "for" "match" "return" "mut" "pub" "const" "type" "defer" "go" "select" "or"))
           (gosu-mode "Gosu" "//" "gosu" ("\\.gs\\'") ("package" "class" "interface" "function" "construct" "property" "if" "else" "for" "while" "return" "uses" "var" "new" "override" "as" "typeis"))
           (boo-mode "Boo" "#" "boo" ("\\.boo\\'") ("class" "def" "if" "else" "elif" "for" "while" "return" "import" "namespace" "struct" "interface" "of" "in" "as" "public" "private"))
           (fantom-mode "Fantom" "//" "fantom" ("\\.fan\\'") ("class" "mixin" "enum" "void" "if" "else" "for" "while" "return" "using" "const" "static" "new" "override" "virtual"))
           (hack-mode "Hack" "//" "hack" ("\\.hack\\'") ("function" "class" "interface" "trait" "namespace" "use" "if" "else" "foreach" "for" "while" "return" "async" "await" "newtype" "shape" "type"))
           (processing-mode "Processing" "//" "processing" ("\\.pde\\'") ("void" "setup" "draw" "int" "float" "if" "else" "for" "while" "return" "class" "new" "size" "background"))
           (arduino-mode "Arduino" "//" "arduino" ("\\.ino\\'") ("void" "setup" "loop" "int" "float" "bool" "if" "else" "for" "while" "return" "digitalWrite" "digitalRead" "pinMode" "Serial"))
           (actionscript-mode "ActionScript" "//" "actionscript" ("\\.as\\'") ("package" "class" "function" "var" "const" "if" "else" "for" "while" "return" "import" "extends" "implements" "public" "private" "override"))
           (angelscript-mode "AngelScript" "//" "angelscript" ("\\.asc\\'") ("class" "interface" "enum" "void" "if" "else" "for" "while" "return" "funcdef" "mixin" "namespace" "import" "shared" "private"))
           (smali-mode "Smali" "#" "smali" ("\\.smali\\'") ("class" "super" "implements" "source" "field" "method" "end" "registers" "locals" "invoke" "return" "goto" "if" "const" "move"))
           (cil-mode "CIL" "//" "cil" ("\\.il\\'") ("assembly" "class" "method" "field" "public" "private" "static" "void" "ldarg" "ldloc" "stloc" "call" "ret" "newobj" "box"))
           (wit-mode "WIT" "//" "wit" ("\\.wit\\'") ("package" "interface" "world" "use" "type" "record" "variant" "enum" "flags" "resource" "func" "static" "constructor" "result" "option"))
           (teal-mode "Teal" "--" "teal" ("\\.tl\\'") ("local" "function" "if" "then" "else" "elseif" "end" "for" "while" "do" "return" "record" "enum" "global" "require"))
           (wren-mode "Wren" "//" "wren" ("\\.wren\\'") ("class" "is" "foreign" "construct" "var" "if" "else" "for" "while" "return" "import" "static" "break" "continue"))
           (squirrel-mode "Squirrel" "//" "squirrel" ("\\.nut\\'") ("function" "class" "local" "if" "else" "for" "while" "foreach" "return" "switch" "case" "extends" "constructor" "yield" "resume"))
           (pawn-mode "Pawn" "//" "pawn" ("\\.pwn\\'") ("new" "if" "else" "for" "while" "return" "switch" "case" "public" "stock" "static" "forward" "native" "enum" "const"))
           (sourcepawn-mode "SourcePawn" "//" "sourcepawn" ("\\.sp\\'") ("public" "stock" "static" "native" "forward" "if" "else" "for" "while" "return" "switch" "case" "enum" "methodmap" "typeset"))
           (quakec-mode "QuakeC" "//" "quakec" ("\\.qc\\'") ("void" "float" "vector" "entity" "string" "if" "else" "while" "do" "return" "local"))
           (unrealscript-mode "UnrealScript" "//" "unrealscript" ("\\.uc\\'") ("class" "extends" "function" "event" "state" "if" "else" "for" "while" "return" "var" "local" "defaultproperties" "simulated"))
           (verse-mode "Verse" "#" "verse" ("\\.verse\\'") ("if" "else" "for" "loop" "return" "class" "module" "var" "set" "block" "race" "sync" "branch" "spawn" "defer"))
           (papyrus-mode "Papyrus" "//" "papyrus" ("\\.psc\\'") ("Scriptname" "extends" "Function" "EndFunction" "Event" "EndEvent" "If" "Else" "ElseIf" "EndIf" "While" "EndWhile" "Return" "Property" "Auto"))
           (nwscript-mode "NWScript" "//" "nwscript" ("\\.nss\\'") ("void" "int" "float" "string" "object" "if" "else" "for" "while" "return" "struct" "const"))
           (lsl-mode "LSL" "//" "lsl" ("\\.lsl\\'") ("default" "state" "if" "else" "while" "for" "return" "jump" "integer" "float" "string" "key" "vector" "rotation" "list"))
           (mel-mode "MEL" "//" "mel" ("\\.mel\\'") ("proc" "global" "if" "else" "for" "while" "return" "string" "int" "float" "vector"))
           (osl-mode "OSL" "//" "osl" ("\\.osl\\'") ("shader" "surface" "displacement" "volume" "if" "else" "for" "while" "return" "color" "point" "vector" "normal" "float" "int" "string" "output"))
           (futhark-mode "Futhark" "--" "futhark" ("\\.fut\\'") ("def" "let" "if" "then" "else" "loop" "for" "in" "do" "return" "type" "module" "open" "import" "entry" "local"))
           (bend-mode "Bend" "#" "bend" ("\\.bend\\'") ("def" "if" "else" "switch" "case" "return" "object" "fold" "bend" "open" "lambda" "when"))
           (dafny-mode "Dafny" "//" "dafny" ("\\.dfy\\'") ("method" "function" "lemma" "predicate" "class" "datatype" "if" "else" "while" "return" "var" "requires" "ensures" "invariant" "decreases" "module" "import"))
           (boogie-mode "Boogie" "//" "boogie" ("\\.bpl\\'") ("procedure" "function" "implementation" "axiom" "const" "type" "var" "if" "else" "while" "return" "requires" "ensures" "invariant" "modifies" "assert" "assume"))
           (smt-mode "SMT-LIB" ";;" "smt2" ("\\.smt2\\'") ("assert" "declare-fun" "define-fun" "declare-const" "check-sat" "get-model" "set-logic" "push" "pop" "let" "forall" "exists" "as"))
           (isabelle-mode "Isabelle" "(*" "isabelle" ("\\.thy\\'") ("theory" "imports" "begin" "end" "lemma" "theorem" "definition" "fun" "function" "datatype" "type_synonym" "locale" "interpretation" "apply" "done" "by" "proof" "qed"))
           (ats-mode "ATS" "//" "ats" ("\\.dats\\'" "\\.sats\\'" "\\.hats\\'") ("fun" "fn" "val" "var" "if" "then" "else" "case" "of" "implement" "extern" "typedef" "datatype" "sortdef" "stadef" "prfun" "prval"))
           (koka-mode "Koka" "//" "koka" ("\\.kk\\'") ("fun" "fn" "val" "var" "if" "then" "else" "match" "return" "type" "effect" "control" "handle" "handler" "module" "import" "public" "private"))
           (brainfuck-mode "Brainfuck" "#" "brainfuck" ("\\.bf\\'" "\\.b\\'") ("brainfuck"))
           (lolcode-mode "LOLCODE" "BTW" "lolcode" ("\\.lol\\'") ("HAI" "KTHXBYE" "VISIBLE" "HAS" "ITZ" "SUM" "DIFF" "PRODUKT" "QUOSHUNT" "MOD" "BIGGR" "SMALLR" "BOTH" "EITHER" "WON" "NOT" "MAEK" "GIMMEH" "RLY" "YA" "NO" "WAI" "OIC" "IM" "IN" "YR" "UPPIN" "NERFIN" "TIL" "WILE" "GTFO" "HOW" "IZ" "IF" "SAY" "SO" "FOUND" "YR"))
           (rockstar-mode "Rockstar" "(" "rockstar" ("\\.rock\\'") ("Listen" "Say" "Shout" "Whisper" "If" "Else" "While" "Until" "Build" "Knock" "Take" "Put" "into" "Give" "back" "takes"))
           (arnoldc-mode "ArnoldC" "#" "arnoldc" ("\\.arnoldc\\'") ("IT'S" "SHOWTIME" "YOU" "HAVE" "BEEN" "TERMINATED" "TALK" "TO" "THE" "HAND" "GET" "UP" "GET" "DOWN" "YOU'RE" "FIRED" "HE" "HAD" "TO" "SPLIT" "BECAUSE" "I'M" "GOING" "TO" "SAY" "PLEASE" "BULLSHIT" "YOU" "ARE" "NOT" "YOU" "ARE" "LISTEN" "TO" "ME" "VERY" "CAREFULLY"))
           (emojicode-mode "Emojicode" "#" "emojicode" ("\\.emojic\\'") ("emojicode"))
           (inform7-mode "Inform 7" "[" "inform7" ("\\.ni\\'" "\\.i7\\'") ("The" "When" "play" "begins" "Instead" "of" "Check" "Carry" "out" "Report" "Say" "If" "otherwise" "To" "Definition" "Understand" "does" "the" "player" "mean"))
           (yarn-mode "Yarn" "//" "yarn" ("\\.yarn\\'") ("title" "tags" "color" "if" "else" "endif" "set" "jump" "declare" "once" "command"))
           (abap-mode "ABAP" "*" "abap" ("\\.abap\\'") ("REPORT" "PROGRAM" "CLASS" "METHOD" "ENDMETHOD" "DATA" "IF" "ELSE" "ENDIF" "LOOP" "ENDLOOP" "SELECT" "ENDSELECT" "WRITE" "FORM" "ENDFORM" "TYPES" "CONSTANTS"))
           (rpg-mode "RPG" "//" "rpg" ("\\.rpgle\\'") ("dcl-s" "dcl-ds" "dcl-c" "dcl-proc" "end-proc" "if" "else" "endif" "dow" "enddo" "for" "endfor" "return" "begsr" "endsr" "select" "when" "endsl"))
           (pli-mode "PL/I" "/*" "pli" ("\\.pli\\'" "\\.pl1\\'") ("PROCEDURE" "END" "DECLARE" "IF" "THEN" "ELSE" "DO" "SELECT" "WHEN" "OTHERWISE" "RETURN" "CALL" "PUT" "GET" "DCL"))
           (rexx-mode "REXX" "--" "rexx" ("\\.rexx\\'" "\\.rex\\'") ("parse" "arg" "if" "then" "else" "do" "end" "return" "exit" "call" "procedure" "say" "pull" "select" "when" "otherwise" "iterate" "leave"))
           (smalltalk-mode "Smalltalk" "\"" "smalltalk" ("\\.st\\'") ("class" "subclass" "instanceVariableNames" "classVariableNames" "ifTrue" "ifFalse" "whileTrue" "whileFalse" "self" "super" "true" "false" "nil" "new"))
           (io-lang-mode "Io" "//" "io" ("\\.io\\'") ("method" "if" "then" "else" "for" "while" "return" "clone" "Object" "List" "self" "block"))
           (factor-mode "Factor" "!" "factor" ("\\.factor\\'") ("USING:" "IN:" "TUPLE:" "GENERIC:" "M:" ":" "if" "when" "unless" "each" "map" "dup" "drop" "swap" "nip" "over"))
           (red-lang-mode "Red" ";;" "red" ("\\.red\\'") ("Red" "func" "function" "if" "either" "while" "until" "foreach" "forall" "return" "set" "context" "object" "view"))
           (rebol-mode "Rebol" ";;" "rebol" ("\\.reb\\'") ("func" "function" "if" "either" "while" "until" "foreach" "forall" "return" "set" "context" "object" "make" "reduce"))
           (oberon-mode "Oberon" "(*" "oberon" ("\\.ob2\\'") ("MODULE" "IMPORT" "PROCEDURE" "BEGIN" "END" "IF" "THEN" "ELSE" "ELSIF" "WHILE" "DO" "REPEAT" "UNTIL" "FOR" "RETURN" "TYPE" "CONST" "VAR" "RECORD" "ARRAY" "OF" "POINTER"))
           (eiffel-mode "Eiffel" "--" "eiffel" ("\\.e\\'") ("class" "inherit" "create" "feature" "do" "end" "if" "then" "else" "elseif" "from" "until" "loop" "require" "ensure" "local" "deferred" "redefine" "is"))
           (icon-lang-mode "Icon" "#" "icon" ("\\.icn\\'") ("procedure" "end" "if" "then" "else" "while" "do" "every" "until" "return" "fail" "suspend" "local" "global" "static" "case" "of"))
           (maxima-mode "Maxima" "/*" "maxima" ("\\.mac\\'") ("block" "if" "then" "else" "for" "while" "do" "thru" "return" "define" "local" "lambda"))
           (gap-lang-mode "GAP" "#" "gap" ("\\.gap\\'" "\\.gi\\'") ("function" "local" "if" "then" "else" "elif" "fi" "for" "do" "od" "while" "return" "end" "rec" "IsList"))
           (alloy-mode "Alloy" "--" "alloy" ("\\.als\\'") ("module" "open" "sig" "fact" "pred" "fun" "assert" "check" "run" "for" "but" "exactly" "extends" "in" "lone" "one" "some" "all" "no" "let"))
           (tla-mode "TLA+" "(*" "tla" ("\\.tla\\'") ("MODULE" "EXTENDS" "CONSTANTS" "VARIABLES" "THEOREM" "ASSUME" "LOCAL" "INSTANCE" "WITH" "IF" "THEN" "ELSE" "LET" "IN" "EXCEPT" "UNCHANGED" "ENABLED" "SUBSET"))
           (promela-mode "Promela" "/*" "promela" ("\\.pml\\'") ("proctype" "init" "if" "fi" "do" "od" "atomic" "d_step" "unless" "timeout" "skip" "break" "goto" "printf" "typedef" "mtype" "chan" "of" "xr" "xs"))
           (yang-mode "YANG" "//" "yang" ("\\.yang\\'") ("module" "submodule" "namespace" "prefix" "import" "include" "revision" "container" "list" "leaf" "leaf-list" "grouping" "uses" "typedef" "type" "mandatory" "config" "description"))
           (asn1-mode "ASN.1" "--" "asn1" ("\\.asn\\'" "\\.asn1\\'") ("DEFINITIONS" "BEGIN" "END" "IMPORTS" "EXPORTS" "FROM" "SEQUENCE" "SET" "CHOICE" "INTEGER" "BOOLEAN" "OCTET" "STRING" "OF" "OPTIONAL" "DEFAULT" "IMPLICIT" "EXPLICIT"))
           (smithy-mode "Smithy" "//" "smithy" ("\\.smithy\\'") ("namespace" "use" "metadata" "service" "resource" "operation" "structure" "list" "map" "union" "string" "integer" "boolean" "document" "apply"))
           (sieve-mode "Sieve" "#" "sieve" ("\\.sieve\\'") ("require" "if" "elsif" "else" "stop" "keep" "discard" "redirect" "fileinto" "reject" "vacation" "notify" "set" "header" "address" "size"))
           (djot-mode "Djot" "%" "djot" ("\\.dj\\'") ("djot"))
           (d2-mode "D2" "#" "d2" ("\\.d2\\'") ("shape" "style" "direction" "label" "icon" "link" "tooltip" "constraint" "near" "grid-rows" "grid-columns"))
           (gcode-mode "G-code" ";" "gcode" ("\\.gcode\\'" "\\.ngc\\'" "\\.nc\\'") ("G0" "G1" "G2" "G3" "G17" "G20" "G21" "G28" "G90" "G91" "M3" "M5" "M30"))
           (cmd-mode "Batch" "rem" "batch" ("\\.bat\\'" "\\.cmd\\'") ("echo" "set" "if" "else" "goto" "call" "for" "in" "do" "exist" "not" "equ" "neq" "defined" "exit" "shift" "start"))
           (nsis-mode "NSIS" ";" "nsis" ("\\.nsi\\'" "\\.nsh\\'") ("Name" "OutFile" "InstallDir" "Section" "SectionEnd" "Function" "FunctionEnd" "Page" "File" "WriteRegStr" "ReadRegStr" "CreateDirectory" "SetOutPath" "!include" "!define" "!ifdef" "!endif"))
           (move-mode "Move" "//" "move" ("\\.move\\'") ("module" "script" "public" "fun" "struct" "has" "acquires" "use" "friend" "if" "else" "while" "loop" "return" "abort" "let" "mut" "const" "spec" "native"))
           (cairo-mode "Cairo" "//" "cairo" ("\\.cairo\\'") ("fn" "mod" "use" "struct" "enum" "trait" "impl" "if" "else" "loop" "match" "return" "let" "mut" "const" "pub" "of" "nopanic" "type"))
           (vyper-mode "Vyper" "#" "vyper" ("\\.vy\\'") ("def" "struct" "event" "interface" "enum" "if" "else" "for" "in" "return" "assert" "raise" "import" "from" "public" "external" "internal" "view" "pure" "payable" "immutable"))
           (sway-mode "Sway" "//" "sway" ("\\.sw\\'") ("contract" "script" "predicate" "library" "fn" "struct" "enum" "impl" "trait" "if" "else" "while" "match" "return" "let" "mut" "use" "pub" "storage" "abi"))
           (motoko-mode "Motoko" "//" "motoko" ("\\.mo\\'") ("actor" "module" "func" "public" "shared" "query" "if" "else" "switch" "case" "while" "for" "return" "let" "var" "type" "object" "class" "import"))
           (cadence-mode "Cadence" "//" "cadence" ("\\.cdc\\'") ("pub" "fun" "access" "contract" "struct" "resource" "event" "if" "else" "while" "for" "return" "let" "var" "import" "from" "emit" "create" "destroy" "transaction" "prepare" "execute"))
           (clarity-mode "Clarity" ";;" "clarity" ("\\.clar\\'") ("define-public" "define-private" "define-read-only" "define-constant" "define-data-var" "define-map" "define-fungible-token" "define-non-fungible-token" "let" "if" "begin" "match" "try" "asserts" "contract-call"))
           (noir-mode "Noir" "//" "noir" ("\\.nr\\'") ("fn" "mod" "use" "struct" "if" "else" "for" "while" "return" "let" "mut" "pub" "global" "constrain" "comptime" "impl" "trait"))
           (leo-mode "Leo" "//" "leo" ("\\.leo\\'") ("program" "function" "struct" "record" "mapping" "if" "else" "for" "return" "let" "const" "transition" "inline" "import" "console"))
           (circom-mode "Circom" "//" "circom" ("\\.circom\\'") ("pragma" "include" "template" "component" "signal" "input" "output" "var" "function" "if" "else" "for" "while" "return" "public" "bus"))
           (pact-mode "Pact" ";;" "pact" ("\\.pact\\'") ("module" "defun" "defcap" "defconst" "defschema" "deftable" "let" "if" "interface" "implements" "use" "bless" "step" "step-with-rollback"))
           (michelson-mode "Michelson" "#" "michelson" ("\\.tz\\'") ("parameter" "storage" "code" "unit" "int" "nat" "string" "pair" "list" "map" "option" "or" "lambda" "if" "loop" "dip" "push" "car" "cdr" "nil" "cons"))
           (algoteal-mode "Algorand TEAL" "//" "algoteal" ("\\.teal\\'") ("int" "byte" "txn" "gtxn" "global" "arg" "load" "store" "bnz" "bz" "return" "err" "assert" "app_global_get" "itxn_begin" "itxn_submit"))
           (huff-mode "Huff" "//" "huff" ("\\.huff\\'") ("#define" "macro" "fn" "takes" "returns" "jump" "jumpi" "dup1" "swap1" "calldataload" "mstore" "sstore" "revert"))
           (yul-mode "Yul" "//" "yul" ("\\.yul\\'") ("object" "code" "data" "function" "let" "if" "switch" "case" "default" "for" "leave" "break" "continue" "true" "false"))
           (fe-mode "Fe" "//" "fe" ("\\.fe\\'") ("contract" "pub" "fn" "struct" "enum" "event" "if" "else" "while" "for" "return" "let" "mut" "use" "const" "type" "impl"))
           (aiken-mode "Aiken" "//" "aiken" ("\\.ak\\'") ("use" "pub" "fn" "type" "if" "else" "when" "is" "let" "expect" "test" "validator" "mint" "spend" "withdraw" "opaque"))
           (scilla-mode "Scilla" "(*" "scilla" ("\\.scilla\\'") ("let" "in" "fun" "tfun" "match" "with" "end" "library" "import" "contract" "transition" "procedure" "field" "accept" "send"))
           (daml-mode "Daml" "--" "daml" ("\\.daml\\'") ("module" "template" "choice" "controller" "do" "if" "then" "else" "return" "let" "in" "data" "where" "signatory" "observer" "ensure" "create" "exercise" "archive"))
           (grain-mode "Grain" "//" "grain" ("\\.gr\\'") ("module" "import" "export" "let" "fn" "if" "else" "while" "for" "match" "return" "data" "type" "record" "enum" "provide"))
           (nickel-mode "Nickel" "#" "nickel" ("\\.ncl\\'") ("let" "in" "if" "then" "else" "fun" "forall" "match" "import" "default" "doc" "force" "rec"))
           (kcl-mode "KCL" "#" "kcl" ("\\.k\\'") ("schema" "mixin" "protocol" "check" "if" "else" "elif" "for" "in" "import" "lambda" "assert" "rule"))
           (elvish-mode "Elvish" "#" "elvish" ("\\.elv\\'") ("fn" "if" "else" "while" "for" "try" "catch" "finally" "use" "var" "set" "return" "break" "continue"))
           (ysh-mode "YSH" "#" "ysh" ("\\.ysh\\'") ("proc" "func" "if" "else" "elif" "for" "while" "return" "const" "var" "setvar" "module" "source" "case" "fork"))
           (typst-mode "Typst" "//" "typst" ("\\.typ\\'") ("let" "set" "show" "import" "include" "if" "else" "for" "while" "return" "context" "pagebreak" "align" "text" "heading"))
           (zscript-mode "ZScript" "//" "zscript" ("\\.zs\\'") ("class" "struct" "enum" "mixin" "if" "else" "for" "while" "do" "return" "switch" "case" "default" "virtual" "override" "states" "let" "const"))
           (mcfunction-mode "Minecraft Function" "#" "mcfunction" ("\\.mcfunction\\'") ("execute" "say" "tellraw" "give" "summon" "setblock" "fill" "scoreboard" "function" "tag" "data" "tp" "kill" "effect" "playsound"))
           (byond-mode "BYOND DM" "//" "dm" ("\\.dm\\'") ("mob" "obj" "turf" "area" "proc" "verb" "var" "if" "else" "for" "while" "return" "new" "del" "set" "switch" "spawn"))
           (unison-mode "Unison" "--" "unison" ("\\.u\\'") ("type" "ability" "structural" "unique" "if" "then" "else" "match" "with" "cases" "handle" "where" "let" "do" "forall"))
           (cedar-mode "Cedar" "//" "cedar" ("\\.cedar\\'") ("permit" "forbid" "when" "unless" "principal" "action" "resource" "context" "in" "has" "like" "if" "then" "else" "is"))
           (polar-mode "Polar" "#" "polar" ("\\.polar\\'") ("allow" "resource" "actor" "has_relation" "has_role" "if" "and" "or" "not" "matches" "cut"))
           (moonscript-mode "MoonScript" "--" "moonscript" ("\\.moon\\'") ("class" "extends" "export" "import" "with" "if" "else" "elseif" "then" "super" "local" "while" "for" "in" "return" "switch" "when"))
           (cobol-mode "COBOL" "*>" "cobol" ("\\.cob\\'" "\\.cbl\\'" "\\.cobol\\'" "\\.cpy\\'") ("IDENTIFICATION" "DIVISION" "PROGRAM-ID" "ENVIRONMENT" "DATA" "PROCEDURE" "WORKING-STORAGE" "LOCAL-STORAGE" "LINKAGE" "SECTION" "CONFIGURATION" "INPUT-OUTPUT" "FILE-CONTROL" "SELECT" "ASSIGN" "ORGANIZATION" "ACCESS" "FILE" "FD" "SD" "PIC" "PICTURE" "PERFORM" "VARYING" "UNTIL" "TIMES" "THRU" "THROUGH" "END-PERFORM" "MOVE" "TO" "IF" "ELSE" "END-IF" "EVALUATE" "WHEN" "END-EVALUATE" "DISPLAY" "ACCEPT" "COMPUTE" "ADD" "SUBTRACT" "MULTIPLY" "DIVIDE" "GIVING" "REMAINDER" "ROUNDED" "GOTO" "STOP" "RUN" "OPEN" "CLOSE" "READ" "WRITE" "REWRITE" "DELETE" "START" "END-READ" "END-WRITE" "CALL" "USING" "RETURNING" "END-CALL" "EXIT" "GOBACK" "CONTINUE" "STRING" "UNSTRING" "INSPECT" "INITIALIZE" "SET" "TRUE" "FALSE" "NOT" "AND" "OR" "OCCURS" "REDEFINES" "VALUE" "COPY" "REPLACE" "REPLACING" "EXEC" "END-EXEC" "SEARCH" "SORT" "MERGE" "RETURN" "INTO" "FROM" "DEPENDING" "INDEXED" "KEY" "ALPHABETIC" "NUMERIC" "COMP" "COMP-3" "BINARY" "PACKED-DECIMAL" "SIGN" "LEADING" "TRAILING" "JUSTIFIED" "BLANK" "FUNCTION" "SCREEN" "REPORT" "CLASS" "REPOSITORY" "METHOD-ID" "CLASS-ID" "OBJECT" "METHOD" "END-METHOD" "INVOKE" "SELF" "SUPER" "FACTORY" "ABSTRACT" "OVERRIDE" "FINAL") t)
           (algol60-mode "ALGOL 60" "comment" "algol60" ("\\.alg\\'" "\\.a60\\'") ("begin" "end" "if" "then" "else" "for" "step" "until" "do" "while" "own" "Boolean" "integer" "real" "array" "switch" "procedure" "goto" "value" "string" "label") t)
           (algol68-mode "ALGOL 68" "#" "algol68" ("\\.a68\\'") ("begin" "end" "if" "then" "else" "elif" "fi" "while" "do" "od" "for" "from" "by" "to" "case" "esac" "mode" "op" "proc" "ref" "flex" "heap" "loc" "struct" "union" "int" "real" "bool" "char" "string" "void" "goto" "skip" "exit" "par" "nil") t)
           (snobol-mode "SNOBOL" "*" "snobol" ("\\.sno\\'" "\\.snobol\\'" "\\.sbl\\'") ("INPUT" "OUTPUT" "END" "DEFINE" "RETURN" "FRETURN" "NRETURN" "EQ" "NE" "GE" "GT" "LE" "LT" "INTEGER" "IDENT" "DIFFER" "ANY" "NOTANY" "SPAN" "BREAK" "LEN" "POS" "RPOS" "TAB" "RTAB" "ARB" "BAL" "FAIL" "SUCCEED" "REM" "ABORT") t)
           (bcpl-mode "BCPL" "//" "bcpl" ("\\.bcpl\\'") ("LET" "MANIFEST" "GLOBAL" "BE" "TEST" "IF" "UNLESS" "THEN" "ELSE" "WHILE" "UNTIL" "REPEAT" "REPEATWHILE" "REPEATUNTIL" "FOR" "TO" "BY" "DO" "VALOF" "RESULTIS" "SWITCHON" "CASE" "DEFAULT" "GOTO" "BREAK" "LOOP" "RETURN" "FINISH" "AND" "OR" "NOT" "TRUE" "FALSE" "VEC"))
           (simula-mode "Simula" "!" "simula" ("\\.sim\\'") ("begin" "end" "class" "procedure" "if" "then" "else" "while" "do" "for" "step" "until" "inspect" "when" "otherwise" "this" "qua" "new" "ref" "integer" "real" "Boolean" "character" "text"))
           (clu-mode "CLU" "%" "clu" ("\\.clu\\'") ("cluster" "proc" "iter" "rep" "type" "if" "then" "else" "elseif" "while" "do" "for" "yield" "return" "except" "when" "others" "signal" "resign" "tagcase" "where" "begin" "end"))
           (setl-mode "SETL" "$" "setl" ("\\.setl\\'") ("program" "end" "if" "then" "else" "elseif" "while" "do" "for" "in" "loop" "exists" "forall" "proc" "procedure" "return" "case" "otherwise" "not" "and" "or"))
           (jovial-mode "JOVIAL" "\"" "jovial" ("\\.jov\\'" "\\.jovial\\'") ("START" "TERM" "PROC" "PROCEDURE" "IF" "THEN" "ELSE" "BEGIN" "END" "FOR" "WHILE" "GOTO" "RETURN" "ITEM" "TABLE" "STATUS" "DEFINE") t)
           (mumps-mode "MUMPS" ";" "mumps" ("\\.mumps\\'" "\\.mps\\'") ("SET" "IF" "ELSE" "FOR" "QUIT" "WRITE" "READ" "KILL" "DO" "GOTO" "NEW" "MERGE" "OPEN" "CLOSE" "USE" "LOCK" "JOB" "HALT" "HANG" "VIEW" "XECUTE" "BREAK" "CONTINUE") t)
           (jcl-mode "JCL" "//*" "jcl" ("\\.jcl\\'") ("JOB" "EXEC" "DD" "PROC" "PEND" "SET" "IF" "THEN" "ELSE" "ENDIF" "INCLUDE" "JCLLIB" "OUTPUT" "CNTL" "ENDCNTL" "COMMAND") t)
           (plm-mode "PL/M" "/*" "plm" ("\\.plm\\'") ("DECLARE" "PROCEDURE" "IF" "THEN" "ELSE" "DO" "END" "CALL" "RETURN" "GOTO" "WHILE" "CASE" "LITERALLY" "BYTE" "ADDRESS" "STRUCTURE") t)
           (logo-mode "Logo" ";" "logo" ("\\.logo\\'") ("to" "end" "repeat" "while" "if" "ifelse" "make" "local" "output" "stop" "fd" "bk" "lt" "rt" "pu" "pd" "setxy" "home" "foreach") t)
           (comal-mode "COMAL" "//" "comal" ("\\.comal\\'") ("PROC" "ENDPROC" "FUNC" "ENDFUNC" "IF" "THEN" "ELSE" "ELIF" "ENDIF" "WHILE" "DO" "ENDWHILE" "REPEAT" "UNTIL" "FOR" "TO" "STEP" "NEXT" "CASE" "WHEN" "OTHERWISE" "ENDCASE" "DIM" "RETURN") t)
           (pilot-mode "PILOT" "R:" "pilot" ("\\.pilot\\'") ("TYPE" "ACCEPT" "MATCH" "JUMP" "USE" "COMPUTE" "PROBLEM" "END" "WAIT" "LINK" "YES" "NO") t)
           (bliss-mode "BLISS" "!" "bliss" ("\\.bli\\'" "\\.bliss\\'") ("MODULE" "BEGIN" "END" "IF" "THEN" "ELSE" "WHILE" "DO" "ROUTINE" "BIND" "LOCAL" "GLOBAL" "MACRO" "EXTERNAL" "FORWARD" "LEAVE" "RETURN" "SELECT" "OTHERWISE"))
           (coral66-mode "CORAL 66" "COMMENT" "coral66" ("\\.coral\\'") ("BEGIN" "END" "IF" "THEN" "ELSE" "FOR" "WHILE" "DO" "PROCEDURE" "INTEGER" "REAL" "BOOLEAN" "COMMENT" "GOTO" "SWITCH") t)
           (mad-mode "MAD" "C " "mad" ("\\.mad\\'") ("PROGRAM" "INTEGER" "WHENEVER" "END" "CONDITIONAL" "TRANSFER" "FUNCTION" "EXTERNAL" "DIMENSION" "THROUGH" "OTHERWISE") t)
           (flowmatic-mode "FLOW-MATIC" "." "flowmatic" ("\\.flowmatic\\'") ("INPUT" "OUTPUT" "COMPARE" "TRANSFER" "JUMP" "STOP" "EQUAL" "GREATER" "OPERATION") t)
           (turing-mode "Turing" "%" "turing" ("\\.turing\\'" "\\.tu\\'") ("var" "procedure" "function" "if" "then" "elsif" "else" "end" "loop" "for" "while" "case" "result" "return" "type" "record" "const" "put" "get" "module" "export" "import" "begin"))
           (modula3-mode "Modula-3" "(*" "modula3" ("\\.m3\\'" "\\.i3\\'") ("MODULE" "INTERFACE" "IMPORT" "EXPORT" "PROCEDURE" "BEGIN" "END" "IF" "THEN" "ELSE" "ELSIF" "VAR" "CONST" "TYPE" "RECORD" "ARRAY" "REF" "OBJECT" "METHODS" "OVERRIDE" "NEW" "RAISES" "EXCEPTION" "TRY" "EXCEPT" "FINALLY" "LOOP" "WHILE" "FOR" "DO" "RETURN" "EXIT"))
           (mesa-lang-mode "Mesa" "--" "mesa" ("\\.mesa\\'") ("DEFINITIONS" "PROGRAM" "BEGIN" "END" "IF" "THEN" "ELSE" "WHILE" "DO" "PROCEDURE" "RETURNS" "MODULE" "IMPORTS" "EXPORTS" "RECORD"))
           (euclid-mode "Euclid" "--" "euclid" ("\\.euclid\\'") ("var" "procedure" "function" "if" "then" "else" "begin" "end" "module" "imports" "exports" "type" "const" "assert" "return"))
           (alphard-mode "Alphard" "--" "alphard" ("\\.alphard\\'") ("form" "proc" "if" "then" "else" "begin" "end" "pre" "post"))
           (apl-mode "APL" "⍝" "apl" ("\\.apl\\'") ("If" "Else" "ElseIf" "EndIf" "While" "EndWhile" "For" "EndFor" "Select" "Case" "AndIf" "OrIf" "Until" "Repeat" "Leave" "Continue" "GoTo" "Return" "Namespace" "Class" "Section" "Property" "Trap") t)
           (j-lang-mode "J" "NB." "j" ("\\.ijs\\'") ("if." "do." "else." "elseif." "end." "while." "whilst." "for." "select." "case." "fcase." "throw." "catch." "try." "return." "break." "continue." "assert."))
           (bqn-mode "BQN" "#" "bqn" ("\\.bqn\\'") ("Import" "ReBQN" "Show" "Exit" "List" "FFI" "file" "state" "args" "path" "name" "type" "glyph"))
           (uiua-mode "Uiua" "#" "uiua" ("\\.ua\\'") ("dup" "flip" "over" "pop" "not" "abs" "sign" "sqrt" "floor" "ceil" "round" "add" "sub" "mul" "div" "mod" "pow" "log" "min" "max" "range" "reverse" "transpose" "sort" "where" "pick" "select" "reshape" "join" "box" "repeat" "reduce" "rows" "table" "group"))
           (clean-lang-mode "Clean" "//" "clean" ("\\.icl\\'" "\\.dcl\\'") ("module" "import" "implementation" "definition" "if" "then" "else" "where" "let" "in" "case" "class" "instance" "infix" "from" "qualified" "generic" "derive"))
           (miranda-mode "Miranda" "||" "miranda" ("\\.mira\\'" "\\.mra\\'") ("where" "if" "otherwise" "abstype" "with" "type" "show"))
           (hope-mode "Hope" "!" "hope" ("\\.hope\\'") ("dec" "data" "type" "if" "then" "else" "let" "in" "where"))
           (sasl-mode "SASL" "||" "sasl" ("\\.sasl\\'") ("where" "if" "let" "def" "then" "else"))
           (krc-mode "KRC" "||" "krc" ("\\.krc\\'") ("where" "if" "then" "else" "def"))
           (orwell-mode "Orwell" "||" "orwell" ("\\.orwell\\'") ("where" "if" "otherwise" "then" "else"))
           (curry-mode "Curry" "--" "curry" ("\\.curry\\'") ("module" "import" "data" "type" "if" "then" "else" "case" "let" "in" "where" "do" "free" "fcase" "eval"))
           (mercury-mode "Mercury" "%" "mercury" ("\\.mercury\\'" "\\.mer\\'") ("pred" "func" "mode" "type" "if" "then" "else" "det" "semidet" "nondet" "multi" "failure" "module" "import" "interface" "some" "all" "not" "inst"))
           (oz-mode "Oz" "%" "oz" ("\\.oz\\'") ("functor" "import" "export" "define" "proc" "fun" "if" "then" "else" "elseif" "case" "thread" "local" "in" "end" "class" "meth" "self" "true" "false" "nil" "lock" "try" "catch" "finally"))
           (flix-mode "Flix" "//" "flix" ("\\.flix\\'") ("def" "pub" "enum" "type" "if" "else" "match" "case" "let" "law" "eff" "import" "trait" "instance" "where" "region"))
           (carp-mode "Carp" ";;" "carp" ("\\.carp\\'") ("defn" "def" "defmodule" "let" "if" "while" "fn" "use" "doc"))
           (shen-mode "Shen" "\\\\" "shen" ("\\.shen\\'") ("define" "defun" "lambda" "if" "and" "or" "package" "datatype"))
           (arc-mode "Arc" ";" "arc" ("\\.arc\\'") ("def" "defn" "mac" "if" "let" "fn" "and" "or" "when" "with"))
           (txr-mode "TXR" ";" "txr" ("\\.txr\\'") ("defun" "defmacro" "if" "let" "when" "progn" "lambda" "defstruct"))
           (newlisp-mode "newLISP" ";" "newlisp" ("\\.newlisp\\'") ("define" "lambda" "if" "let" "when" "dolist" "dotimes" "begin" "local" "setq" "fn" "and" "or" "not" "import" "context"))
           (picolisp-mode "PicoLisp" "#" "picolisp" ("\\.pil\\'") ("de" "dm" "if" "let" "when" "prog" "loop" "for" "while" "setq" "load" "nil"))
           (bel-mode "Bel" ";" "bel" ("\\.bel\\'") ("def" "defun" "mac" "if" "let" "fn" "when" "or" "and"))
           (faust-mode "Faust" "//" "faust" ("\\.dsp\\'" "\\.faust\\'") ("process" "with" "letrec" "case" "if" "import" "component" "library" "environment" "declare"))
           (joy-mode "Joy" "(*" "joy" ("\\.joy\\'") ("DEFINE" "MODULE" "HIDE" "LIBRA" "PUBLIC" "PRIVATE" "END"))
           (kitten-mode "Kitten" "//" "kitten" ("\\.ktn\\'") ("define" "synonym" "about" "type" "if" "else" "while" "return"))
           (eff-mode "Eff" "(*" "eff" ("\\.eff\\'") ("effect" "operation" "let" "handle" "handler" "if" "then" "else" "match" "with" "type" "perform"))
           (kind-lang-mode "Kind" "//" "kind" ("\\.kind\\'") ("type" "def" "if" "else" "match" "case" "let" "open"))
           (whiley-mode "Whiley" "//" "whiley" ("\\.whiley\\'") ("function" "method" "if" "else" "while" "for" "return" "type" "import" "public" "native" "where" "ensures" "requires" "assert"))
           (eta-mode "Eta" "--" "eta" ("\\.eta\\'") ("module" "import" "data" "type" "if" "then" "else" "where" "let" "in" "case" "do" "class" "instance" "foreign"))
           (alpaca-mode "Alpaca" "--" "alpaca" ("\\.alpaca\\'") ("module" "export" "type" "let" "receive" "if" "case" "error"))
           (hamler-mode "Hamler" "--" "hamler" ("\\.hmr\\'") ("module" "import" "if" "then" "else" "case" "data" "type" "where"))
           (dex-mode "Dex" "--" "dex" ("\\.dx\\'") ("def" "for" "if" "then" "else" "case" "interface" "instance"))
           (sisal-mode "SISAL" "%" "sisal" ("\\.sis\\'" "\\.sisal\\'") ("function" "returns" "if" "then" "else" "for" "in" "let"))
           (nesl-mode "NESL" "%" "nesl" ("\\.nesl\\'") ("function" "if" "then" "else" "in" "let"))
           (sequencel-mode "SequenceL" "//" "sequencel" ("\\.seql\\'") ("if" "then" "else" "for" "while" "return" "function"))
           (hume-mode "Hume" "--" "hume" ("\\.hume\\'") ("box" "wire" "if" "then" "else" "match"))
           (fortress-mode "Fortress" "(*" "fortress" ("\\.fss\\'") ("component" "export" "api" "trait" "object" "if" "then" "else" "while" "do" "for" "type"))
           (x10-mode "X10" "//" "x10" ("\\.x10\\'") ("class" "def" "if" "else" "for" "while" "async" "finish" "at" "type" "val" "var" "return" "try" "catch" "finally"))
           (lucid-mode "Lucid" "%" "lucid" ("\\.lucid\\'") ("where" "if" "then" "else" "fby" "asa" "upon"))
           (refal-mode "Refal" "*" "refal" ("\\.ref\\'" "\\.refal\\'") ("ENTRY" "EXTRN" "EXTERNAL"))
           (pop11-mode "POP-11" ";" "pop11" ("\\.pop\\'" "\\.p11\\'") ("define" "enddefine" "vars" "if" "then" "else" "elseif" "endif" "while" "do" "endwhile" "for" "endfor" "procedure" "return"))
           (jass-mode "JASS" "//" "jass" ("\\.jass\\'" "\\.vjass\\'") ("function" "endfunction" "if" "then" "else" "elseif" "endif" "loop" "endloop" "exitwhen" "return" "local" "set" "call" "takes" "returns" "nothing" "integer" "real" "string" "boolean" "handle" "constant" "native" "globals" "endglobals" "library" "endlibrary" "struct" "endstruct" "method" "endmethod" "private" "public" "static"))
           (zinc-mode "Zinc" "//" "zinc" ("\\.zn\\'") ("function" "if" "else" "while" "for" "return" "local" "struct"))
           (galaxy-mode "Galaxy" "//" "galaxy" ("\\.galaxy\\'") ("void" "int" "bool" "string" "if" "else" "while" "for" "return" "include" "struct" "funcref" "trigger" "true" "false"))
           (acs-mode "ACS" "//" "acs" ("\\.acs\\'") ("script" "if" "else" "while" "for" "until" "do" "return" "int" "str" "void" "bool" "restart" "suspend" "terminate" "include"))
           (decorate-mode "DECORATE" "//" "decorate" ("\\.dec\\'") ("actor" "states" "loop" "stop" "wait" "goto" "fail" "spawn" "see" "melee" "missile" "pain" "death"))
           (luau-mode "Luau" "--" "luau" ("\\.luau\\'") ("local" "function" "if" "then" "else" "elseif" "end" "for" "while" "do" "repeat" "until" "return" "break" "continue" "type" "export" "typeof" "and" "or" "not" "in" "nil" "true" "false"))
           (tal-mode "Uxntal" "(" "tal" ("\\.tal\\'") ("BRK" "LIT" "INC" "POP" "NIP" "SWP" "ROT" "DUP" "OVR" "EQU" "NEQ" "GTH" "LTH" "JMP" "JCN" "JSR" "STH" "LDZ" "STZ" "LDR" "STR" "LDA" "STA" "DEI" "DEO" "ADD" "SUB" "MUL" "DIV" "AND" "ORA" "EOR"))
           (kag-mode "KAG" ";" "kag" ("\\.ks\\'") ("cm" "wait" "link" "jump" "call" "if" "else" "endif" "return" "bg" "bgm" "movie" "chara" "font" "position"))
           (twee-mode "Twee" "/%" "twee" ("\\.twee\\'" "\\.tw2\\'") ("if" "else" "endif" "set" "link" "button" "widget" "script"))
           (weidu-mode "WeiDU" "//" "weidu" ("\\.baf\\'" "\\.bcs\\'" "\\.tp2\\'" "\\.tpa\\'" "\\.tph\\'") ("IF" "THEN" "RESPONSE" "BEGIN" "END" "ACTION" "CHAIN" "PRINT" "REQUIRE" "APPEND" "COPY" "COMPILE"))
           (runescript-mode "RuneScript" "//" "runescript" ("\\.rs2\\'") ("if" "else" "while" "return" "def_int" "def_obj" "def_string" "mes" "calc" "enum"))
           (heroscript-mode "HeroScript" "//" "heroscript" ("\\.hsl\\'") ("function" "if" "else" "while" "for" "return" "var" "script"))
           (torquescript-mode "TorqueScript" "//" "torquescript" ("\\.tscript\\'" "\\.mis\\'") ("function" "if" "else" "while" "for" "return" "datablock" "new" "package" "switch" "case" "default" "break" "continue" "true" "false"))
           (mugen-mode "MUGEN" ";" "mugen" ("\\.cns\\'" "\\.air\\'") ("type" "stateno" "value" "trigger" "triggerall" "anim" "physics"))
           (zil-mode "ZIL" ";" "zil" ("\\.zil\\'") ("ROUTINE" "OBJECT" "ROOM" "IF" "ELSE" "COND" "PROG" "REPEAT" "RETURN" "TELL" "DEFINE" "GLOBAL" "VERB" "SYNTAX"))
           (inform6-mode "Inform 6" "!" "inform6" ("\\.inf\\'") ("Object" "Class" "if" "else" "while" "for" "return" "print" "switch" "with" "has" "Attribute" "Property" "Verb"))
           (tads-mode "TADS" "/*" "tads" ("\\.tads\\'" "\\.t3\\'") ("if" "else" "while" "for" "return" "function" "object" "class" "modify" "replace" "switch" "case" "default" "break" "continue" "local" "true" "nil"))
           (hugo-mode "Hugo" "!" "hugo" ("\\.hug\\'" "\\.hugo\\'") ("routine" "if" "else" "while" "for" "return" "print" "object" "attribute" "property" "verb"))
           (alan-mode "Alan" "--" "alan" ("\\.alan\\'" "\\.ala\\'") ("IF" "THEN" "ELSE" "ENDIF" "WHEN" "DEPENDING" "LOCATION" "ACTOR" "VERB" "DOES" "CHECK" "END") t)
           (dialog-lang-mode "Dialog" "%%" "dialog" ("\\.dg\\'") ("if" "then" "else" "currently" "select" "stop" "query" "or" "not"))
           (qsp-mode "QSP" "!" "qsp" ("\\.qsrc\\'" "\\.qsps\\'") ("if" "else" "act" "gt" "gs" "pl" "addobj" "delobj" "cls"))
           (puzzlescript-mode "PuzzleScript" "(" "puzzlescript" ("\\.puzzlescript\\'" "\\.pz\\'") ("title" "author" "homepage" "objects" "legend" "sounds" "collisionlayers" "rules" "winconditions" "levels" "again" "cancel" "restart" "message"))
           (wurst-mode "Wurst" "//" "wurst" ("\\.wurst\\'") ("package" "function" "if" "else" "while" "for" "return" "class" "interface" "module" "init" "let" "var" "abstract" "override"))
           (clausewitz-mode "Clausewitz" "#" "clausewitz" ("\\.gui\\'" "\\.gfx\\'") ("if" "limit" "else" "trigger" "AND" "OR" "NOT" "allow" "effect" "option"))
           (supercollider-mode "SuperCollider" "//" "supercollider" ("\\.sc\\'" "\\.scd\\'") ("var" "arg" "if" "else" "while" "for" "SynthDef" "play" "fork" "true" "false" "nil"))
           (ink-mode "Ink" "//" "ink" ("\\.ink\\'") ("VAR" "CONST" "LIST" "INCLUDE" "EXTERNAL" "function" "return" "temp" "choice"))
           (choicescript-mode "ChoiceScript" "*comment" "choicescript" ("\\.choicescript\\'") ("title" "author" "choice" "label" "goto" "finish" "set" "if" "else" "elseif" "fake_choice" "page_break" "ending" "achieve"))
           (chaiscript-mode "ChaiScript" "//" "chaiscript" ("\\.chai\\'") ("def" "var" "if" "else" "while" "for" "return" "true" "false" "fun" "global"))
           (gamemonkey-mode "GameMonkey" "//" "gamemonkey" ("\\.gm\\'") ("global" "function" "if" "else" "while" "for" "return" "local" "and" "or" "null" "true" "false"))
           (nml-mode "NML" "//" "nml" ("\\.nml\\'") ("grf" "replace" "spriteset" "spritegroup" "spritelayout" "item" "if" "else" "while" "return"))
           (ags-mode "AGS Script" "//" "ags" ("\\.ash\\'") ("function" "if" "else" "while" "for" "return" "import" "export" "struct" "int" "String" "bool" "true" "false"))
           (con-lang-mode "CON" "//" "con" ("\\.con\\'") ("defstate" "state" "getactor" "ends" "enda" "action"))
           ))
  (let ((mode (nth 0 row))
        (title (nth 1 row))
        (comment (nth 2 row))
        (ft (nth 3 row))
        (pats (nth 4 row))
        (words (nth 5 row))
        (fold (nth 6 row)))
    (my/own-mode mode title comment words fold)
    (when (fboundp mode)
      (dolist (pat pats)
        (unless (assoc pat auto-mode-alist)
          (my/add-ft pat mode)))
      (unless (member (cons ft mode) my/extra-ft-cells)
        (push (cons ft mode) my/extra-ft-cells))
      (unless (assoc title my/extra-help-rows)
        (push (list title ft (mapconcat (lambda (e) (concat "." (substring e 2 -2))) pats " "))
              my/extra-help-rows)))))
(my/add-ft "\\.hh\\'" 'c++-mode)
(my/add-ft "\\.hxx\\'" 'c++-mode)
(when (fboundp 'luau-mode)
  (my/drop-ft "\\.luau\\'")
  (my/add-ft "\\.luau\\'" 'luau-mode))
(when (fboundp 'cobol-mode)
  (modify-syntax-entry ?- "_" cobol-mode-syntax-table))
(when (fboundp 'j-lang-mode)
  (modify-syntax-entry ?. "_" j-lang-mode-syntax-table))
(when (fboundp 'choicescript-mode)
  (add-to-list 'magic-mode-alist
               '("\\`\\(?:\\*title\\|\\*choice\\|\\*label\\)" . choicescript-mode)))
(dolist (cell '(("newlisp" . newlisp-mode)
                ("luau" . luau-mode)
                ("cobc" . cobol-mode)))
  (when (fboundp (cdr cell))
    (add-to-list 'interpreter-mode-alist cell)))
;;; Languages added from the long request. Each has its own mode.
(unless (fboundp 'expect-mode)
  (define-derived-mode expect-mode tcl-mode "Expect"
    "Expect scripts."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<"
                 (regexp-opt
                  '("spawn" "expect" "send" "interact" "exp_send"
                    "exp_internal" "log_file" "log_user" "timeout")
                  t)
                 "\\_>")
        0 font-lock-keyword-face t))
     'prepend)))
(unless (fboundp 'xslt-mode)
  (define-derived-mode xslt-mode nxml-mode "XSLT"
    "XSLT stylesheet."
    (font-lock-add-keywords
     nil
     '(("xsl:[A-Za-z-]+" . font-lock-keyword-face))
     'prepend)))
(unless (fboundp 'helm-lang-mode)
  (define-derived-mode helm-lang-mode yaml-mode "Helm"
    "Helm template."
    (font-lock-add-keywords
     nil
     `((,(concat "\\_<"
                 (regexp-opt
                  '("define" "template" "include" "required" "default"
                    "toYaml" "nindent" "quote" "range" "with" "else")
                  t)
                 "\\_>")
        0 font-lock-keyword-face t))
     'prepend)))
(unless (fboundp 'kustomize-mode)
  (define-derived-mode kustomize-mode yaml-mode "Kustomize"
    "Kustomize file."
    (font-lock-add-keywords
     nil
     '(("^[[:space:]]*\\(?:resources\\|patchesStrategicMerge\\|patches\\|configMapGenerator\\|secretGenerator\\|namespace\\|namePrefix\\|nameSuffix\\|commonLabels\\|images\\|replicas\\|components\\):"
        . font-lock-keyword-face))
     'prepend)))
(unless (fboundp 'ansible-mode)
  (define-derived-mode ansible-mode yaml-mode "Ansible"
    "Ansible playbook."
    (font-lock-add-keywords
     nil
     '(("ansible\\.builtin\\|\\_<\\(?:hosts\\|tasks\\|handlers\\|become\\|gather_facts\\|roles\\)\\_>:"
        . font-lock-keyword-face))
     'prepend)))
(unless (fboundp 'befunge-mode)
  (define-derived-mode befunge-mode prog-mode "Befunge"
    "Befunge."))
(unless (fboundp 'whitespace-lang-mode)
  (define-derived-mode whitespace-lang-mode prog-mode "Whitespace"
    "Whitespace."))
(unless (fboundp 'piet-mode)
  (define-derived-mode piet-mode prog-mode "Piet"
    "Piet."))
(unless (fboundp 'malbolge-mode)
  (define-derived-mode malbolge-mode prog-mode "Malbolge"
    "Malbolge."))
(unless (fboundp 'messagepack-mode)
  (define-derived-mode messagepack-mode prog-mode "MessagePack"
    "MessagePack data."))
(unless (fboundp 'cbor-mode)
  (define-derived-mode cbor-mode prog-mode "CBOR"
    "CBOR data."))
(unless (fboundp 'bson-mode)
  (define-derived-mode bson-mode prog-mode "BSON"
    "BSON data."))
(unless (fboundp 'unlambda-mode)
  (define-derived-mode unlambda-mode prog-mode "Unlambda"
    "Unlambda."))
(defun my/befunge-setup ()
  "Befunge-93: digits are numbers, commands are builtins."
  (setq-local font-lock-defaults
              '((("[0-9]" . font-lock-constant-face)
                 ("[][<>^v_|+*/%!:@#$&\\\\,.'`?\"~=-gp]" . font-lock-builtin-face)))))
(add-hook 'befunge-mode-hook #'my/befunge-setup)
(defun my/unlambda-setup ()
  "Application, combinators, and one-character output."
  (setq-local font-lock-defaults
              '((("`" . font-lock-keyword-face)
                 ("[skicdevr@|*]" . font-lock-builtin-face)
                 ("\\.[^[:space:]]" . font-lock-string-face)))))
(add-hook 'unlambda-mode-hook #'my/unlambda-setup)
(defvar my/new-lang-rows
  '(
    (hjson-mode "Hjson" "#" "hjson" ("\\.hjson\\'")
     ("true" "false" "null"))
    (ninja-mode "Ninja" "#" "ninja" ("\\.ninja\\'")
     ("rule" "build" "default" "command" "pool" "include" "subninja" "deps" "msvc_deps_prefix"))
    (m4-mode "M4" "#" "m4" ("\\.m4\\'" "\\.ac\\'")
     ("define" "include" "divert" "dnl" "changequote" "ifdef" "ifelse"))
    (bison-mode "Bison / Yacc" "/*" "bison" ("\\.y\\'" "\\.yy\\'")
     ("token" "type" "union" "left" "right" "nonassoc" "start" "expect"))
    (flex-mode "Flex / Lex" "/*" "flex" ("\\.l\\'")
     ("option" "echo" "reject" "yymore" "unput" "input"))
    (puppet-mode "Puppet" "#" "puppet" ("\\.pp\\'" "\\.epp\\'")
     ("class" "define" "node" "include" "if" "else" "case" "package" "service" "file"))
    (antlr-mode "ANTLR" "//" "antlr" ("\\.g4\\'")
     ("grammar" "lexer" "parser" "import" "fragment" "returns" "locals" "throws"))
    (conf-javaprop-mode "Java Properties" "#" "properties" ("\\.properties\\'")
     ("true" "false"))
    (stylus-mode "Stylus" "//" "stylus" ("\\.styl\\'")
     ("if" "else" "unless" "for" "in" "return"))
    (gotemplate-mode "Go template" "{{/*" "gotemplate" ("\\.tmpl\\'" "\\.tpl\\'" "\\.gotmpl\\'")
     ("define" "template" "block" "if" "else" "end" "range" "with" "nil" "index" "printf" "len"))
    (nunjucks-mode "Nunjucks" "{#" "nunjucks" ("\\.njk\\'")
     ("if" "else" "elif" "endif" "for" "endfor" "macro" "endmacro" "set" "include" "import" "extends" "block" "endblock" "filter" "raw" "endraw"))
    (marko-mode "Marko" "//" "marko" ("\\.marko\\'")
     ("if" "else" "for" "while" "class" "import" "export" "static"))
    (wxml-mode "WXML" "<!--" "wxml" ("\\.wxml\\'")
     ("import" "include" "template" "block"))
    (cypher-mode "Cypher" "//" "cypher" ("\\.cypher\\'" "\\.cql\\'")
     ("MATCH" "WHERE" "RETURN" "CREATE" "MERGE" "DELETE" "DETACH" "REMOVE" "WITH" "UNWIND" "ORDER" "LIMIT" "SKIP" "OPTIONAL" "CALL" "YIELD" "UNION" "AND" "NOT" "DISTINCT" "ASC" "DESC") t)
    (datalog-mode "Datalog / Souffle" "//" "datalog" ("\\.dl\\'" "\\.souffle\\'")
     ("decl" "type" "input" "output" "include" "plan" "printsize" "number_of_rows" "override" "magic" "inline" "brie" "btree" "eqrel"))
    (plsql-mode "PL/SQL" "--" "plsql" ("\\.pks\\'" "\\.pkb\\'" "\\.pls\\'")
     ("PROCEDURE" "FUNCTION" "PACKAGE" "BODY" "BEGIN" "END" "EXCEPTION" "WHEN" "THEN" "LOOP" "WHILE" "FOR" "IF" "ELSIF" "ELSE" "RETURN" "PRAGMA" "CURSOR" "TYPE" "REPLACE" "CREATE" "IS" "AS" "INTO" "FROM" "SELECT" "WHERE" "INSERT" "UPDATE" "DELETE") t)
    (gremlin-mode "Gremlin" "//" "gremlin" ("\\.gremlin\\'")
     ("addV" "addE" "property" "hasLabel" "outE" "inE" "bothE" "values" "valueMap" "repeat" "until" "emit" "choose" "coalesce" "constant" "fold" "unfold" "limit" "order" "by" "where" "select" "as" "group" "count"))
    (xpath-mode "XPath" "(:" "xpath" ("\\.xpath\\'")
     ("child" "parent" "ancestor" "descendant" "following" "preceding" "self" "attribute" "namespace" "node" "text" "comment" "contains" "starts-with" "substring" "normalize-space" "count" "sum" "boolean" "number" "string"))
    (xquery-mode "XQuery" "(:" "xquery" ("\\.xq\\'" "\\.xquery\\'" "\\.xqy\\'")
     ("xquery" "version" "module" "namespace" "declare" "function" "import" "let" "return" "for" "where" "order" "group" "by" "some" "every" "satisfies" "typeswitch" "if" "then" "else"))
    (kql-mode "KQL" "//" "kql" ("\\.kql\\'")
     ("let" "where" "project" "summarize" "extend" "join" "union" "take" "top" "by" "on" "parse" "render" "order" "distinct" "count" "make_series" "mv-expand" "bag_unpack") t)
    (dax-mode "DAX" "//" "dax" ("\\.dax\\'")
     ("CALCULATE" "FILTER" "ALL" "ALLEXCEPT" "SUM" "SUMX" "AVERAGE" "COUNT" "COUNTROWS" "DISTINCT" "RELATED" "RELATEDTABLE" "VAR" "RETURN" "EVALUATE" "DEFINE" "MEASURE" "COLUMN" "TABLE" "DATATABLE" "SELECTEDVALUE" "IF" "SWITCH" "AND" "OR" "NOT") t)
    (jsonpath-mode "JsonPath" "//" "jsonpath" ("\\.jsonpath\\'")
     ("length" "min" "max" "avg" "sum" "keys"))
    (jmespath-mode "JMESPath" "//" "jmespath" ("\\.jmespath\\'")
     ("length" "sort" "contains" "starts_with" "ends_with" "keys" "values" "abs" "avg" "max" "min" "not_null" "to_string" "to_number" "type" "map" "filter"))
    (cel-mode "CEL" "//" "cel" ("\\.cel\\'")
     ("true" "false" "null" "in" "has"))
    (gams-mode "GAMS" "*" "gams" ("\\.gms\\'")
     ("SETS" "PARAMETERS" "VARIABLES" "EQUATIONS" "MODEL" "SOLVE" "USING" "DISPLAY" "SCALAR" "TABLE" "POSITIVE" "FREE" "BINARY" "INTEGER") t)
    (minizinc-mode "MiniZinc" "%" "minizinc" ("\\.mzn\\'" "\\.dzn\\'")
     ("constraint" "solve" "satisfy" "minimize" "maximize" "output" "predicate" "function" "include" "array" "set" "of" "var" "par" "int" "float" "bool" "string" "enum" "ann" "if" "then" "else" "endif" "where"))
    (ampl-mode "AMPL" "#" "ampl" ("\\.run\\'")
     ("set" "param" "var" "minimize" "maximize" "subject" "s.t" "solve" "display" "data" "model" "option" "let" "if" "then" "else" "sum" "forall") t)
    (ion-mode "Ion" "//" "ion" ("\\.ion\\'")
     ("null" "bool" "int" "float" "decimal" "timestamp" "string" "symbol" "blob" "clob" "struct" "list" "sexp" "true" "false"))
    (avro-schema-mode "Avro schema" "//" "avro-schema" ("\\.avsc\\'")
     ("type" "name" "namespace" "fields" "symbols" "items" "values" "aliases" "doc" "default" "order" "record" "enum" "array" "map" "fixed" "null" "boolean" "string" "int" "long" "float" "double" "bytes"))
    (stata-mode "Stata" "*" "stata" ("\\.do\\'" "\\.ado\\'")
     ("regress" "logit" "probit" "summarize" "tabulate" "generate" "replace" "drop" "keep" "merge" "append" "sort" "by" "if" "in" "local" "global" "foreach" "forvalues" "quietly" "capture" "program" "end" "use" "save" "clear"))
    (wolfram-mode "Wolfram / Mathematica" "(*" "wolfram" ("\\.wl\\'" "\\.wls\\'" "\\.nb\\'")
     ("Module" "Block" "With" "If" "Which" "Switch" "While" "For" "Do" "Table" "Plot" "BeginPackage" "EndPackage" "Begin" "End" "Needs" "SetDelayed" "Function" "Null" "True" "False"))
    (maple-mode "Maple" "#" "maple" ("\\.mpl\\'")
     ("proc" "end" "local" "global" "option" "if" "then" "elif" "else" "fi" "for" "from" "to" "by" "do" "od" "while" "return" "module" "export" "use"))
    (stan-mode "Stan" "//" "stan" ("\\.stan\\'")
     ("data" "transformed" "parameters" "model" "generated" "quantities" "functions" "real" "int" "vector" "matrix" "array" "if" "else" "for" "while" "return" "target"))
    (scilab-mode "Scilab" "//" "scilab" ("\\.sci\\'" "\\.sce\\'")
     ("function" "endfunction" "if" "then" "else" "elseif" "end" "for" "while" "select" "case" "break" "continue" "return" "global"))
    (applescript-mode "AppleScript" "--" "applescript" ("\\.applescript\\'" "\\.scpt\\'")
     ("tell" "end" "if" "then" "else" "repeat" "with" "return" "set" "to" "of" "on" "error" "try" "script" "property" "global" "local") t)
    (csh-mode "csh / tcsh" "#" "csh" ("\\.csh\\'" "\\.tcsh\\'")
     ("if" "then" "else" "endif" "foreach" "end" "while" "switch" "case" "breaksw" "default" "set" "setenv" "unset" "alias" "unalias" "goto"))
    (ksh-mode "Ksh" "#" "ksh" ("\\.ksh\\'")
     ("function" "if" "then" "else" "elif" "fi" "for" "while" "do" "done" "case" "esac" "select" "in" "return" "typeset" "integer"))
    (xonsh-mode "Xonsh" "#" "xonsh" ("\\.xsh\\'")
     ("def" "if" "else" "elif" "for" "while" "return" "import" "from" "and" "or" "not" "lambda" "with" "as" "class"))
    (wikitext-mode "Wikitext / MediaWiki" "<!--" "wikitext" ("\\.wiki\\'" "\\.mediawiki\\'")
     ("if" "ifeq" "switch" "default" "for" "foreach"))
    (textile-mode "Textile" "<!--" "textile" ("\\.textile\\'")
     ("table" "notextile"))
    (bbcode-mode "BBCode" "<!--" "bbcode" ("\\.bbcode\\'")
     ("quote" "code" "url" "img" "list" "size" "color"))
    (pod-mode "POD" "=" "pod" ("\\.pod\\'")
     ("head1" "head2" "item" "over" "back" "cut" "pod" "begin" "end" "for" "encoding"))
    (rtf-mode "RTF" "\\*" "rtf" ("\\.rtf\\'")
     ("rtf" "fonttbl" "colortbl" "stylesheet" "ansi" "ansicpg" "pard" "par" "deff"))
    (ps-mode "PostScript" "%" "postscript" ("\\.ps\\'" "\\.eps\\'")
     ("def" "begin" "end"))
    (nroff-mode "Groff / Troff / man" "\\\"" "groff" ("\\.man\\'" "\\.roff\\'" "\\.troff\\'" "\\.groff\\'" "\\.1\\'" "\\.2\\'" "\\.3\\'" "\\.4\\'" "\\.5\\'" "\\.6\\'" "\\.7\\'" "\\.8\\'" "\\.9\\'")
     ("TH" "SH" "SS" "PP" "TP" "BR" "EE" "EX"))
    (sgml-mode "SGML" "<!--" "sgml" ("\\.sgml\\'" "\\.sgm\\'")
     ("DOCTYPE" "ELEMENT" "ATTLIST" "ENTITY"))
    (delphi-mode "Delphi / Object Pascal" "//" "delphi" ("\\.dpr\\'" "\\.dpk\\'" "\\.dfm\\'")
     ("unit" "interface" "implementation" "uses" "begin" "end" "procedure" "function" "property"))
    (gml-mode "GML / GameMaker" "//" "gml" ("\\.gml\\'")
     ("var" "if" "else" "for" "while" "repeat" "function" "with" "return"))
    (llvm-mode "LLVM IR" ";" "llvm" ("\\.ll\\'")
     ("define" "declare" "ret" "br"))
    (mlir-mode "MLIR" "//" "mlir" ("\\.mlir\\'")
     ("func" "module" "return"))
    (ptx-mode "PTX" "//" "ptx" ("\\.ptx\\'")
     ("visible" "entry" "func" "reg" "param" "ld" "st" "add" "mul" "mov" "ret" "bra" "call" "bar" "shared" "global" "local"))
    (bluespec-mode "Bluespec" "//" "bluespec" ("\\.bsv\\'")
     ("package" "endpackage" "module" "endmodule" "interface" "endinterface" "method" "endmethod" "rule" "endrule" "import" "export" "if" "else" "typedef" "struct" "enum" "deriving"))
    (maude-mode "Maude" "---" "maude" ("\\.maude\\'")
     ("mod" "endm" "fmod" "endfm" "sort" "subsort" "op" "eq" "ceq" "rl" "crl" "mb" "cmb" "var" "if" "protecting" "including" "extending"))
    (acl2-mode "ACL2" ";" "acl2" ("\\.acl2\\'")
     ("defun" "defthm" "defmacro" "implies" "let" "let*" "if" "cond" "declare" "include-book" "in-package" "local" "progn" "mutual-recursion"))
    (pvs-mode "PVS" "%" "pvs" ("\\.pvs\\'")
     ("BEGIN" "END" "THEORY" "LEMMA" "THEOREM" "FORMULA" "JUDGEMENT" "IMPORTING" "TYPE" "VAR" "LAMBDA" "FORALL" "EXISTS" "IF" "THEN" "ELSE" "ENDIF") t)
    (why3-mode "Why3" "(*" "why3" ("\\.mlw\\'" "\\.why\\'")
     ("module" "end" "theory" "use" "clone" "goal" "lemma" "axiom" "predicate" "function" "let" "val" "if" "then" "else" "match" "with" "forall" "exists"))
    (chuck-mode "ChucK" "//" "chuck" ("\\.ck\\'")
     ("fun" "function" "if" "else" "while" "for" "until" "repeat" "return" "spork" "class" "extends" "public" "static" "new" "true" "false" "null" "now" "dur" "time"))
    (csound-mode "Csound" ";" "csound" ("\\.csd\\'" "\\.orc\\'" "\\.sco\\'")
     ("instr" "endin" "opcode" "endop" "ksmps" "sr" "nchnls" "if" "then" "else" "elseif" "endif" "while" "do" "od" "goto" "igoto"))
    (puredata-mode "Pure Data" "//" "puredata" ("\\.pd\\'")
     ("obj" "msg" "floatatom" "symbolatom" "connect" "canvas" "restore" "text" "declare"))
    (tact-mode "Tact" "//" "tact" ("\\.tact\\'")
     ("contract" "trait" "struct" "message" "fun" "get" "native" "extends" "with" "if" "else" "while" "repeat" "return" "let" "const" "import" "primitive" "asm" "true" "false" "null"))
    (func-mode "FunC" ";;" "func" ("\\.fc\\'" "\\.func\\'")
     ("forall" "return" "if" "else" "while" "do" "repeat" "until" "try" "catch" "impure" "inline" "global" "asm" "int" "cell" "slice" "builder" "cont" "tuple"))
    (fift-mode "Fift" "//" "fift" ("\\.fif\\'")
     ("abort" "execute" "include" "if" "ifnot" "dup" "drop" "swap" "over" "rot" "tuck" "nip"))
    (plutus-mode "Plutus" "--" "plutus" ("\\.plutus\\'")
     ("module" "where" "import" "data" "newtype" "type" "let" "in" "if" "then" "else" "case" "of" "do" "validator" "mintingPolicy" "True" "False"))
    (marlowe-mode "Marlowe" "--" "marlowe" ("\\.marlowe\\'")
     ("When" "Case" "If" "Let" "Assert" "Close" "Pay" "Choice" "Notify" "Deposit" "Party" "Token" "Constant" "Timeout" "Action"))
    (ligo-mode "LIGO" "//" "ligo" ("\\.ligo\\'" "\\.mligo\\'" "\\.religo\\'" "\\.jsligo\\'")
     ("function" "let" "in" "if" "then" "else" "match" "with" "type" "module" "struct" "begin" "end" "const" "switch" "case" "return" "import" "export"))
    (reach-mode "Reach" "//" "reach" ("\\.rsh\\'")
     ("export" "import" "reach" "const" "function" "if" "else" "return" "while" "forall" "exists" "commit" "publish" "pay" "wait" "timeout" "only" "each" "Participant" "ParticipantClass"))
    (zokrates-mode "ZoKrates" "//" "zokrates" ("\\.zok\\'")
     ("def" "return" "if" "else" "for" "in" "field" "bool" "u8" "u16" "u32" "u64" "struct" "import" "from" "private" "public" "assert"))
    (intercal-mode "INTERCAL" "NOTE" "intercal" ("\\.intercal\\'" "\\.ic\\'")
     ("PLEASE" "NOTE" "FORGET" "RESUME" "IGNORE" "REMEMBER" "ABSTAIN" "REINSTATE" "NEXT" "DO" "COME" "FROM" "WRITE" "READ" "SUBROUTINE") t)
    (chef-mode "Chef" "#" "chef" ("\\.chef\\'")
     ("Ingredients" "Method" "Cooking" "Take" "from" "refrigerator" "Put" "into" "mixing" "bowl" "Serve" "with" "Refrigerate" "Liquefy" "Pour" "until" "set" "aside" "Fold" "Stir" "Combine" "Clean") t)
    (shakespeare-mode "Shakespeare" "#" "shakespeare" ("\\.spl\\'")
     ("Act" "Scene" "Enter" "Exit" "Exeunt" "Speak" "Open" "heart" "Listen" "Remember" "Recall" "You" "thou" "thee"))
    ))
(dolist (row my/new-lang-rows)
  (let ((mode (nth 0 row))
        (title (nth 1 row))
        (comment (nth 2 row))
        (ft (nth 3 row))
        (pats (nth 4 row))
        (words (nth 5 row))
        (fold (nth 6 row)))
    (my/own-mode mode title comment words fold)
    (when (fboundp mode)
      (dolist (pat pats)
        (unless (assoc pat auto-mode-alist)
          (my/add-ft pat mode)))
      (unless (member (cons ft mode) my/extra-ft-cells)
        (push (cons ft mode) my/extra-ft-cells))
      (unless (assoc title my/extra-help-rows)
        (push (list title ft
                    (mapconcat (lambda (e) (concat "." (substring e 2 -2)))
                               pats " "))
              my/extra-help-rows)))))

(add-hook 'wxml-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("wx:[A-Za-z0-9-]+" . font-lock-keyword-face))
             'prepend)))
(my/own-mode 'hocon-mode "HOCON" "#"
             '("include" "required" "optional" "substitute" "url" "file" "classpath")
             nil)
(my/own-mode 'tsql-mode "T-SQL" "--"
             '("SELECT" "FROM" "WHERE" "JOIN" "INNER" "LEFT" "RIGHT" "FULL" "OUTER"
               "GROUP" "ORDER" "HAVING" "INSERT" "UPDATE" "DELETE" "CREATE" "ALTER"
               "DROP" "TABLE" "INTO" "VALUES" "BEGIN" "END" "DECLARE" "NVARCHAR"
               "VARCHAR" "PROCEDURE" "FUNCTION" "RETURNS" "TOP" "OFFSET" "FETCH"
               "MERGE" "OUTPUT" "TRY" "CATCH" "THROW" "EXEC" "EXECUTE" "GO")
             t)
(my/own-mode 'plpgsql-mode "PLpgSQL" "--"
             '("FUNCTION" "PROCEDURE" "RETURNS" "RETURN" "BEGIN" "END" "DECLARE"
               "LANGUAGE" "plpgsql" "IF" "ELSIF" "ELSE" "LOOP" "WHILE" "FOR"
               "EXCEPTION" "WHEN" "RAISE" "NOTICE" "QUERY" "PERFORM" "SELECT"
               "INTO" "FROM" "WHERE")
             t)
(my/own-mode 'modelica-mode "Modelica" "//"
             '("model" "equation" "algorithm" "end" "connector" "class" "package"
               "within" "extends" "import" "outer" "inner" "replaceable" "redeclare"
               "if" "then" "else" "when" "for" "loop" "function" "record" "type"
               "block" "protected" "public")
             nil)
(my/own-mode 'idl-lang-mode "IDL" ";"
             '("pro" "function" "end" "if" "then" "else" "endif" "for" "do"
               "endfor" "while" "endwhile" "repeat" "until" "case" "of" "endcase"
               "begin" "compile_opt" "return" "goto" "common" "forward_function")
             t)
(my/own-mode 'spss-mode "SPSS" "*"
             '("GET" "FILE" "DATA" "LIST" "BEGIN" "END" "FREQUENCIES" "VARIABLES"
               "CROSSTABS" "REGRESSION" "DESCRIPTIVES" "MEANS" "COMPUTE" "RECODE"
               "IF" "DO" "REPEAT" "SELECT" "EXECUTE" "SAVE" "OUTFILE" "TITLE")
             t)
(my/own-mode 'mdx-mode "MDX" "--"
             '("SELECT" "FROM" "WHERE" "COLUMNS" "ROWS" "PAGES" "WITH" "MEMBER"
               "SET" "AS" "NON" "EMPTY" "AXIS" "ON" "CELL" "PROPERTIES")
             t)

(defun my/register-lang (mode title ft ext)
  (when (fboundp mode)
    (unless (member (cons ft mode) my/extra-ft-cells)
      (push (cons ft mode) my/extra-ft-cells))
    (unless (assoc title my/extra-help-rows)
      (push (list title ft ext) my/extra-help-rows))))

(my/register-lang 'hocon-mode "HOCON" "hocon" ".conf 内容像 HOCON 时，否则按 INI")
(my/register-lang 'tsql-mode "T-SQL" "tsql" ".sql 内容像 T-SQL 时")
(my/register-lang 'plpgsql-mode "PLpgSQL" "plpgsql" ".sql 函数体像 PLpgSQL 时")
(my/register-lang 'modelica-mode "Modelica" "modelica" ".mo 内容像 Modelica 时，否则 Motoko")
(my/register-lang 'idl-lang-mode "IDL" "idl" ".pro 内容像 IDL 时，否则 Prolog")
(my/register-lang 'spss-mode "SPSS" "spss" ".sps 内容像 SPSS 时，否则 Scheme")
(my/register-lang 'mdx-mode "MDX" "mdx" ".mdx 内容像 MDX 查询时，否则 Markdown")
(my/register-lang 'helm-lang-mode "Helm template" "helm" "templates 目录里带 {{ }} 的 .yaml")
(my/register-lang 'kustomize-mode "Kustomize" "kustomize" "kustomization.yaml")
(my/register-lang 'ansible-mode "Ansible" "ansible" ".yml .yaml 含 tasks 或 hosts 时")
(my/register-lang 'expect-mode "Expect" "expect" ".exp")
(my/register-lang 'xslt-mode "XSLT" "xslt" ".xsl .xslt")
(my/register-lang 'befunge-mode "Befunge" "befunge" ".befunge")
(my/register-lang 'whitespace-lang-mode "Whitespace" "whitespace" ".ws")
(my/register-lang 'piet-mode "Piet" "piet" ".piet")
(my/register-lang 'malbolge-mode "Malbolge" "malbolge" ".mb .mal")
(my/register-lang 'messagepack-mode "MessagePack" "messagepack" ".msgpack .mpk")
(my/register-lang 'cbor-mode "CBOR" "cbor" ".cbor")
(my/register-lang 'bson-mode "BSON" "bson" ".bson")
(my/register-lang 'unlambda-mode "Unlambda" "unlambda" ".unl")

;;; Russian, Japanese, and the English edition of EPL.
(my/own-mode 'bsl-mode "1C BSL" "//"
             '("Procedure" "Function" "EndProcedure" "EndFunction" "If" "Then" "Else" "ElsIf" "EndIf" "For" "Each" "While" "EndDo" "Return" "Var" "Export" "Val" "Try" "Except" "EndTry" "Raise" "Break" "Continue" "New" "And" "Or" "Not" "True" "False" "Процедура" "Функция" "КонецПроцедуры" "КонецФункции" "Если" "Тогда" "Иначе" "ИначеЕсли" "КонецЕсли" "Пока" "Для" "Каждого" "Цикл" "КонецЦикла" "Возврат" "Перем" "Экспорт" "Знач" "Попытка" "Исключение" "КонецПопытки" "ВызватьИсключение" "Прервать" "Продолжить" "Новый" "ИЛИ" "НЕ")
             t)
(my/add-ft "\\.bsl\\'" 'bsl-mode)
(my/add-ft "\\.os\\'" 'bsl-mode)
(my/register-lang 'bsl-mode "1C / BSL" "bsl" ".bsl .os")

(my/own-mode 'kumir-mode "Kumir" "|"
             '("алг" "нач" "кон" "если" "то" "иначе" "все" "выбор" "при" "нц" "кц" "пока" "для" "от" "до" "шаг" "утв" "дано" "надо" "или" "не" "да" "нет" "выход" "ввод" "вывод" "использовать" "арг" "рез" "аргрез" "кц_при" "цел" "вещ" "лог" "сим" "лит")
             t)
(my/add-ft "\\.kum\\'" 'kumir-mode)
(my/register-lang 'kumir-mode "КуМир" "kumir" ".kum")

(my/own-mode 'rapira-mode "Rapira" "(*"
             '("ПРОЦ" "ФУНК" "КНЦ" "РЕЗ" "ИМЕНА" "ЕСЛИ" "ТО" "ИНАЧЕ" "ВСЕ" "ПОКА" "ЦИКЛ" "ДЛЯ" "ОТ" "ДО" "ШАГ" "ВЫВОД" "ВВОД" "ВЫХОД" "ВЫБОР" "ПРИ" "ИЛИ" "НЕ" "ДА" "НЕТ")
             t)
(my/add-ft "\\.rap\\'" 'rapira-mode)
(my/register-lang 'rapira-mode "Рапира" "rapira" ".rap")

(my/own-mode 'yasny-mode "Yasny" "//"
             '("модуль" "импорт" "экспорт" "пусть" "константа" "функция" "вернуть" "если" "тогда" "иначе" "конец" "пока" "для" "класс" "истина" "ложь" "или" "не" "новый" "тип")
             nil)
(my/add-ft "\\.ya\\'" 'yasny-mode)
(my/register-lang 'yasny-mode "Ясный" "yasny" ".ya")

(my/own-mode 'rusl-mode "RuSL" "//"
             '("изм" "пока" "если" "иначе" "конец" "это" "Правда" "ложь" "число" "ввести" "вывести" "функция" "вернуть" "текст")
             nil)
(my/add-ft "\\.rusl\\'" 'rusl-mode)
(my/register-lang 'rusl-mode "RuSL" "rusl" ".rusl")

(my/own-mode 'nadesiko-mode "Nadesiko" "#"
             '("もし" "ならば" "違えば" "でなければ" "ここまで" "関数" "戻す" "戻る" "変数" "定数" "表示" "それ" "かつ" "または" "エラー監視")
             nil)
(my/add-ft "\\.nako3\\'" 'nadesiko-mode)
(my/register-lang 'nadesiko-mode "なでしこ3" "nadesiko" ".nako3")

(unless (fboundp 'epl-mode)
  (define-derived-mode epl-mode prog-mode "EPL"
    "English edition of EPL."
    (setq-local comment-start "'"
                comment-end ""
                indent-tabs-mode nil
                tab-width 4
                my/rich-filled t)
    (modify-syntax-entry ?\" ".")
    (modify-syntax-entry ?\' ".")
    (modify-syntax-entry ?\` ".")
    (setq-local font-lock-defaults
                (list (my/rich-keywords
                       "'"
                       '("If" "Else" "EndIf" "IfTrue"
                         "Switch" "EndSwitch" "While" "Wend"
                         "For" "Next" "Do" "Loop" "Return"
                         "Break" "Continue" "Exit" "And" "Or"
                         "Not" "True" "False")
                       '(("\\.\\(?:Version\\|Support\\|Assembly\\|Sub\\|Parameter\\|Local\\|Global\\|DLLCommand\\|Constant\\|DataType\\|Member\\|Default\\)\\_>"
                          0 'font-lock-keyword-face keep))
                       t)
                      nil t))))

(my/add-ft "\\.epl\\'" 'epl-mode)
(defun my/mode-e ()
  "English EPL when the file uses its headers, otherwise Eiffel."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (if (and (fboundp 'epl-mode)
                 (string-match-p "^\\s-*\\.\\(?:Version\\|Support\\|Assembly\\|Sub\\)\\_>" chunk))
            (epl-mode)
          (eiffel-mode)))
    (error (when (fboundp 'eiffel-mode) (eiffel-mode)))))
(add-to-list 'auto-mode-alist '("\\.e\\'" . my/mode-e))
(my/register-lang 'epl-mode "易语言英文版" "epl" ".epl  以及以 .Version 或 .Sub 开头的 .e")

(defun my/set-help (title ext)
  (setq my/extra-help-rows
        (mapcar (lambda (row)
                  (if (equal (car row) title)
                      (list (car row) (nth 1 row) ext)
                    row))
                my/extra-help-rows)))
(my/set-help "Ninja" ".ninja build.ninja")
(my/set-help "Flex / Lex" ".l  .ll 像 Flex 时")
(my/set-help "Wolfram / Mathematica" ".wl .wls .nb  内容像 Mathematica 的 .m")
(my/set-help "AMPL" ".run  内容像 AMPL 的 .mod 与 .dat")
(my/set-help "PL/SQL" ".pks .pkb .pls  以及内容像 PL/SQL 的 .sql")
(my/set-help "Groff / Troff / man" ".man .roff .troff .1 .2 .3 .4 .5 .6 .7 .8 .9")
(my/add-ft "build\\.ninja\\'" 'ninja-mode)
(my/add-ft "\\.befunge\\'" 'befunge-mode)
(my/add-ft "\\.ws\\'" 'whitespace-lang-mode)
(my/add-ft "\\.piet\\'" 'piet-mode)
(my/add-ft "\\.mb\\'" 'malbolge-mode)
(my/add-ft "\\.mal\\'" 'malbolge-mode)
(my/add-ft "\\.msgpack\\'" 'messagepack-mode)
(my/add-ft "\\.mpk\\'" 'messagepack-mode)
(my/add-ft "\\.cbor\\'" 'cbor-mode)
(my/add-ft "\\.bson\\'" 'bson-mode)
(my/add-ft "\\.unl\\'" 'unlambda-mode)
(my/add-ft "\\.xsl\\'" 'xslt-mode)
(my/add-ft "\\.xslt\\'" 'xslt-mode)

(defun my/retake (sample mode control)
  "Point SAMPLE's private pattern at MODE.
A pattern that also matches CONTROL stays."
  (when (fboundp mode)
    (let ((pat nil))
      (dolist (cell auto-mode-alist)
        (when (and (null pat)
                   (stringp (car-safe cell))
                   (string-match-p (car cell) sample)
                   (not (string-match-p (car cell) control)))
          (setq pat (car cell))))
      (if pat
          (progn (my/drop-ft pat) (my/add-ft pat mode))
        (let ((ext (file-name-extension sample)))
          (when ext
            (my/add-ft (concat "\\." ext "\\'") mode)))))))
(my/retake "/tmp/a.pp" 'puppet-mode "/tmp/a.pas")
(my/retake "/tmp/a.dpr" 'delphi-mode "/tmp/a.pas")
(my/retake "/tmp/a.njk" 'nunjucks-mode "/tmp/a.html")
(my/retake "/tmp/a.exp" 'expect-mode "/tmp/a.tcl")
(my/retake "/tmp/a.pod" 'pod-mode "/tmp/a.pl")

(defun my/mode-conf ()
  "Nginx, Apache, then HOCON, otherwise the INI conf mode."
  (condition-case _
      (let ((name (file-name-nondirectory (or buffer-file-name "")))
            (chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'nginx-mode)
               (string-match-p "\\`nginx\\.conf\\'" name))
          (nginx-mode))
         ((and (fboundp 'apache-mode)
               (string-match-p "\\`httpd\\.conf\\'" name))
          (apache-mode))
         ((and (fboundp 'hocon-mode)
               (string-match-p "{\\|\\${" chunk)
               (string-match-p "^\\s-*\\(?:include\\s-+\\|\\w+\\s-*{\\)" chunk))
          (hocon-mode))
         (t (conf-mode))))
    (error (conf-mode))))

(defun my/mode-sql ()
  "Pick T-SQL, PL/SQL or PLpgSQL when the text says so."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'plpgsql-mode)
               (string-match-p "language\\s-+plpgsql\\|RETURN\\s-+QUERY\\|RAISE\\s-+EXCEPTION" chunk))
          (plpgsql-mode))
         ((and (fboundp 'plsql-mode)
               (string-match-p "\\_<DBMS_\\|\\_<PRAGMA\\_>\\|\\_<PACKAGE\\s-+BODY\\_>" chunk))
          (plsql-mode))
         ((and (fboundp 'tsql-mode)
               (string-match-p "^\\s-*GO\\s-*$\\|\\_<NVARCHAR\\_>\\|\\_<sp_executesql\\_>\\|\\[dbo\\]" chunk))
          (tsql-mode))
         (t (sql-mode))))
    (error (sql-mode))))

(defun my/mode-yaml ()
  "Kustomize, Helm and Ansible keep their own modes. Other YAML stays YAML."
  (condition-case _
      (let ((chunk (my/head-chunk))
            (name (or buffer-file-name "")))
        (cond
         ((and (fboundp 'kustomize-mode)
               (string-match-p "kustomization\\.ya?ml\\'" name))
          (kustomize-mode))
         ((and (fboundp 'helm-lang-mode)
               (string-match-p "/templates/" name)
               (string-match-p "{{" chunk))
          (helm-lang-mode))
         ((and (fboundp 'ansible-mode)
               (string-match-p "^\\s-*\\(?:-\\s-*\\)?\\(?:tasks\\|hosts\\|roles\\):" chunk))
          (ansible-mode))
         (t (my/call-ts 'yaml-mode 'yaml-ts-mode 'yaml))))
    (error (when (fboundp 'yaml-mode) (yaml-mode)))))

(defun my/mode-ll ()
  "Flex when .ll looks like Lex, otherwise LLVM IR."
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'flex-mode)
               (string-match-p "^\\s-*%{\\|^\\s-*%%" chunk)
               (not (string-match-p "datalayout\\|\\_<define\\_>" chunk)))
          (flex-mode))
         ((fboundp 'llvm-mode) (llvm-mode))
         (t nil)))
    (error (when (fboundp 'llvm-mode) (llvm-mode)))))

(defun my/mode-mo ()
  "Modelica when .mo looks like Modelica, otherwise Motoko."
  (condition-case _
      (let ((chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'modelica-mode)
               (string-match-p "^\\s-*\\(?:model\\|equation\\|connector\\|within\\)\\_>" chunk))
          (modelica-mode))
         ((fboundp 'motoko-mode) (motoko-mode))
         (t nil)))
    (error (when (fboundp 'motoko-mode) (motoko-mode)))))

(defun my/mode-mod ()
  "go.mod stays a Go module file.
AMPL when .mod looks like a model, otherwise Modula-2."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'go-dot-mod-mode)
               (string-match-p "go\\.mod\\'" (or buffer-file-name "")))
          (my/call-ts 'go-dot-mod-mode 'go-mod-ts-mode 'gomod))
         ((and (fboundp 'ampl-mode)
               (string-match-p "^\\s-*\\(?:param\\|minimize\\|maximize\\|subject\\s+to\\|s\\.t\\.\\)\\_>" chunk)
               (not (string-match-p "^\\s-*MODULE\\_>" chunk)))
          (ampl-mode))
         ((fboundp 'm2-mode) (m2-mode))
         (t nil)))
    (error (when (fboundp 'm2-mode) (m2-mode)))))

(defun my/mode-pro ()
  "IDL when .pro looks like IDL, otherwise Prolog."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (cond
         ((string-match-p ":-" chunk)
          (prolog-mode))
         ((and (fboundp 'idl-lang-mode)
               (string-match-p "^\\s-*\\(?:pro\\|function\\|compile_opt\\)\\_>" chunk))
          (idl-lang-mode))
         (t (prolog-mode))))
    (error (prolog-mode))))

(defun my/mode-sps ()
  "SPSS when .sps looks like SPSS syntax, otherwise Scheme."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'spss-mode)
               (string-match-p "^\\s-*\\(?:GET\\s-+FILE\\|DATA\\s-+LIST\\|FREQUENCIES\\|BEGIN\\s-+DATA\\)\\>" chunk))
          (spss-mode))
         (t (scheme-mode))))
    (error (scheme-mode))))

(defun my/mode-mdx ()
  "MDX queries use MDX. Markdown documents stay Markdown."
  (condition-case _
      (let ((case-fold-search t)
            (chunk (my/head-chunk)))
        (cond
         ((and (fboundp 'mdx-mode)
               (string-match-p "ON\\s-+\\(?:COLUMNS\\|ROWS\\|PAGES\\)" chunk))
          (mdx-mode))
         (t (markdown-mode))))
    (error (markdown-mode))))

(add-to-list 'auto-mode-alist '("\\.conf\\'" . my/mode-conf))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . my/mode-sql))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . my/mode-yaml))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . my/mode-yaml))
(add-to-list 'auto-mode-alist '("\\.ll\\'" . my/mode-ll))
(add-to-list 'auto-mode-alist '("\\.mo\\'" . my/mode-mo))
(add-to-list 'auto-mode-alist '("\\.mod\\'" . my/mode-mod))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . my/mode-pro))
(add-to-list 'auto-mode-alist '("\\.sps\\'" . my/mode-sps))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . my/mode-mdx))

(defun my/systemc-p ()
  (and buffer-file-name
       (string-match-p "\\.\\(?:h\\|hh\\|hpp\\|hxx\\|cpp\\|cc\\|cxx\\)\\'" buffer-file-name)
       (condition-case _
           (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (re-search-forward "#include[[:space:]]*[<\"]systemc" 12000 t)))
         (error nil))))
(setq magic-mode-alist (assq-delete-all 'matlab-is-matlab-file magic-mode-alist))
(add-to-list 'magic-mode-alist '(my/systemc-p . c++-mode))

(defun my/ampl-dat-p ()
  (and buffer-file-name
       (string-match-p "\\.dat\\'" buffer-file-name)
       (condition-case _
           (save-excursion
             (save-restriction
               (widen)
               (goto-char (point-min))
               (let ((case-fold-search t))
                 (re-search-forward "^[[:space:]]*\\(?:param\\|set\\)\\_>" 4000 t))))
         (error nil))))
(add-to-list 'magic-mode-alist '(my/ampl-dat-p . ampl-mode))

(dolist (cell '(("csh" . csh-mode) ("tcsh" . csh-mode) ("ksh" . ksh-mode)
                ("xonsh" . xonsh-mode) ("expect" . expect-mode)))
  (when (fboundp (cdr cell))
    (add-to-list 'interpreter-mode-alist cell)))

(dolist (n '("2" "3" "4" "5" "6" "7" "8" "9"))
  (unless (assoc-default (concat "/tmp/page." n) auto-mode-alist #'string-match)
    (my/add-ft (concat "\\." n "\\'") 'nroff-mode)))

(provide 'langs)
