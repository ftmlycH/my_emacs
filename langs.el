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

(defun my/define-lite (mode name comment words)
  "Highlight-only major mode. Skip if MODE already exists."
  (unless (fboundp mode)
    (ignore-errors
      (eval `(define-derived-mode ,mode prog-mode ,name
               ,(format "Highlight-only %s." name)
               (setq-local comment-start ,comment
                           indent-tabs-mode nil
                           tab-width 4)
               (setq-local font-lock-defaults
                           ',(list (list (cons (concat "\\_<" (regexp-opt words t) "\\_>")
                                               'font-lock-keyword-face)))))
            t))))

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
  (setq-local font-lock-defaults
              `(((,(concat "\\_<" (regexp-opt my/erabasic-keywords t) "\\_>")
                  . font-lock-keyword-face)
                 ("^#[A-Za-z_]+" . font-lock-preprocessor-face)
                 ("^@[A-Za-z0-9_]+" . font-lock-function-name-face)
                 ("^\\$[A-Za-z0-9_]+" . font-lock-constant-face)
                 (";.*$" . font-lock-comment-face)
                 ("\"\\(?:\\\\.\\|[^\"]\\)*\"" . font-lock-string-face)))))

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
                  "data.frame" "list" "c" "ggplot" "aes" "mutate" "filter"
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
         ((string-match-p "^\\s-*\\(%\\|function\\>\\)" chunk)
          (octave-mode))
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
           ("\\.pug\\'" . pug-mode) ("\\.jade\\'" . pug-mode)
           ("\\.haml\\'" . haml-mode) ("\\.slim\\'" . slim-mode)
           ("\\.mdx\\'" . markdown-mode)
           ;; Script
           ("\\.mjs\\'" . js-mode) ("\\.cjs\\'" . js-mode)
           ("\\.ts\\'" . typescript-mode)
           ("\\.coffee\\'" . coffee-mode) ("\\.cson\\'" . coffee-mode)
           ("\\.lua\\'" . lua-mode) ("\\.luau\\'" . lua-mode)
           ("\\.moon\\'" . moonscript-mode)
           ("\\.ps1\\'" . powershell-mode) ("\\.psm1\\'" . powershell-mode)
           ("\\.psd1\\'" . powershell-mode)
           ("\\.fish\\'" . fish-mode) ("\\.nu\\'" . sh-mode)
           ;; Data
           ("\\.jsonc\\'" . json-mode) ("\\.json5\\'" . json-mode)
           ("\\.jsonnet\\'" . jsonnet-mode) ("\\.libsonnet\\'" . jsonnet-mode)
           ("\\.jsonl\\'" . json-mode) ("\\.geojson\\'" . json-mode)
           ("\\.webmanifest\\'" . json-mode) ("\\.har\\'" . json-mode)
           ("\\.gltf\\'" . json-mode) ("\\.arb\\'" . json-mode)
           ("\\.toml\\'" . conf-toml-mode) ("Pipfile\\'" . conf-toml-mode)
           ("\\.ini\\'" . conf-mode) ("\\.cfg\\'" . conf-mode)
           ("\\.conf\\'" . conf-mode) ("\\.editorconfig\\'" . conf-mode)
           ("\\.env\\'" . dotenv-mode) ("\\.env\\..+\\'" . dotenv-mode)
           ("\\.properties\\'" . conf-javaprop-mode)
           ("\\.csv\\'" . csv-mode) ("\\.tsv\\'" . csv-mode)
           ("\\.graphql\\'" . graphql-mode) ("\\.gql\\'" . graphql-mode)
           ("\\.proto\\'" . protobuf-mode) ("\\.thrift\\'" . thrift-mode)
           ("\\.avdl\\'" . avro-idl-mode) ("\\.capnp\\'" . conf-mode)
           ("\\.fbs\\'" . conf-mode)
           ;; Game / shader / GPU
           ("\\.gd\\'" . gdscript-mode) ("\\.gdshader\\'" . glsl-mode)
           ("\\.rpy\\'" . python-mode) ("\\.rpym\\'" . python-mode)
           ("\\.glsl\\'" . glsl-mode) ("\\.vert\\'" . glsl-mode)
           ("\\.frag\\'" . glsl-mode) ("\\.geom\\'" . glsl-mode)
           ("\\.comp\\'" . glsl-mode) ("\\.tesc\\'" . glsl-mode)
           ("\\.tese\\'" . glsl-mode)
           ("\\.hlsl\\'" . hlsl-mode) ("\\.hlsli\\'" . hlsl-mode)
           ("\\.fx\\'" . hlsl-mode) ("\\.fxh\\'" . hlsl-mode)
           ("\\.usf\\'" . hlsl-mode) ("\\.ush\\'" . hlsl-mode)
           ("\\.wgsl\\'" . wgsl-mode) ("\\.metal\\'" . objc-mode)
           ("\\.cu\\'" . cuda-mode) ("\\.cuh\\'" . cuda-mode)
           ("\\.cl\\'" . c-mode) ("\\.slang\\'" . c-mode)
           ("\\.spv\\'" . fundamental-mode)
           ("\\.tscn\\'" . conf-mode) ("\\.tres\\'" . conf-mode)
           ("\\.import\\'" . conf-mode) ("\\.godot\\'" . conf-mode)
           ("\\.unity\\'" . yaml-mode) ("\\.prefab\\'" . yaml-mode)
           ("\\.meta\\'" . yaml-mode) ("\\.asset\\'" . yaml-mode)
           ("\\.controller\\'" . yaml-mode) ("\\.anim\\'" . yaml-mode)
           ("\\.vmat\\'" . conf-mode) ("\\.vmf\\'" . conf-mode)
           ("\\.qml\\'" . qml-mode) ("\\.qmltypes\\'" . qml-mode)
           ("\\.gml\\'" . js-mode) ("\\.ink\\'" . ink-mode)
           ("\\.p8\\'" . lua-mode) ("\\.scad\\'" . scad-mode)
           ;; Systems / low-level
           ("\\.rs\\'" . rust-mode) ("\\.go\\'" . go-mode)
           ("go\\.mod\\'" . go-mode) ("go\\.sum\\'" . conf-mode)
           ("\\.zig\\'" . zig-mode) ("\\.zon\\'" . zig-mode)
           ("\\.nim\\'" . nim-mode) ("\\.nims\\'" . nim-mode)
           ("\\.v\\'" . verilog-mode) ("\\.sv\\'" . verilog-mode)
           ("\\.svh\\'" . verilog-mode) ("\\.vhdl?\\'" . vhdl-mode)
           ("\\.odin\\'" . odin-mode) ("\\.d\\'" . d-mode)
           ("\\.di\\'" . d-mode) ("\\.cr\\'" . crystal-mode)
           ("\\.jai\\'" . c-mode)
           ("\\.ll\\'" . conf-mode) ("\\.mlir\\'" . conf-mode)
           ("\\.td\\'" . conf-mode) ("\\.s\\'" . asm-mode)
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
           ("\\.rkt\\'" . scheme-mode) ("\\.rktd\\'" . scheme-mode)
           ("\\.rktl\\'" . scheme-mode) ("\\.scrbl\\'" . scheme-mode)
           ("\\.sld\\'" . scheme-mode) ("\\.sls\\'" . scheme-mode)
           ("\\.sps\\'" . scheme-mode)
           ("\\.clj\\'" . clojure-mode) ("\\.cljs\\'" . clojure-mode)
           ("\\.cljc\\'" . clojure-mode) ("\\.cljd\\'" . clojure-mode)
           ("\\.edn\\'" . clojure-mode) ("\\.bb\\'" . clojure-mode)
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
           ("\\.ads\\'" . ada-mode) ("\\.pas\\'" . pascal-mode)
           ("\\.pp\\'" . pascal-mode) ("\\.dpr\\'" . pascal-mode)
           ("\\.cob\\'" . cobol-mode) ("\\.cbl\\'" . cobol-mode)
           ("\\.forth\\'" . forth-mode) ("\\.fth\\'" . forth-mode)
           ;; Infra / devops
           ("Dockerfile\\'" . dockerfile-mode)
           ("Containerfile\\'" . dockerfile-mode)
           ("docker-compose.*\\.ya?ml\\'" . yaml-mode)
           ("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)
           ("meson\\.build\\'" . meson-mode) ("meson\\.options\\'" . meson-mode)
           ("\\.nix\\'" . nix-mode) ("flake\\.lock\\'" . json-mode)
           ("\\.tf\\'" . terraform-mode) ("\\.tfvars\\'" . terraform-mode)
           ("\\.hcl\\'" . hcl-mode) ("\\.nomad\\'" . hcl-mode)
           ("\\.cue\\'" . cue-mode) ("\\.dhall\\'" . dhall-mode)
           ("Justfile\\'" . just-mode) ("justfile\\'" . just-mode)
           ("\\.just\\'" . just-mode) ("BUILD\\'" . python-mode)
           ("BUILD\\.bazel\\'" . python-mode) ("WORKSPACE\\'" . python-mode)
           ("\\.bzl\\'" . python-mode) ("\\.star\\'" . python-mode)
           ("Tiltfile\\'" . python-mode)
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
           ("\\.gitattributes\\'" . gitattributes-mode)
           ("\\.gitconfig\\'" . gitconfig-mode)
           ("\\.gitmodules\\'" . gitconfig-mode)
           ("\\.vim\\'" . vimrc-mode) ("\\.vimrc\\'" . vimrc-mode)
           ("\\.nvimrc\\'" . vimrc-mode)
           ;; Styles
           ("\\.scss\\'" . scss-mode) ("\\.sass\\'" . sass-mode)
           ("\\.less\\'" . less-css-mode) ("\\.styl\\'" . stylus-mode)
           ("\\.css\\'" . css-mode)
           ;; Other
           ("\\.vala\\'" . vala-mode) ("\\.vapi\\'" . vala-mode)
           ("\\.sql\\'" . sql-mode) ("\\.pgsql\\'" . sql-mode)
           ("\\.mysql\\'" . sql-mode)
           ("\\.kdl\\'" . kdl-lite-mode) ("\\.ron\\'" . rust-mode)
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
           ("\\.bas\\'" . basic-mode) ("\\.vbs\\'" . visual-basic-mode)
           ("\\.apex\\'" . java-mode) ("\\.trigger\\'" . java-mode)
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
           ("\\.matlab\\'" . octave-mode)))
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

;;; Fallbacks when a dedicated package is missing.
(unless (fboundp 'hlsl-mode)
  (dolist (p '("\\.hlsl\\'" "\\.hlsli\\'" "\\.fx\\'" "\\.fxh\\'" "\\.usf\\'" "\\.ush\\'"))
    (add-to-list 'auto-mode-alist (cons p 'c-mode))))
(unless (fboundp 'glsl-mode)
  (dolist (p '("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'" "\\.comp\\'"
               "\\.gdshader\\'"))
    (add-to-list 'auto-mode-alist (cons p 'c-mode))))
(unless (fboundp 'nasm-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(asm\\|nasm\\)\\'" . asm-mode)))
(unless (fboundp 'wat-mode)
  (add-to-list 'auto-mode-alist '("\\.wat\\'" . lisp-data-mode)))
(unless (fboundp 'csharp-mode)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode)))
(unless (fboundp 'powershell-mode)
  (add-to-list 'auto-mode-alist '("\\.ps1\\'" . sh-mode)))
(unless (fboundp 'svelte-mode)
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode)))
(unless (fboundp 'dotenv-mode)
  (add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode)))
(unless (fboundp 'gitignore-mode)
  (add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode)))
(unless (fboundp 'terraform-mode)
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . conf-mode)))
(unless (fboundp 'nix-mode)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . conf-mode)))
(unless (fboundp 'fsharp-mode)
  (add-to-list 'auto-mode-alist '("\\.fsx?\\'" . csharp-mode)))
(unless (fboundp 'qml-mode)
  (add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode)))
(unless (fboundp 'solidity-mode)
  (add-to-list 'auto-mode-alist '("\\.sol\\'" . js-mode)))
(unless (fboundp 'prisma-mode)
  (add-to-list 'auto-mode-alist '("\\.prisma\\'" . conf-mode)))
(unless (fboundp 'cython-mode)
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode)))
(unless (fboundp 'julia-mode)
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . python-mode)))
(unless (fboundp 'odin-mode)
  (add-to-list 'auto-mode-alist '("\\.odin\\'" . c-mode)))
(unless (fboundp 'gleam-mode)
  (add-to-list 'auto-mode-alist '("\\.gleam\\'" . elixir-mode)))
(unless (fboundp 'haskell-literate-mode)
  (dolist (p '("\\.lhs\\'" "\\.lhs-boot\\'"))
    (add-to-list 'auto-mode-alist (cons p 'haskell-mode))))
(unless (fboundp 'haskell-c2hs-mode)
  (add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-mode)))
(unless (fboundp 'tuareg-interface-mode)
  (add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode)))
(unless (fboundp 'reason-mode)
  (add-to-list 'auto-mode-alist '("\\.rei?\\'" . tuareg-mode)))
(unless (fboundp 'rescript-mode)
  (add-to-list 'auto-mode-alist '("\\.resi?\\'" . tuareg-mode)))
(unless (fboundp 'idris-mode)
  (if (fboundp 'idris2-mode)
      (add-to-list 'auto-mode-alist '("\\.l?idr\\'" . idris2-mode))
    (add-to-list 'auto-mode-alist '("\\.l?idr\\'" . haskell-mode))))
(unless (fboundp 'agda2-mode)
  (add-to-list 'auto-mode-alist '("\\.l?agda\\'" . haskell-mode)))
(unless (fboundp 'lean4-mode)
  (add-to-list 'auto-mode-alist
               (cons "\\.lean\\'" (if (fboundp 'lean-mode) 'lean-mode 'haskell-mode))))
(unless (fboundp 'fstar-mode)
  (add-to-list 'auto-mode-alist '("\\.fsti?\\'" . haskell-mode)))
(unless (fboundp 'lfe-mode)
  (add-to-list 'auto-mode-alist '("\\.lfe\\'" . erlang-mode)))
(unless (fboundp 'dune-mode)
  (add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)dune\\(?:-project\\|-workspace\\)?\\'" . conf-mode)))
(unless (fboundp 'feature-mode)
  (add-to-list 'auto-mode-alist '("\\.feature\\'" . gherkin-mode)))
(unless (fboundp 'sed-mode)
  (add-to-list 'auto-mode-alist '("\\.sed\\'" . sh-mode)))
(unless (fboundp 'visual-basic-mode)
  (add-to-list 'auto-mode-alist '("\\.vbs\\'" . basic-mode)))
(unless (fboundp 'gemini-mode)
  (add-to-list 'auto-mode-alist '("\\.gmi\\'" . gemtext-mode)))

(dolist (cell '(("luajit" . lua-mode) ("lua" . lua-mode)
                ("python3" . python-mode) ("python" . python-mode)
                ("pypy" . python-mode) ("node" . js-mode)
                ("nodejs" . js-mode) ("ts-node" . typescript-mode)
                ("bash" . sh-mode) ("zsh" . sh-mode) ("fish" . fish-mode)
                ("ruby" . ruby-mode) ("perl" . perl-mode)
                ("php" . php-mode) ("gjs" . js-mode)
                ("racket" . scheme-mode) ("guile" . scheme-mode)
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

;; Functional modes: highlight + indent only. Never spawn GHCi/format/typecheck.
(setq haskell-mode-hook '(haskell-indentation-mode)
      haskell-tags-on-save nil
      haskell-stylish-on-save nil
      haskell-indentation-layout-offset 4
      haskell-indentation-left-offset 4
      haskell-indentation-starter-offset 4
      haskell-indentation-where-pre-offset 4
      haskell-indentation-where-post-offset 4
      dhall-use-header-line nil
      dhall-format-at-save nil
      elm-format-on-save nil
      elm-tags-on-save nil
      elm-sort-imports-on-save nil
      chapel-format-on-save nil
      chapel-use-ctags nil
      zig-format-on-save nil
      matlab-show-mlint-warnings nil
      idris-mode-hook '(turn-on-idris-simple-indent)
      fstar-enabled-modules '(font-lock indentation comments literate))
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
        haskell-process-suggest-hoogle-imports nil)
  (remove-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (remove-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode))
(with-eval-after-load 'idris-mode
  (setq idris-mode-hook '(turn-on-idris-simple-indent))
  (remove-hook 'idris-mode-hook #'turn-on-eldoc-mode))
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
(with-eval-after-load 'kdl-mode
  (advice-add 'kdl-install-tree-sitter-grammar :override #'ignore))
(with-eval-after-load 'chapel-mode
  (setq chapel-format-on-save nil
        chapel-use-ctags nil)
  (advice-add 'chapel-load-tags :override #'ignore)
  (advice-add 'chapel-build-tags :override #'ignore)
  (advice-add 'chapel-format-buffer :override #'ignore))

(with-eval-after-load 'vim-ex
  (dolist (cell '(("renpy" . python-mode) ("gdscript" . gdscript-mode)
                  ("glsl" . glsl-mode) ("hlsl" . c-mode) ("wgsl" . wgsl-mode)
                  ("asm" . nasm-mode) ("wasm" . lisp-data-mode)
                  ("zig" . zig-mode) ("nim" . nim-mode) ("cs" . csharp-mode)
                  ("cuda" . cuda-mode) ("lua" . lua-mode) ("vue" . web-mode)
                  ("svelte" . web-mode) ("nix" . nix-mode)
                  ("terraform" . terraform-mode) ("tf" . terraform-mode)
                  ("solidity" . solidity-mode) ("julia" . julia-mode)
                  ("elm" . elm-mode) ("ocaml" . tuareg-mode)
                  ("fsharp" . fsharp-mode) ("sml" . sml-mode)
                  ("purescript" . purescript-mode) ("idris" . idris-mode)
                  ("agda" . agda2-mode) ("lean" . lean4-mode)
                  ("fstar" . fstar-mode) ("reason" . reason-mode)
                  ("rescript" . rescript-mode) ("scheme" . scheme-mode)
                  ("racket" . scheme-mode) ("lisp" . lisp-mode)
                  ("clojure" . clojure-mode) ("lfe" . lfe-mode)
                  ("prolog" . prolog-mode) ("dune" . dune-mode)
                  ("crystal" . crystal-mode)
                  ("erlang" . erlang-mode) ("elixir" . elixir-mode)
                  ("haskell" . haskell-mode) ("kotlin" . kotlin-mode)
                  ("dart" . dart-mode) ("swift" . swift-mode)
                  ("powershell" . powershell-mode) ("proto" . protobuf-mode)
                  ("graphql" . graphql-mode) ("fish" . fish-mode)
                  ("qml" . qml-mode) ("haxe" . haxe-mode)
                  ("fennel" . fennel-mode) ("d" . d-mode)
                  ("r" . my-r-mode) ("matlab" . octave-mode)
                  ("octave" . octave-mode) ("ahk" . ahk-mode)
                  ("autoit" . autoit-mode) ("raku" . raku-mode)
                  ("rego" . rego-mode) ("gherkin" . gherkin-mode)
                  ("pkl" . pkl-mode) ("prql" . prql-mode)
                  ("promql" . promql-mode) ("bicep" . bicep-mode)
                  ("ballerina" . ballerina-mode) ("chapel" . chapel-lite-mode)
                  ("hare" . hare-mode) ("holyc" . holyc-mode)
                  ("vb" . basic-mode) ("shaderlab" . shaderlab-mode)
                  ("vex" . vex-mode) ("rsl" . rsl-mode) ("agal" . agal-mode)
                  ("erabasic" . erabasic-mode) ("era" . erabasic-mode)
                  ("coq" . coq-mode) ("awk" . awk-mode) ("sed" . sh-mode)
                  ("kdl" . kdl-lite-mode)))
    (add-to-list 'my/vim-ft-alist cell)))

(provide 'langs)
