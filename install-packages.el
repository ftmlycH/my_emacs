;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; One-shot MELPA install for Debian. Run:
;;;   emacs -nw --batch -l install-packages.el
;;; Does not run on every startup.

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-check-signature nil)
(require 'package)
(package-initialize)
(message "Refreshing package archives…")
(package-refresh-contents)

(defvar my/linux-packages
  '(evil evil-surround evil-commentary
    web-mode markdown-mode yaml-mode json-mode typescript-mode
    lua-mode rust-mode go-mode php-mode
    gdscript-mode glsl-mode wgsl-mode nasm-mode cuda-mode
    zig-mode nim-mode kotlin-mode dart-mode swift-mode groovy-mode scala-mode
    haskell-mode purescript-mode idris-mode fstar-mode elm-mode tuareg sml-mode
    elixir-mode erlang clojure-mode fennel-mode haxe-mode
    nix-mode terraform-mode hcl-mode dockerfile-mode cmake-mode meson-mode
    graphql-mode protobuf-mode powershell fish-mode systemd dotenv-mode
    csv-mode qml-mode vala-mode jsonnet-mode just-mode puppet-mode dhall-mode
    jq-mode cython-mode git-modes adoc-mode gnuplot-mode forth-mode
    mermaid-mode plantuml-mode graphviz-dot-mode scad-mode janet-mode hy-mode
    cue-mode ahk-mode feature-mode raku-mode kdl-mode pkl-mode chapel-mode
    hare-mode basic-mode gemini-mode pov-mode sed-mode matlab-mode ess
    rainbow-delimiters hl-todo highlight-numbers diff-hl corfu
    coffee-mode pug-mode haml-mode slim-mode d-mode crystal-mode
    solidity-mode svelte-mode vimrc-mode nginx-mode apache-mode
    thrift reason-mode rescript-mode lfe-mode ink-mode unison-mode
    csharp-mode bison-mode)
  "Language + editor packages matching the Windows config.")

(let ((ok 0) (skip 0) (fail 0))
  (dolist (pkg my/linux-packages)
    (cond
     ((package-installed-p pkg)
      (setq skip (1+ skip))
      (message "skip %s" pkg))
     ((ignore-errors (package-install pkg) t)
      (setq ok (1+ ok))
      (message "ok   %s" pkg))
     (t
      (setq fail (1+ fail))
      (message "FAIL %s" pkg))))
  (package-quickstart-refresh)
  (message "done installed=%d skipped=%d failed=%d" ok skip fail))
(kill-emacs 0)
