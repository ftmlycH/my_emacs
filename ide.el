;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Quiet IDE. Nothing starts by itself. :Lsp / keys only.

(setq xref-prompt-for-identifier nil
      imenu-auto-rescan nil
      compilation-scroll-output 'first-error
      eglot-autoshutdown t
      eglot-sync-connect nil
      eglot-connect-timeout 8
      eglot-report-progress nil
      eglot-extend-to-xref t
      eglot-ignored-server-capabilities
      '(:inlayHintProvider :documentOnTypeFormattingProvider)
      flymake-fringe-indicator-position nil
      eldoc-idle-delay 1000
      eldoc-echo-area-use-multiline-p nil
      uniquify-buffer-name-style 'forward
      auto-revert-check-vc-info nil)
(when (boundp 'eglot-events-buffer-config)
  (setq eglot-events-buffer-config '(:size 0 :format full)))

(setq save-place-forget-unreadable-files t
      save-place-limit 400)
(ignore-errors (save-place-mode 1))

(defvar my/lsp-servers
  '((python-mode "pylsp" "pyright" "jedi-language-server")
    (python-ts-mode "pylsp" "pyright")
    (rust-mode "rust-analyzer") (rust-ts-mode "rust-analyzer")
    (go-mode "gopls") (go-ts-mode "gopls")
    (c-mode "clangd") (c-ts-mode "clangd")
    (c++-mode "clangd") (c++-ts-mode "clangd")
    (js-mode "typescript-language-server") (js-ts-mode "typescript-language-server")
    (typescript-mode "typescript-language-server")
    (typescript-ts-mode "typescript-language-server")
    (lua-mode "lua-language-server") (lua-ts-mode "lua-language-server")
    (php-mode "intelephense" "phpactor")
    (json-mode "vscode-json-language-server")
    (yaml-mode "yaml-language-server")
    (cmake-mode "cmake-language-server")
    (sh-mode "bash-language-server") (bash-ts-mode "bash-language-server")
    (haskell-mode "haskell-language-server-wrapper" "haskell-language-server")
    (haskell-literate-mode "haskell-language-server-wrapper")
    (elixir-mode "elixir-ls" "lexical")
    (erlang-mode "erlang_ls")
    (clojure-mode "clojure-lsp")
    (tuareg-mode "ocamllsp" "ocaml-language-server")
    (elm-mode "elm-language-server")
    (fsharp-mode "fsautocomplete")
    (purescript-mode "purescript-language-server")
    (nix-mode "nil" "nixd")
    (scala-mode "metals")
    (idris-mode "idris2-lsp")
    (lean4-mode "lean"))
  "Mode → language-server executables.")

(defun my/lsp-command ()
  (catch 'found
    (dolist (exe (cdr (assq major-mode my/lsp-servers)))
      (when (executable-find exe)
        (throw 'found exe)))))

(defun my/lsp-start ()
  (interactive)
  (unless (my/lsp-command)
    (user-error "当前语言没有可用的语言服务器"))
  (require 'eglot)
  (call-interactively #'eglot))

(defun my/doc-at-point ()
  (interactive)
  (cond
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p) (fboundp 'eldoc-doc-buffer))
    (eldoc-doc-buffer t))
   ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
    (describe-symbol (or (symbol-at-point) (user-error "光标处没有符号"))))
   ((fboundp 'evil-lookup)
    (call-interactively #'evil-lookup))
   (t (call-interactively #'describe-symbol))))

(defun my/format-buffer ()
  (interactive)
  (when (> (buffer-size) 400000)
    (user-error "文件太大，不格式化"))
  (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (eglot-format-buffer)
    (indent-region (point-min) (point-max))))

(defun my/rename ()
  (interactive)
  (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (user-error "语言服务未启动，先执行 :Lsp"))
  (call-interactively #'eglot-rename))

(defun my/code-action ()
  (interactive)
  (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (user-error "语言服务未启动，先执行 :Lsp"))
  (call-interactively #'eglot-code-actions))

(defun my/term ()
  (interactive)
  (ansi-term (or (getenv "SHELL") "/bin/bash")))

(defun my/recent ()
  (interactive)
  (require 'recentf)
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200)
  (recentf-mode 1)
  (recentf-open-files))

(defun my/eglot-quiet ()
  (setq-local eldoc-idle-delay 1000
              eldoc-display-functions nil))
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'my/eglot-quiet))

(defun my/ide-keys ()
  "Features are started from the : command line, not from keys.")

(defun my/install-ide-ex ()
  (when (fboundp 'evil-ex-define-cmd)
    (evil-ex-define-cmd "Lsp" #'my/lsp-start)
    (evil-ex-define-cmd "LspStop" #'my/ex-lsp-stop)
    (evil-ex-define-cmd "Def" #'xref-find-definitions)
    (evil-ex-define-cmd "Ref" #'xref-find-references)
    (evil-ex-define-cmd "Impl" #'eglot-find-implementation)
    (evil-ex-define-cmd "Type" #'eglot-find-typeDefinition)
    (evil-ex-define-cmd "Doc" #'my/doc-at-point)
    (evil-ex-define-cmd "Rename" #'my/rename)
    (evil-ex-define-cmd "Format" #'my/format-buffer)
    (evil-ex-define-cmd "Action" #'my/code-action)
    (evil-ex-define-cmd "Diag" #'flymake-show-buffer-diagnostics)
    (evil-ex-define-cmd "Diags" #'flymake-show-project-diagnostics)
    (evil-ex-define-cmd "Imenu" #'imenu)
    (evil-ex-define-cmd "Tags" #'imenu)
    (evil-ex-define-cmd "Make" #'project-compile)
    (evil-ex-define-cmd "make" #'compile)
    (evil-ex-define-cmd "cn[ext]" #'next-error)
    (evil-ex-define-cmd "cp[revious]" #'previous-error)
    (evil-ex-define-cmd "term" #'my/term)
    (evil-ex-define-cmd "Term" #'my/term)
    (evil-ex-define-cmd "Git" #'vc-dir)
    (evil-ex-define-cmd "Gblame" #'vc-annotate)
    (evil-ex-define-cmd "Gdiff" #'vc-diff)
    (evil-ex-define-cmd "Glog" #'vc-print-log)
    (evil-ex-define-cmd "Project" #'project-switch-project)
    (evil-ex-define-cmd "Recent" #'my/recent)
    (evil-ex-define-cmd "Find" #'project-find-file)
    (evil-ex-define-cmd "Rg" #'project-find-regexp)
    (evil-ex-define-cmd "back" #'xref-go-back)
    (evil-ex-define-cmd "dn" #'flymake-goto-next-error)
    (evil-ex-define-cmd "dp" #'flymake-goto-prev-error)))

(my/ide-keys)
(provide 'ide)
