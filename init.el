;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; 原生 Emacs。只加语法高亮、按语言缩进。GUI 用不透明底 #181818；TTY 仍透明。

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8
      locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t t))

;;; --- indent follows the language ---
(setq-default indent-tabs-mode nil
              tab-width 4
              standard-indent 4)
(setq tab-always-indent t
      c-basic-offset 4
      c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r"))
      python-indent-offset 4
      python-indent-guess-indent-offset nil
      js-indent-level 4
      js-jsx-indent-level 4
      css-indent-offset 4
      sh-basic-offset 4
      sh-indentation 4
      ruby-indent-level 4
      perl-indent-level 4
      nxml-child-indent 4
      sgml-basic-offset 4
      lua-indent-level 4)
(electric-indent-mode 1)

(defun my/indent-setup ()
  "Spaces, width 4. Makefiles keep tabs."
  (unless (or (minibufferp) (string-prefix-p " " (buffer-name)))
    (setq-local indent-tabs-mode (derived-mode-p 'makefile-mode)
                tab-width 4
                standard-indent 4)))
(add-hook 'after-change-major-mode-hook #'my/indent-setup)

;;; --- highlighting stays on the visible text ---
(setq font-lock-maximum-decoration t
      font-lock-support-mode 'jit-lock-mode
      jit-lock-defer-time nil
      jit-lock-stealth-time nil
      jit-lock-stealth-verbose nil
      jit-lock-chunk-size 2000
      jit-lock-context-time 0.4
      fast-but-imprecise-scrolling nil
      redisplay-skip-fontification-on-input nil
      bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)
(global-font-lock-mode 1)
(show-paren-mode 1)
(setq-default display-line-numbers-type t
              display-line-numbers-width 4
              display-line-numbers-grow-only t)
(global-display-line-numbers-mode 1)

;;; --- the window moves one line at a time ---
(setq scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount
      '(1 ((shift) . hscroll)
          ((meta) . nil)
          ((control meta) . global-text-scale)
          ((control) . text-scale))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)
(xterm-mouse-mode 1)
(when (fboundp 'global-so-long-mode)
  (setq so-long-threshold 400000)
  (global-so-long-mode 1))

;;; --- language modes, loaded when a file opens ---
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-check-signature nil
      package-quickstart t)
(package-initialize)
(add-to-list 'load-path (directory-file-name user-emacs-directory))
(require 'langs)
;; Level 4 is the finest tree-sitter layer: calls, variables,
;; properties, operators and brackets. Level 3 leaves those bare.
(when (fboundp 'treesit-available-p)
  (setq-default treesit-font-lock-level 4))

;;; --- Cursor colors; GUI opaque #181818, TTY transparent ---
(defun my/set-fg (face color)
  (when (facep face)
    (set-face-attribute face nil :foreground color)))

(defconst my/gui-bg (if (display-graphic-p) "#181818" 'unspecified))

(set-face-attribute 'default nil :foreground "#FFFFFF" :background my/gui-bg)
(set-face-attribute 'cursor nil :background "#F0F0F0")
(set-cursor-color "#F0F0F0")
(set-face-attribute 'region nil :background "#404040" :foreground 'unspecified)
(set-face-attribute 'mode-line nil
                    :foreground "#F0F0F0" :background my/gui-bg
                    :inverse-video nil :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#9A9A9A" :background my/gui-bg
                    :inverse-video nil :box nil)
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "#F0F0F0" :background my/gui-bg
                    :inverse-video nil :weight 'normal)
(set-face-attribute 'fringe nil :background my/gui-bg)
(set-face-attribute 'minibuffer-prompt nil :foreground "#F0F0F0" :background my/gui-bg)
(set-face-attribute 'show-paren-match nil :background "#404040" :foreground 'unspecified :weight 'bold)
(set-face-attribute 'show-paren-mismatch nil :foreground "#FF0000" :background 'unspecified)
(set-face-attribute 'line-number nil
                    :foreground "#9A9A9A" :background my/gui-bg
                    :italic nil :weight 'normal)
(set-face-attribute 'line-number-current-line nil
                    :foreground "#FFFFFF" :background my/gui-bg
                    :italic nil :weight 'normal)
(set-face-attribute 'error nil :foreground "#E34671")
(set-face-attribute 'warning nil :foreground "#F1B467")
(set-face-attribute 'success nil :foreground "#779977")
(set-face-attribute 'link nil :foreground "#81A1C1" :underline t)
(set-face-attribute 'link-visited nil :foreground "#87A6C4" :underline t)

(defun my/tty-clear-bg (&optional frame)
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame) (not (display-graphic-p frame)))
    (when (fboundp 'xterm--maybe-update-default-face)
      (remove-hook 'after-make-frame-functions #'xterm--maybe-update-default-face))
    (ignore-errors
      (set-terminal-parameter (frame-terminal frame) 'xterm--background-color nil))
    (dolist (face '(default fringe minibuffer-prompt
                     line-number line-number-current-line
                     mode-line mode-line-inactive mode-line-buffer-id))
      (when (facep face)
        (set-face-attribute face frame :background 'unspecified)
        (set-face-attribute face nil :background 'unspecified)))))
(add-hook 'after-make-frame-functions #'my/tty-clear-bg)
(add-hook 'window-setup-hook #'my/tty-clear-bg)
(add-hook 'emacs-startup-hook #'my/tty-clear-bg)
(add-hook 'after-init-hook #'my/tty-clear-bg t)

(set-face-attribute 'font-lock-comment-face nil :foreground "#335566" :italic t)
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#335566" :italic t)
(set-face-attribute 'font-lock-doc-face nil :foreground "#335566" :italic t)
(dolist (pair '((font-lock-string-face . "#AA4477")
                (font-lock-keyword-face . "#779977")
                (font-lock-builtin-face . "#779977")
                (font-lock-function-name-face . "#BB9955")
                (font-lock-variable-name-face . "#7799CC")
                (font-lock-type-face . "#77BBDD")
                (font-lock-constant-face . "#FF8899")
                (font-lock-preprocessor-face . "#77DD77")
                (font-lock-warning-face . "#E34671")
                (font-lock-negation-char-face . "#FFFFFF")
                (font-lock-number-face . "#FFDD88")
                (font-lock-escape-face . "#FFFFFF")
                (font-lock-property-name-face . "#7777AA")
                (font-lock-property-use-face . "#7777AA")
                (font-lock-operator-face . "#FFFFFF")
                (font-lock-bracket-face . "#FFFFFF")
                (font-lock-delimiter-face . "#FFFFFF")
                (font-lock-punctuation-face . "#FFFFFF")
                (font-lock-misc-punctuation-face . "#FFFFFF")
                (font-lock-regexp-face . "#FFFFFF")
                (font-lock-regexp-grouping-backslash . "#FFFFFF")
                (font-lock-regexp-grouping-construct . "#779977")
                (font-lock-doc-markup-face . "#77DD77")
                (c-annotation-face . "#BB9955")))
  (my/set-fg (car pair) (cdr pair)))
(with-eval-after-load 'cc-mode
  (my/set-fg 'c-annotation-face "#BB9955"))
(with-eval-after-load 'php-mode
  (dolist (face '(php-operator php-assignment-op php-comparison-op
                  php-logical-op php-arithmetic-op php-inc-dec-op
                  php-string-op php-object-op php-pipe-op
                  php-paamayim-nekudotayim))
    (my/set-fg face "#FFFFFF"))
  (dolist (face '(php-function-call-traditional php-method-call-traditional
                  php-static-method-call-traditional))
    (my/set-fg face "#BB9955"))
  (my/set-fg 'php-variable-sigil "#7799CC")
  (my/set-fg 'php-property-name "#7777AA"))
(with-eval-after-load 'web-mode
  (my/set-fg 'web-mode-html-tag-face "#77BBDD")
  (my/set-fg 'web-mode-html-attr-name-face "#7777AA")
  (my/set-fg 'web-mode-html-attr-value-face "#AA4477")
  (my/set-fg 'web-mode-keyword-face "#779977")
  (my/set-fg 'web-mode-string-face "#AA4477")
  (my/set-fg 'web-mode-function-name-face "#BB9955")
  (my/set-fg 'web-mode-type-face "#77BBDD")
  (set-face-attribute 'web-mode-comment-face nil :foreground "#335566" :italic t)
  (my/set-fg 'web-mode-constant-face "#FF8899")
  (my/set-fg 'web-mode-variable-name-face "#7799CC"))
(with-eval-after-load 'markdown-mode
  (my/set-fg 'markdown-header-face "#88C0D0")
  (my/set-fg 'markdown-inline-code-face "#AA4477")
  (my/set-fg 'markdown-code-face "#AA4477")
  (my/set-fg 'markdown-link-face "#779977")
  (my/set-fg 'markdown-url-face "#7777AA")
  (my/set-fg 'markdown-bold-face "#f8c762")
  (set-face-attribute 'markdown-italic-face nil :foreground "#779977" :italic t))

(defun my/lisp-rainbow ()
  "Give each nested Lisp delimiter its own color."
  (when (and (fboundp 'rainbow-delimiters-mode)
             (< (buffer-size) 150000))
    (rainbow-delimiters-mode 1)))
(dolist (hook '(lisp-data-mode-hook
                scheme-mode-hook
                racket-mode-hook
                clojure-mode-hook
                fennel-mode-hook
                hy-mode-hook
                lfe-mode-hook))
  (add-hook hook #'my/lisp-rainbow))

(defconst my/lisp-rainbow-colors
  '("#FF5FAF" "#FF8700" "#FFD700" "#5FFF5F" "#5F87FF" "#5F5FFF" "#AF5FFF"))

(defun my/lisp-rainbow-apply ()
  (setq rainbow-delimiters-max-face-count (length my/lisp-rainbow-colors)
        rainbow-delimiters-outermost-only-face-count 0)
  (let ((n 0))
    (dolist (color my/lisp-rainbow-colors)
      (setq n (1+ n))
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" n))))
        (when (facep face)
          (set-face-attribute face nil
                              :foreground color
                              :inherit 'rainbow-delimiters-base-face)))))
  (dolist (face '(rainbow-delimiters-base-error-face
                  rainbow-delimiters-unmatched-face
                  rainbow-delimiters-mismatched-face
                  show-paren-mismatch))
    (when (facep face)
      (set-face-attribute face nil :foreground "#FF0000"))))

(when (require 'rainbow-delimiters nil t)
  (my/lisp-rainbow-apply)
  (with-eval-after-load 'rainbow-delimiters
    (my/lisp-rainbow-apply)))

(require 'chinese-help)
