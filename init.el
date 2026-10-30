;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Terminal Emacs. Windows editing keys, Vim modes and : command line.
;;; One window, one buffer. :help shows the Chinese manual.

;;; --- core ---
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      ring-bell-function 'ignore
      make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      auto-save-default t
      auto-save-timeout 15
      create-lockfiles t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      frame-title-format "%b — Emacs"
      use-short-answers t
      require-final-newline t
      tab-always-indent t
      select-enable-clipboard t
      save-interprogram-paste-before-kill nil
      kill-do-not-save-duplicates t
      warning-minimum-level :error
      native-comp-async-report-warnings-errors 'silent)
(when (file-exists-p custom-file)
  (load custom-file t t))
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
  (ignore-errors (make-directory backup-dir t))
  (ignore-errors (make-directory autosave-dir t))
  (when (file-directory-p backup-dir)
    (setq backup-directory-alist `(("." . ,backup-dir))))
  (when (file-directory-p autosave-dir)
    (setq auto-save-file-name-transforms `((".*" ,autosave-dir md5)))))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8
      locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      xterm-extra-capabilities '(modifyOtherKeys))
(set-coding-system-priority 'utf-8 'utf-8-unix 'utf-8-dos 'gb18030 'gbk)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              truncate-lines nil
              word-wrap t
              cursor-type '(hbar . 2)
              cursor-in-non-selected-windows '(hbar . 1)
              display-line-numbers t
              display-line-numbers-width 3
              left-fringe-width 0
              right-fringe-width 0
              left-margin-width 0
              indicate-empty-lines nil)

(setq scroll-margin 5
      scroll-conservatively 101
      mouse-wheel-progressive-speed nil
      display-line-numbers-type t
      display-line-numbers-width-start nil
      display-line-numbers-grow-only t
      x-stretch-cursor t
      resize-mini-windows nil
      max-mini-window-height 1
      message-truncate-lines t
      echo-keystrokes 0.5
      completion-auto-help nil
      auto-revert-verbose nil
      auto-revert-check-vc-info nil
      show-paren-delay 0.125
      blink-matching-paren nil
      font-lock-maximum-decoration t
      font-lock-support-mode 'jit-lock-mode
      standard-indent 4
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

(setq bidi-inhibit-bpa t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      jit-lock-stealth-time nil
      jit-lock-stealth-nice 0.5
      jit-lock-chunk-size 1500
      auto-revert-use-notify t
      auto-revert-avoid-polling t
      auto-revert-remote-files nil
      confirm-kill-processes nil
      vc-handled-backends '(Git)
      vc-follow-symlinks t
      debug-on-error nil
      debug-on-quit nil
      auto-mode-case-fold nil
      large-file-warning-threshold 80000000)
(setq-default bidi-paragraph-direction 'left-to-right)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(unless (display-graphic-p)
  (global-hl-line-mode -1))
(show-paren-mode 1)
(electric-pair-mode -1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(global-font-lock-mode 1)
(global-eldoc-mode -1)
(when (fboundp 'global-so-long-mode)
  (setq so-long-threshold 400000)
  (global-so-long-mode 1))
(with-eval-after-load 'so-long
  (dolist (m '(highlight-numbers-mode rainbow-delimiters-mode hl-todo-mode))
    (add-to-list 'so-long-minor-modes m)))
(dolist (mode '(fido-mode fido-vertical-mode icomplete-mode which-key-mode tab-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'set-fringe-mode) (set-fringe-mode 0))

;;; xfce4-terminal: mouse, 24-bit color, X11 clipboard via xclip
(unless (display-graphic-p)
  (setq frame-background-mode 'dark
        visible-cursor nil
        tty-menu-open-use-tmm t)
  (xterm-mouse-mode 1)
  (setq mouse-yank-at-point t
        mouse-wheel-progressive-speed nil)
  (setq interprogram-cut-function #'my/xclip-cut
        interprogram-paste-function #'my/xclip-paste))

(defvar my/xclip-proc nil)
(defvar my/xclip-file nil)
(defvar my/xclip-seq 0)
(defvar my/copied-text nil)
(defvar my/copied-at nil)

(defun my/xclip-available-p ()
  (and (not (display-graphic-p))
       (getenv "DISPLAY")
       (executable-find "xclip")))

(defun my/xclip-retire (proc)
  (when (and proc (process-live-p proc))
    (run-with-timer 1.0 nil
                    (lambda (p)
                      (when (process-live-p p)
                        (delete-process p)))
                    proc)))

(defun my/xclip-drop-file (path)
  (when (and path (not (equal path my/xclip-file)))
    (ignore-errors (delete-file path))))

(defun my/xclip-cleanup ()
  (let ((path my/xclip-file))
    (setq my/xclip-file nil)
    (my/xclip-drop-file path)))
(add-hook 'kill-emacs-hook #'my/xclip-cleanup)

(defun my/xclip-cut (text)
  "Hand TEXT to the X clipboard without waiting.
xclip reads a file and keeps running to own the selection.
Waiting for it to exit, or filling its pipe, freezes Emacs."
  (when (stringp text)
    (setq my/copied-text (substring-no-properties text)
          my/copied-at (float-time)))
  (when (and (my/xclip-available-p)
             (stringp text)
             (> (length text) 0)
             (< (length text) 400000)
             (file-directory-p (expand-file-name "autosave/" user-emacs-directory)))
    (ignore-errors
      (let* ((old my/xclip-proc)
             (old-file my/xclip-file)
             (path (expand-file-name
                    (format "autosave/clip-%s-%s" (emacs-pid)
                            (setq my/xclip-seq (1+ my/xclip-seq)))
                    user-emacs-directory))
             (process-connection-type nil)
             (coding-system-for-write 'utf-8-unix))
        (with-temp-file path
          (insert text))
        (setq my/xclip-file path
              my/xclip-proc
              (start-process "xclip" nil "xclip"
                             "-i" "-selection" "clipboard"
                             "-t" "UTF8_STRING" path))
        (set-process-query-on-exit-flag my/xclip-proc nil)
        (when (and old (not (eq old my/xclip-proc)))
          (my/xclip-retire old))
        (when old-file
          (run-with-timer 1.0 nil #'my/xclip-drop-file old-file))))))

(defun my/xclip-paste ()
  "Clipboard text. A copy made here wins for one second, then X is read.
The X read gives up after 0.2s so a dead owner cannot freeze Emacs."
  (cond
   ((and my/copied-text my/copied-at
         (< (- (float-time) my/copied-at) 1.0))
    (unless (and kill-ring (string-equal my/copied-text (car kill-ring)))
      my/copied-text))
   ((and (my/xclip-available-p) (executable-find "timeout"))
    (with-temp-buffer
      (when (eq 0 (ignore-errors
                    (call-process "timeout" nil '(t nil) nil
                                  "0.2" "xclip" "-o" "-selection" "clipboard")))
        (let ((text (buffer-string)))
          (unless (string-empty-p text)
            (setq my/copied-text text
                  my/copied-at 0)
            text)))))))

(defun my/tty-ctrl-event (mod code)
  "Emacs event for a terminal Ctrl sequence, or nil.
MOD is the xterm/kitty modifier parameter. CODE is the Unicode code point."
  (let* ((bits (max 0 (1- mod)))
         (shift (/= 0 (logand bits 1)))
         (alt (/= 0 (logand bits 2)))
         (ctrl (/= 0 (logand bits 4)))
         (base (cond
                ((and (>= code ?a) (<= code ?z)) code)
                ((and (>= code ?A) (<= code ?Z))
                 (setq shift t)
                 (+ code 32))
                ((and (>= code 1) (<= code 26))
                 (setq ctrl t)
                 (+ code 96)))))
    (when (and base ctrl)
      (event-convert-list
       (nconc (and alt (list 'meta))
              (and shift (list 'shift))
              (list 'control base))))))

(defun my/tty-decode-ctrl-keys ()
  "Turn xfce4-terminal Ctrl letters into real C-c, C-v, C-x, and the rest.
VTE sometimes sends these as escape sequences instead of the raw control byte."
  (when (and (not (display-graphic-p)) (keymapp input-decode-map))
    (dolist (mod '(5 6))
      (dotimes (code 128)
        (let ((event (my/tty-ctrl-event mod code)))
          (when event
            (dolist (seq (list (format "\e[27;%d;%d~" mod code)
                               (format "\e[%d;%du" code mod)))
              (let ((key (vconcat seq)))
                (unless (vectorp (lookup-key input-decode-map key))
                  (define-key input-decode-map key (vector event)))))))))))
(defun my/disable-tty-selection-query ()
  "Do not ask the terminal for the clipboard. That wait freezes a key."
  (unless (display-graphic-p)
    (set-terminal-parameter nil 'xterm--get-selection nil)
    (set-terminal-parameter nil 'xterm--set-selection nil)))
(add-hook 'tty-setup-hook #'my/tty-decode-ctrl-keys)
(add-hook 'tty-setup-hook #'my/disable-tty-selection-query)
(add-hook 'emacs-startup-hook #'my/tty-decode-ctrl-keys)
(add-hook 'emacs-startup-hook #'my/disable-tty-selection-query)

;;; --- packages (autoloads only; language modes load when a file opens) ---
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-check-signature nil
      package-quickstart t)
(condition-case err
    (package-initialize)
  (error (message "package-initialize: %s" err)))
(add-to-list 'load-path (directory-file-name user-emacs-directory))
(condition-case err
    (require 'langs)
  (error (message "langs: %s" err)))

(defun my/buffer-setup ()
  "Indent defaults only. Do not re-toggle font-lock (that re-fontifies the whole buffer)."
  (ignore-errors
    (unless (or (minibufferp) (string-prefix-p " " (buffer-name)))
      (setq-local indent-tabs-mode (derived-mode-p 'makefile-mode)
                  tab-width 4
                  standard-indent 4)
      (when (boundp 'evil-shift-width)
        (setq-local evil-shift-width 4)))))
(add-hook 'after-change-major-mode-hook #'my/buffer-setup)

;;; --- evil / netrw ---
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-disable-insert-state-bindings nil
      evil-toggle-key "C-<f20>"
      evil-want-C-u-scroll t
      evil-want-C-d-scroll t
      evil-want-C-i-jump t
      evil-want-C-w-delete nil
      evil-want-C-h-delete nil
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo nil
      evil-undo-system 'undo-redo
      evil-esc-delay 0.01
      evil-want-minibuffer nil
      evil-search-module 'evil-search
      evil-shift-width 4
      evil-move-cursor-back t
      evil-mode-line-format nil
      evil-insert-state-message nil
      evil-visual-state-message nil
      evil-replace-state-message nil
      evil-emacs-state-message nil
      evil-default-state 'normal
      evil-normal-state-cursor '((hbar . 2) "#d6d6dd")
      evil-insert-state-cursor '((bar . 2) "#d6d6dd")
      evil-visual-state-cursor '((hbar . 2) "#87c3ff")
      evil-replace-state-cursor '((hbar . 2) "#E34671")
      evil-operator-state-cursor '((hbar . 2) "#efb080")
      evil-motion-state-cursor '((hbar . 2) "#d6d6dd")
      evil-emacs-state-cursor '((bar . 2) "#d6d6dd"))

(autoload 'dired-jump "dired" nil t)
(autoload 'dired-omit-mode "dired-x" nil t)

;;; --- one window, numbered buffers, Windows editing keys ---

(defvar my/one-window-lock nil)

(defun my/enforce-one-window ()
  "Keep a single editing window. The minibuffer is the command line."
  (unless my/one-window-lock
    (let ((my/one-window-lock t))
      (when (> (length (window-list nil 'no-minibuf)) 1)
        (ignore-errors (delete-other-windows))))))

(defun my/install-one-window ()
  (setq pop-up-windows nil
        pop-up-frames nil
        split-height-threshold nil
        split-width-threshold nil
        switch-to-buffer-obey-display-actions nil
        display-buffer-overriding-action
        '((display-buffer-same-window display-buffer-reuse-window)
          (inhibit-switch-frame . t))
        display-buffer-alist '((".*" . (display-buffer-same-window))))
  (add-hook 'window-configuration-change-hook #'my/enforce-one-window)
  (add-hook 'emacs-startup-hook #'my/enforce-one-window))

(defvar my/buffer-ids (make-hash-table :test 'eq :weakness 'key))
(defvar my/buffer-id-seq 0)

(defun my/real-buffer-p (&optional buf)
  (let ((buf (or buf (current-buffer))))
    (and (buffer-live-p buf)
         (not (minibufferp buf))
         (not (string-prefix-p " " (buffer-name buf))))))

(defun my/buffer-id (&optional buf)
  "Stable number for a user buffer. Internal buffers get nil."
  (let ((buf (or buf (current-buffer))))
    (or (gethash buf my/buffer-ids)
        (and (my/real-buffer-p buf)
             (puthash buf (setq my/buffer-id-seq (1+ my/buffer-id-seq))
                      my/buffer-ids)))))

(defun my/buffer-by-id (n)
  (catch 'found
    (dolist (buf (buffer-list))
      (when (eql (gethash buf my/buffer-ids) n)
        (throw 'found buf)))))

(defun my/real-buffers ()
  (let (bufs)
    (dolist (buf (buffer-list))
      (when (my/real-buffer-p buf)
        (push buf bufs)))
    (sort bufs (lambda (a b) (< (my/buffer-id a) (my/buffer-id b))))))

(defun my/show-buffer-number ()
  (interactive)
  (let ((id (my/buffer-id)))
    (if id
        (message "当前缓冲区编号 %d    %s" id (buffer-name))
      (message "这个缓冲区没有编号"))))

(defun my/switch-to-id (n)
  (let ((buf (my/buffer-by-id n)))
    (unless (and buf (buffer-live-p buf) (my/real-buffer-p buf))
      (user-error "没有编号 %d 的缓冲区" n))
    (switch-to-buffer buf)
    (message "缓冲区 #%d  %s" n (buffer-name buf))))

(defun my/switch-to-name (name)
  (let (hits exact)
    (dolist (buf (my/real-buffers))
      (when (string-match-p (regexp-quote name) (buffer-name buf))
        (push buf hits)
        (when (string= (buffer-name buf) name)
          (setq exact buf))))
    (cond
     (exact (switch-to-buffer exact)
            (message "缓冲区 #%d  %s" (my/buffer-id exact) (buffer-name exact)))
     ((null hits) (user-error "没有匹配「%s」的缓冲区" name))
     ((null (cdr hits))
      (switch-to-buffer (car hits))
      (message "缓冲区 #%d  %s" (my/buffer-id (car hits)) (buffer-name (car hits))))
     (t (user-error "「%s」匹配到多个缓冲区，请用 :ls 看编号后再 :b 编号" name)))))

(defun my/switch-buffer (arg)
  (let ((arg (string-trim (or arg ""))))
    (cond
     ((string-empty-p arg) (my/show-buffer-number))
     ((string-match-p "\\`[0-9]+\\'" arg)
      (my/switch-to-id (string-to-number arg)))
     (t (my/switch-to-name arg)))))

(defun my/doc-buffer-p (buf)
  (member (buffer-name buf) '("*使用说明*" "*缓冲区*")))

(defun my/cycle-buffer (step)
  (let* ((bufs (delq nil (mapcar (lambda (b) (unless (my/doc-buffer-p b) b))
                                 (my/real-buffers))))
         (n (length bufs))
         (idx 0)
         (i 0))
    (when (zerop n) (user-error "没有可切换的缓冲区"))
    (dolist (b bufs)
      (when (eq b (current-buffer)) (setq idx i))
      (setq i (1+ i)))
    (switch-to-buffer (nth (mod (+ idx step) n) bufs))
    (message "缓冲区 #%d  %s" (my/buffer-id) (buffer-name))))

(defun my/close-insert-undo ()
  (when (and (bound-and-true-p evil-mode)
             (memq evil-state '(insert replace))
             (fboundp 'evil-end-undo-step))
    (evil-end-undo-step)))

(defun my/open-insert-undo ()
  (when (and (bound-and-true-p evil-mode)
             (memq evil-state '(insert replace))
             (fboundp 'evil-start-undo-step))
    (evil-start-undo-step)))

(defun my/undo ()
  "Windows Ctrl+Z。停在当前模式里。"
  (interactive)
  (my/close-insert-undo)
  (condition-case nil
      (progn (undo-only 1) (message "已撤销"))
    (user-error (message "没有可撤销的操作")))
  (my/open-insert-undo))

(defun my/redo ()
  "Windows Ctrl+Y。"
  (interactive)
  (my/close-insert-undo)
  (condition-case nil
      (progn (undo-redo 1) (message "已重做"))
    (user-error (message "没有可重做的操作")))
  (my/open-insert-undo))

(defun my/region-active-p ()
  (and (or (use-region-p)
           (and (bound-and-true-p evil-mode)
                (eq evil-state 'visual)
                (region-active-p)))
       (> (abs (- (region-end) (region-beginning))) 0)))

(defun my/copy-line ()
  (kill-ring-save (line-beginning-position) (line-beginning-position 2))
  (message "已复制当前行"))

(defun my/copy ()
  "Windows Ctrl+C。没选中时复制当前行。"
  (interactive)
  (cond
   ((my/region-active-p)
    (let ((n (abs (- (region-end) (region-beginning)))))
      (kill-ring-save (region-beginning) (region-end))
      (when (and (bound-and-true-p evil-mode) (eq evil-state 'visual))
        (evil-exit-visual-state))
      (message "已复制 %d 个字符" n)))
   (t (my/copy-line))))

(defun my/cut ()
  "Windows Ctrl+X。没选中时剪切当前行。"
  (interactive)
  (condition-case nil
      (cond
       ((my/region-active-p)
        (let ((n (abs (- (region-end) (region-beginning)))))
          (kill-region (region-beginning) (region-end))
          (when (and (bound-and-true-p evil-mode) (eq evil-state 'visual))
            (evil-exit-visual-state))
          (message "已剪切 %d 个字符" n)))
       (t
        (kill-region (line-beginning-position) (line-beginning-position 2))
        (message "已剪切当前行")))
    (buffer-read-only (message "只读，不能剪切"))
    (text-read-only (message "只读，不能剪切"))))

(defun my/shift-move (fn)
  "Extend the selection by one movement. Works in INSERT and NORMAL."
  (cond
   ((and (bound-and-true-p evil-mode) (eq evil-state 'visual))
    (let ((deactivate-mark nil))
      (condition-case nil (funcall fn 1) (error nil))))
   ((and (bound-and-true-p evil-mode)
         (memq evil-state '(normal motion)))
    (evil-visual-char)
    (let ((deactivate-mark nil))
      (condition-case nil (funcall fn 1) (error nil))))
   (t
    (unless (region-active-p)
      (setq transient-mark-mode t)
      (push-mark nil t t))
    (let ((deactivate-mark nil))
      (condition-case nil (funcall fn 1) (error nil))))))

(defun my/sel-left () (interactive) (my/shift-move #'left-char))
(defun my/sel-right () (interactive) (my/shift-move #'right-char))
(defun my/sel-up () (interactive) (my/shift-move #'previous-line))
(defun my/sel-down () (interactive) (my/shift-move #'next-line))
(defun my/sel-word-left () (interactive) (my/shift-move #'left-word))
(defun my/sel-word-right () (interactive) (my/shift-move #'right-word))
(defun my/sel-bol () (interactive) (my/shift-move #'beginning-of-line))
(defun my/sel-eol () (interactive) (my/shift-move #'end-of-line))
(defun my/sel-bob () (interactive) (my/shift-move #'beginning-of-buffer))
(defun my/sel-eob () (interactive) (my/shift-move #'end-of-buffer))

(defun my/clipboard-text ()
  "Text to paste. Never blocks, and falls back to the kill ring."
  (let ((text (or (my/xclip-paste)
                  my/copied-text
                  (and kill-ring (stringp (car kill-ring)) (car kill-ring)))))
    (unless (and (stringp text) (not (string-empty-p text)))
      (user-error "剪贴板是空的"))
    text))

(defun my/paste ()
  "Windows Ctrl+V。有选区就先替换选区。"
  (interactive)
  (let ((text (my/clipboard-text)))
    (condition-case nil
        (progn
          (when (my/region-active-p)
            (delete-region (region-beginning) (region-end))
            (when (and (bound-and-true-p evil-mode) (eq evil-state 'visual))
              (evil-exit-visual-state)))
          (insert text)
          (message "已粘贴"))
      (buffer-read-only (message "只读，不能粘贴"))
      (text-read-only (message "只读，不能粘贴")))))

(defun my/select-all ()
  (interactive)
  (if (<= (buffer-size) 0)
      (message "缓冲区是空的")
    (if (fboundp 'evil-visual-select)
        (evil-visual-select (point-min) (point-max))
      (mark-whole-buffer))
    (message "已全选")))

(defun my/save ()
  (interactive)
  (cond
   (buffer-file-name
    (save-buffer)
    (message "已保存 %s" buffer-file-name))
   (t
    (let ((path (expand-file-name (read-file-name "保存到： "))))
      (when (or (string-blank-p path) (file-directory-p path))
        (user-error "请指定一个文件，不能是目录"))
      (if (or (not (file-exists-p path))
              (y-or-n-p "文件已存在，覆盖吗？ "))
          (progn
            (write-file path)
            (message "已保存 %s" path))
        (message "未保存"))))))

(defun my/open ()
  (interactive)
  (find-file (read-file-name "打开文件： " nil nil nil)))

(defun my/create-untitled ()
  "Empty text buffer named untitled, untitled2, …"
  (let ((name "untitled")
        (n 1)
        buf)
    (while (get-buffer name)
      (setq n (1+ n)
            name (format "untitled%d" n)))
    (setq buf (generate-new-buffer name))
    (with-current-buffer buf
      (text-mode)
      (setq buffer-offer-save nil)
      (let ((dir (expand-file-name "autosave/" user-emacs-directory)))
        (when (file-directory-p dir)
          ;; Setting the name is what turns auto-save on. Calling
          ;; auto-save-mode would replace it with a file in the
          ;; current directory.
          (setq-local buffer-auto-save-file-name
                      (expand-file-name
                       (format "#%s-%s#" (emacs-pid) name) dir))))
      (set-buffer-modified-p nil))
    buf))

(defun my/new-file ()
  (interactive)
  (switch-to-buffer (my/create-untitled))
  (when (fboundp 'evil-insert-state)
    (evil-insert-state))
  (message "#%d %s    i 进入 INSERT，Esc 回到 NORMAL" (my/buffer-id) (buffer-name)))

(defun my/ensure-blank-start ()
  "With no file on the command line, show one empty buffer in NORMAL."
  (unless (catch 'file
            (dolist (buf (buffer-list))
              (when (buffer-file-name buf)
                (throw 'file t))))
    (switch-to-buffer (my/create-untitled))
    (when (fboundp 'evil-normal-state)
      (evil-normal-state)))
  (let ((scratch (get-buffer "*scratch*")))
    (when (and scratch
               (not (eq scratch (current-buffer)))
               (= (buffer-size scratch) 0)
               (not (buffer-modified-p scratch)))
      (kill-buffer scratch)))
  (my/enforce-one-window))
(add-hook 'emacs-startup-hook #'my/ensure-blank-start -90)

(defun my/close-buffer ()
  "Windows Ctrl+W。已修改时询问。说明和列表直接返回。"
  (interactive)
  (if (memq major-mode '(my/help-mode my/buffer-menu-mode))
      (my/quit-to-previous)
    (let ((buf (current-buffer)))
      (when (and (buffer-modified-p buf) (my/real-buffer-p buf))
        (pcase (read-char-choice
                (format "「%s」已修改  s 保存并关闭  d 不保存  c 取消 "
                        (buffer-name buf))
                '(?s ?d ?c ?S ?D ?C))
          ((or ?c ?C) (user-error "已取消"))
          ((or ?s ?S)
           (my/save)
           (when (buffer-modified-p buf)
             (user-error "没有保存，已取消关闭")))
          ((or ?d ?D) (set-buffer-modified-p nil))))
      (when (kill-buffer buf)
        (unless (my/real-buffer-p (current-buffer))
          (my/new-file))))))

(defun my/find ()
  (interactive)
  (call-interactively #'isearch-forward))

(defun my/find-next ()
  (interactive)
  (cond
   ((bound-and-true-p isearch-mode) (isearch-repeat-forward))
   ((and (stringp isearch-string) (not (string-empty-p isearch-string)))
    (let ((case-fold-search isearch-case-fold-search))
      (unless (search-forward isearch-string nil t)
        (goto-char (point-min))
        (unless (search-forward isearch-string nil t)
          (message "找不到：%s" isearch-string)))))
   (t (message "先按 Ctrl+F 查找"))))

(defun my/find-prev ()
  (interactive)
  (cond
   ((bound-and-true-p isearch-mode) (isearch-repeat-backward))
   ((and (stringp isearch-string) (not (string-empty-p isearch-string)))
    (let ((case-fold-search isearch-case-fold-search))
      (unless (search-backward isearch-string nil t)
        (goto-char (point-max))
        (unless (search-backward isearch-string nil t)
          (message "找不到：%s" isearch-string)))))
   (t (message "先按 Ctrl+F 查找"))))

(defun my/replace ()
  "按提示替换。逐处确认时：y 替换，n 跳过，! 全部，q 结束。"
  (interactive)
  (let* ((from (read-string "查找： "))
         (to (read-string (format "把「%s」替换成： " from))))
    (when (string-empty-p from)
      (user-error "查找内容不能为空"))
    (query-replace from to)))

(defun my/visual-block ()
  (interactive)
  (unless (fboundp 'evil-visual-block)
    (user-error "Vim 模式不可用"))
  (when (memq evil-state '(insert replace emacs))
    (evil-normal-state))
  (evil-visual-block))

(defun my/ex-not-editing ()
  (interactive)
  (message "先按 Esc 或 Ctrl+G 离开命令栏"))

(defun my/ex-copy-or-abort ()
  (interactive)
  (if (use-region-p)
      (progn (kill-ring-save (region-beginning) (region-end) t)
             (message "已复制"))
    (abort-recursive-edit)))

(defun my/ex-cut ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) t)
    (message "先选中命令栏里的文字，再剪切")))

(defun my/ex-select-all ()
  (interactive)
  (when (minibufferp)
    (goto-char (minibuffer-prompt-end))
    (set-mark (point))
    (goto-char (point-max))
    (activate-mark)))

(defvar my/win-keys
  '(("C-c" . my/copy)
    ("C-x" . my/cut)
    ("C-v" . my/paste)
    ("C-z" . my/undo)
    ("C-y" . my/redo)
    ("C-S-z" . my/redo)
    ("C-a" . my/select-all)
    ("C-s" . my/save)
    ("C-o" . my/open)
    ("C-n" . my/new-file)
    ("C-w" . my/close-buffer)
    ("C-f" . my/find)
    ("C-h" . my/replace)
    ("<f3>" . my/find-next)
    ("S-<f3>" . my/find-prev)
    ("C-S-v" . my/visual-block)
    ("C-<left>" . left-word)
    ("C-<right>" . right-word)
    ("C-S-<left>" . my/sel-word-left)
    ("C-S-<right>" . my/sel-word-right)
    ("C-<backspace>" . backward-kill-word)
    ("C-<delete>" . kill-word)
    ("C-<home>" . beginning-of-buffer)
    ("C-<end>" . end-of-buffer)
    ("S-<home>" . my/sel-bol)
    ("S-<end>" . my/sel-eol)
    ("<home>" . beginning-of-line)
    ("<end>" . end-of-line)
    ("S-<left>" . my/sel-left)
    ("S-<right>" . my/sel-right)
    ("S-<up>" . my/sel-up)
    ("S-<down>" . my/sel-down))
  "Windows text-editing keys shared by every Vim state.")

(defun my/define-win-keys (map)
  (when (keymapp map)
    (dolist (cell my/win-keys)
      (define-key map (kbd (car cell)) (cdr cell)))))

(define-minor-mode my/win-keys-mode
  "Windows 复制、粘贴、撤销、保存等快捷键。"
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (dolist (cell my/win-keys)
              (define-key map (kbd (car cell)) (cdr cell)))
            map))

(defun my/win-keys-maybe ()
  (unless (minibufferp)
    (my/win-keys-mode 1)))

(define-globalized-minor-mode my/win-keys-global-mode
  my/win-keys-mode my/win-keys-maybe)

(defun my/install-windows-keys ()
  (dolist (map (list evil-normal-state-map evil-visual-state-map
                     evil-motion-state-map evil-insert-state-map
                     evil-replace-state-map evil-emacs-state-map))
    (my/define-win-keys map))
  ;; Netrw: NORMAL `-` opens this file's directory, or the parent when already there.
  (define-key evil-normal-state-map (kbd "-") #'dired-jump)
  (define-key evil-normal-state-map (kbd "C-g") #'evil-force-normal-state)
  (define-key evil-visual-state-map (kbd "C-g") #'evil-force-normal-state)
  (define-key evil-insert-state-map [escape] #'evil-normal-state)
  (define-key evil-replace-state-map [escape] #'evil-normal-state)
  (define-key evil-command-line-map (kbd "C-c") #'my/ex-copy-or-abort)
  (define-key evil-command-line-map (kbd "C-x") #'my/ex-cut)
  (define-key evil-command-line-map (kbd "C-v") #'yank)
  (define-key evil-command-line-map (kbd "C-z") #'my/undo)
  (define-key evil-command-line-map (kbd "C-y") #'my/redo)
  (define-key evil-command-line-map (kbd "C-a") #'my/ex-select-all)
  (define-key evil-command-line-map (kbd "C-f") #'forward-char)
  (define-key evil-command-line-map (kbd "C-b") #'backward-char)
  (define-key evil-command-line-map (kbd "C-s") #'my/ex-not-editing)
  (define-key evil-command-line-map (kbd "C-o") #'my/ex-not-editing)
  (define-key evil-command-line-map (kbd "C-w") #'backward-kill-word)
  (define-key evil-command-line-map (kbd "C-h") #'backward-delete-char)
  (define-key evil-command-line-map (kbd "C-q") #'quoted-insert)
  (when (keymapp isearch-mode-map)
    (define-key isearch-mode-map (kbd "<f3>") #'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "S-<f3>") #'isearch-repeat-backward))
  (dolist (key '("C-c" "C-x" "C-v" "C-z" "C-y" "C-a" "C-s" "C-o"
                 "C-n" "C-w" "C-f" "C-h"))
    (define-key global-map (kbd key) (lookup-key my/win-keys-mode-map (kbd key))))
  (my/win-keys-global-mode 1)
  (when (fboundp 'evil-make-intercept-map)
    (evil-make-intercept-map my/win-keys-mode-map 'all))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (bound-and-true-p evil-local-mode)
                 (fboundp 'evil-normalize-keymaps))
        (evil-normalize-keymaps))))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'dired-mode 'normal))
  (setq evil-default-state 'normal
        shift-select-mode t))

(my/install-one-window)

(when (require 'evil nil t)
  (condition-case err
      (progn
        (evil-mode 1)
        (my/install-windows-keys)
        (require 'vim-ex)
        (require 'ide)
        (my/install-ex-commands)
        (my/install-ide-ex))
    (error (message "evil setup: %s" err))))

(defvar my/netrw-target nil)
(defvar my/netrw-hist nil)
(defvar my/netrw-hist-index 0)
(defvar my/netrw-hist-lock nil)
(defvar my/netrw-return nil)
(defvar my/netrw-sort 'name)
(defvar my/netrw-reverse nil)
(defvar my/netrw-long nil)
(defvar my/netrw-applying nil)

(defun my/netrw-switches ()
  (concat "-alh --group-directories-first"
          (pcase my/netrw-sort
            ('time " -t")
            ('size " -S")
            (_ ""))
          (if my/netrw-reverse " -r" "")))

(defun my/netrw-apply-listing ()
  (unless my/netrw-applying
    (let ((my/netrw-applying t))
      (let ((sw (my/netrw-switches)))
        (when (and (not (bound-and-true-p dired-sort-inhibit))
                   (not (equal sw dired-actual-switches)))
          (dired-sort-other sw)))
      (dired-hide-details-mode (if my/netrw-long -1 1)))))

(defun my/netrw-remember ()
  (unless my/netrw-hist-lock
    (let ((dir (file-name-as-directory (expand-file-name default-directory))))
      (unless (equal dir (nth my/netrw-hist-index my/netrw-hist))
        (let ((hist (cons dir (nthcdr my/netrw-hist-index my/netrw-hist))))
          (when (> (length hist) 40)
            (setcdr (nthcdr 39 hist) nil))
          (setq my/netrw-hist hist
                my/netrw-hist-index 0))))))

(defun my/netrw-note-origin ()
  (let ((prev (other-buffer (current-buffer) t)))
    (when (and prev
               (buffer-live-p prev)
               (not (with-current-buffer prev (derived-mode-p 'dired-mode))))
      (setq my/netrw-return prev))))

(defun my/netrw-quit ()
  "Leave the listing and return to the buffer that opened it."
  (interactive)
  (if (and my/netrw-return (buffer-live-p my/netrw-return))
      (switch-to-buffer my/netrw-return)
    (quit-window)))

(defun my/netrw-open ()
  "Open the file or directory on this line in the same window."
  (interactive)
  (dired-find-file))

(defun my/netrw-up ()
  "Parent directory, with point on the directory just left."
  (interactive)
  (dired-jump))

(defun my/netrw-history (step)
  (let ((n (+ my/netrw-hist-index step)))
    (unless (and (>= n 0) (< n (length my/netrw-hist)))
      (user-error (if (> step 0) "没有更早的目录" "没有更新的目录")))
    (setq my/netrw-hist-index n)
    (let ((my/netrw-hist-lock t))
      (dired (nth n my/netrw-hist)))))

(defun my/netrw-back ()
  (interactive)
  (my/netrw-history 1))

(defun my/netrw-forward ()
  (interactive)
  (my/netrw-history -1))

(defun my/netrw-refresh ()
  (interactive)
  (revert-buffer nil t))

(defun my/netrw-mkdir ()
  (interactive)
  (call-interactively #'dired-create-directory))

(defun my/netrw-new-file ()
  (interactive)
  (let* ((dir (dired-current-directory))
         (name (read-string (format "新文件（%s）: " dir))))
    (when (string-blank-p name)
      (user-error "未输入文件名"))
    (find-file (expand-file-name name dir))))

(defun my/netrw-delete ()
  (interactive)
  (dired-do-delete))

(defun my/netrw-rename ()
  (interactive)
  (call-interactively #'dired-do-rename))

(defun my/netrw-external ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (unless (executable-find "xdg-open")
      (user-error "没有 xdg-open"))
    (let ((proc (start-process "xdg-open" nil "xdg-open" file)))
      (set-process-query-on-exit-flag proc nil))
    (message "用系统程序打开 %s" (file-name-nondirectory file))))

(defun my/netrw-execute ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (unless (and (file-regular-p file) (file-executable-p file))
      (user-error "这个文件不能执行"))
    (let ((proc (start-process (file-name-nondirectory file) nil file)))
      (set-process-query-on-exit-flag proc nil))
    (message "已启动 %s" (file-name-nondirectory file))))

(defun my/netrw-cd ()
  (interactive)
  (cd (dired-current-directory))
  (message "%s" default-directory))

(defun my/netrw-cycle-style ()
  (interactive)
  (setq my/netrw-long (not my/netrw-long))
  (dired-hide-details-mode (if my/netrw-long -1 1))
  (message "列表：%s" (if my/netrw-long "long 详细信息" "thin 仅文件名")))

(defun my/netrw-cycle-sort ()
  (interactive)
  (setq my/netrw-sort (pcase my/netrw-sort
                        ('name 'time)
                        ('time 'size)
                        (_ 'name)))
  (my/netrw-apply-listing)
  (message "排序：%s%s"
           (pcase my/netrw-sort ('time "时间") ('size "大小") (_ "名称"))
           (if my/netrw-reverse "，逆序" "")))

(defun my/netrw-reverse-sort ()
  (interactive)
  (setq my/netrw-reverse (not my/netrw-reverse))
  (my/netrw-apply-listing)
  (message "排序：%s" (if my/netrw-reverse "逆序" "正序")))

(defun my/netrw-toggle-dotfiles ()
  (interactive)
  (require 'dired-x)
  (dired-omit-mode 'toggle)
  (message "点文件：%s" (if dired-omit-mode "隐藏" "显示")))

(defun my/netrw-files ()
  (let (out)
    (dolist (file (dired-get-marked-files nil nil nil nil t))
      (unless (member (file-name-nondirectory (directory-file-name file))
                      '("." ".."))
        (push file out)))
    (or (nreverse out) (user-error "没有选中文件"))))

(defun my/netrw-mark ()
  (interactive)
  (dired-mark 1))

(defun my/netrw-unmark ()
  (interactive)
  (dired-unmark 1))

(defun my/netrw-unmark-all ()
  (interactive)
  (dired-unmark-all-marks)
  (message "已取消全部标记"))

(defun my/netrw-set-target ()
  (interactive)
  (setq my/netrw-target (file-name-as-directory (dired-current-directory)))
  (message "目标目录 %s" my/netrw-target))

(defun my/netrw-transfer (move)
  (unless (and my/netrw-target (file-directory-p my/netrw-target))
    (user-error "先到目标目录按 mt"))
  (unwind-protect
      (dolist (file (my/netrw-files))
        (let ((dest (expand-file-name
                     (file-name-nondirectory (directory-file-name file))
                     my/netrw-target)))
          (when (and (file-exists-p file)
                     (file-exists-p dest)
                     (file-equal-p file dest))
            (user-error "目标和源是同一个文件"))
          (when (and (file-directory-p file)
                     (string-prefix-p
                      (file-name-as-directory (expand-file-name file))
                      (file-name-as-directory (expand-file-name dest))))
            (user-error "不能把目录放进它自己里面"))
          (condition-case err
              (if move
                  (rename-file file dest)
                (if (file-directory-p file)
                    (copy-directory file dest nil t t)
                  (copy-file file dest nil t)))
            (error (user-error "%s" (error-message-string err))))))
    (when (derived-mode-p 'dired-mode)
      (ignore-errors (revert-buffer nil t))))
  (message "已%s到 %s" (if move "移动" "复制") my/netrw-target))

(defun my/netrw-copy ()
  (interactive)
  (my/netrw-transfer nil))

(defun my/netrw-move ()
  (interactive)
  (my/netrw-transfer t))

(defun my/netrw-keep-prefix (key parent-map)
  (dolist (state '(normal motion))
    (let* ((aux (ignore-errors (evil-get-auxiliary-keymap dired-mode-map state)))
           (sub (and aux (lookup-key aux key)))
           (parent (lookup-key parent-map key)))
      (when (and (keymapp sub) (keymapp parent) (not (eq sub parent)))
        (set-keymap-parent sub parent)))))

(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-auto-revert-buffer nil
        dired-listing-switches (my/netrw-switches)
        dired-omit-files "\\`\\.[^.]")
  (setq ls-lisp-use-insert-directory-program t
        insert-directory-program "ls")
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t))
  (setq dired-recursive-deletes 'top)
  (add-hook 'dired-mode-hook #'my/netrw-note-origin)
  (add-hook 'dired-mode-hook #'my/netrw-remember)
  (add-hook 'dired-after-readin-hook #'my/netrw-remember)
  (add-hook 'dired-after-readin-hook #'my/netrw-apply-listing)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'dired-mode 'normal))
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal motion) dired-mode-map
      (kbd "-") #'my/netrw-up
      (kbd "<backspace>") #'my/netrw-up
      (kbd "DEL") #'my/netrw-up
      (kbd "h") #'my/netrw-up
      (kbd "RET") #'my/netrw-open
      (kbd "o") #'my/netrw-open
      (kbd "l") #'my/netrw-open
      (kbd "t") #'my/netrw-open
      (kbd "p") #'my/netrw-open
      (kbd "gf") #'my/netrw-open
      (kbd "q") #'my/netrw-quit
      (kbd "u") #'my/netrw-back
      (kbd "U") #'my/netrw-forward
      (kbd "C-l") #'my/netrw-refresh
      (kbd "gr") #'my/netrw-refresh
      (kbd "d") #'my/netrw-mkdir
      (kbd "%") #'my/netrw-new-file
      (kbd "D") #'my/netrw-delete
      (kbd "<delete>") #'my/netrw-delete
      (kbd "R") #'my/netrw-rename
      (kbd "x") #'my/netrw-external
      (kbd "X") #'my/netrw-execute
      (kbd "c") #'my/netrw-cd
      (kbd "i") #'my/netrw-cycle-style
      (kbd "I") #'my/netrw-cycle-style
      (kbd "s") #'my/netrw-cycle-sort
      (kbd "r") #'my/netrw-reverse-sort
      (kbd "a") #'my/netrw-toggle-dotfiles
      (kbd "gh") #'my/netrw-toggle-dotfiles
      (kbd "mf") #'my/netrw-mark
      (kbd "mF") #'my/netrw-unmark
      (kbd "mu") #'my/netrw-unmark-all
      (kbd "mt") #'my/netrw-set-target
      (kbd "mc") #'my/netrw-copy
      (kbd "mm") #'my/netrw-move)
    (my/netrw-keep-prefix (kbd "g") evil-motion-state-map)))


(defun my/maybe-hs ()
  (ignore-errors
    (when (and (fboundp 'hs-minor-mode)
               (< (buffer-size) 400000)
               (not (bound-and-true-p so-long-minor-mode)))
      (hs-minor-mode 1))))
(add-hook 'prog-mode-hook #'my/maybe-hs)

(with-eval-after-load 'hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" . "#f8c762") ("FIXME" . "#E34671") ("BUG" . "#E34671")
          ("HACK" . "#efb080") ("NOTE" . "#82d2ce") ("XXX" . "#AA9BF5"))))
(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil diff-hl-side 'right)
  (when (boundp 'diff-hl-margin-side) (setq diff-hl-margin-side 'right))
  (when (fboundp 'diff-hl-margin-mode) (diff-hl-margin-mode 1)))

(remove-hook 'find-file-hook #'vc-refresh-state)
(with-eval-after-load 'vc-hooks
  (remove-hook 'find-file-hook #'vc-refresh-state))

;;; --- status bar: details flush right ---
(require 'subr-x)

(defun my/ml-join (items)
  (mapconcat #'identity (delq nil items) "  "))

(defvar my/ml-detail-columns 120
  "At this window width and above, the status bar shows every field.")

(defun my/ml-detailed-p ()
  (>= (window-width) my/ml-detail-columns))

(defun my/ml-ellipsize (s max)
  (cond ((or (null s) (<= (string-width s) max)) s)
        ((<= max 1) "")
        (t (concat (truncate-string-to-width s (max 1 (1- max))) "…"))))

(defvar-local my/ml-parts nil)

(defun my/ml-part (key)
  (cdr (assq key my/ml-parts)))

(defun my/mode-line-flush-right ()
  "Pin status text to the right edge. Wide windows show every field."
  (condition-case nil
      (let* ((s (my/ml-status))
             (w (+ (string-width s) 1)))
        (concat (propertize " " 'display `(space :align-to (- right ,w)))
                s
                " "))
    (error " ")))

(defun my/ml-branch ()
  (when (and (bound-and-true-p vc-mode) (> (length vc-mode) 0))
    (let ((s (string-trim (substring-no-properties vc-mode))))
      (if (string-match "\\`\\([A-Za-z]+\\)[-: ]+\\(.*\\)\\'" s)
          (format "%s:%s" (downcase (match-string 1 s)) (match-string 2 s))
        s))))

(defun my/ml-encoding ()
  (let* ((name (downcase (symbol-name (coding-system-base
                                      (or buffer-file-coding-system 'utf-8)))))
         (pretty (cond
                  ((string-match-p "utf-8" name) "UTF-8")
                  ((string-match-p "utf-16be" name) "UTF-16BE")
                  ((string-match-p "utf-16le" name) "UTF-16LE")
                  ((string-match-p "utf-16" name) "UTF-16")
                  ((string-match-p "gb18030" name) "GB18030")
                  ((string-match-p "gbk\\|cp936" name) "GBK")
                  ((string-match-p "gb2312" name) "GB2312")
                  ((string-match-p "latin-1\\|iso-8859-1" name) "ISO-8859-1")
                  (t (upcase name)))))
    (if (and (fboundp 'my/vim-has-bom-p) (my/vim-has-bom-p))
        (concat pretty " BOM")
      pretty)))

(defun my/ml-eol ()
  (pcase (coding-system-eol-type (or buffer-file-coding-system 'utf-8))
    (1 "CRLF") (2 "CR") (_ "LF")))

(defun my/ml-indent ()
  (if indent-tabs-mode
      (format "Tab Size: %d" tab-width)
    (format "Spaces: %d"
            (or (and (derived-mode-p 'python-mode) (bound-and-true-p python-indent-offset))
                (and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                     (bound-and-true-p c-basic-offset))
                (bound-and-true-p evil-shift-width)
                tab-width))))

(defun my/ml-lang ()
  (or (and buffer-file-name
           (pcase (downcase (or (file-name-extension buffer-file-name) ""))
             ("cs" "C#")
             ((or "hlsl" "hlsli" "fx") "HLSL")
             ("shader" "ShaderLab")
             ("vue" "Vue") ("tsx" "TypeScript TSX") ("jsx" "JavaScript JSX")
             ("rpy" "Ren'Py") ("gd" "GDScript") (_ nil)))
      (and (eq major-mode 'web-mode)
           (boundp 'web-mode-content-type)
           (pcase web-mode-content-type
             ("jsx" "JavaScript JSX") ("tsx" "TypeScript TSX")
             ("vue" "Vue") ("css" "CSS") ("javascript" "JavaScript") (_ "HTML")))
      (alist-get major-mode
                 '((python-mode . "Python") (python-ts-mode . "Python")
                   (lua-mode . "Lua") (lua-ts-mode . "Lua")
                   (c-mode . "C") (c-ts-mode . "C")
                   (c++-mode . "C++") (c++-ts-mode . "C++")
                   (java-mode . "Java") (java-ts-mode . "Java")
                   (js-mode . "JavaScript") (js-ts-mode . "JavaScript")
                   (javascript-mode . "JavaScript")
                   (typescript-mode . "TypeScript") (typescript-ts-mode . "TypeScript")
                   (css-mode . "CSS") (css-ts-mode . "CSS")
                   (json-mode . "JSON") (json-ts-mode . "JSON")
                   (yaml-mode . "YAML") (yaml-ts-mode . "YAML")
                   (markdown-mode . "Markdown") (gfm-mode . "Markdown")
                   (rust-mode . "Rust") (rust-ts-mode . "Rust")
                   (go-mode . "Go") (go-ts-mode . "Go")
                   (php-mode . "PHP") (ruby-mode . "Ruby") (ruby-ts-mode . "Ruby")
                   (perl-mode . "Perl") (sh-mode . "Shell Script")
                   (bash-ts-mode . "Shell Script")
                   (emacs-lisp-mode . "Emacs Lisp")
                   (lisp-interaction-mode . "Emacs Lisp")
                   (org-mode . "Org") (sql-mode . "SQL")
                   (cmake-mode . "CMake") (dockerfile-mode . "Dockerfile")
                   (gdscript-mode . "GDScript") (gdscript-ts-mode . "GDScript")
                   (glsl-mode . "GLSL") (hlsl-mode . "HLSL") (wgsl-mode . "WGSL")
                   (nasm-mode . "Assembly") (asm-mode . "Assembly")
                   (masm-mode . "Assembly") (wat-mode . "WebAssembly")
                   (llvm-mode . "LLVM") (cuda-mode . "CUDA")
                   (zig-mode . "Zig") (nim-mode . "Nim")
                   (csharp-mode . "C#") (csharp-tree-sitter-mode . "C#")
                   (kotlin-mode . "Kotlin") (dart-mode . "Dart")
                   (swift-mode . "Swift") (groovy-mode . "Groovy")
                   (scala-mode . "Scala") (haskell-mode . "Haskell")
                   (haskell-literate-mode . "Literate Haskell")
                   (haskell-cabal-mode . "Cabal") (ghci-script-mode . "GHCi")
                   (purescript-mode . "PureScript") (idris-mode . "Idris")
                   (idris2-mode . "Idris") (agda2-mode . "Agda")
                   (lean4-mode . "Lean") (lean-mode . "Lean")
                   (fstar-mode . "F*") (reason-mode . "Reason")
                   (rescript-mode . "ReScript") (dune-mode . "Dune")
                   (lfe-mode . "LFE") (prolog-mode . "Prolog")
                   (tuareg-interface-mode . "OCaml")
                   (tuareg-menhir-mode . "Menhir")
                   (sml-cm-mode . "Standard ML")
                   (elixir-mode . "Elixir") (erlang-mode . "Erlang")
                   (clojure-mode . "Clojure") (fennel-mode . "Fennel")
                   (haxe-mode . "Haxe") (protobuf-mode . "Protobuf")
                   (graphql-mode . "GraphQL") (powershell-mode . "PowerShell")
                   (vimrc-mode . "Vim script") (meson-mode . "Meson")
                   (objc-mode . "Objective-C") (scss-mode . "SCSS")
                   (less-css-mode . "LESS") (sass-mode . "Sass")
                   (svelte-mode . "Svelte") (nix-mode . "Nix")
                   (terraform-mode . "Terraform") (hcl-mode . "HCL")
                   (elm-mode . "Elm") (tuareg-mode . "OCaml")
                   (fsharp-mode . "F#") (julia-mode . "Julia")
                   (crystal-mode . "Crystal") (odin-mode . "Odin")
                   (gleam-mode . "Gleam") (solidity-mode . "Solidity")
                   (coffee-mode . "CoffeeScript") (pug-mode . "Pug")
                   (haml-mode . "Haml") (slim-mode . "Slim")
                   (d-mode . "D") (fish-mode . "Fish")
                   (nginx-mode . "Nginx") (apache-mode . "Apache")
                   (systemd-mode . "systemd") (dotenv-mode . "Dotenv")
                   (csv-mode . "CSV") (qml-mode . "QML")
                   (vala-mode . "Vala") (thrift-mode . "Thrift")
                   (jsonnet-mode . "Jsonnet") (just-mode . "Just")
                   (puppet-mode . "Puppet") (dhall-mode . "Dhall")
                   (jq-mode . "jq") (cython-mode . "Cython")
                   (gitignore-mode . "Git") (gitconfig-mode . "Git")
                   (adoc-mode . "AsciiDoc") (gnuplot-mode . "Gnuplot")
                   (sml-mode . "Standard ML") (forth-mode . "Forth")
                   (mermaid-mode . "Mermaid") (plantuml-mode . "PlantUML")
                   (graphviz-dot-mode . "Graphviz") (scad-mode . "OpenSCAD")
                   (janet-mode . "Janet") (hy-mode . "Hy")
                   (prisma-mode . "Prisma") (cue-mode . "Cue")
                   (purescript-mode . "PureScript") (bison-mode . "Bison")
                   (verilog-mode . "Verilog") (vhdl-mode . "VHDL")
                   (fortran-mode . "Fortran") (f90-mode . "Fortran")
                   (pascal-mode . "Pascal") (awk-mode . "AWK")
                   (ess-r-mode . "R") (ess-mode . "R") (my-r-mode . "R")
                   (matlab-mode . "MATLAB") (ahk-mode . "AutoHotkey")
                   (autoit-mode . "AutoIt") (raku-mode . "Raku")
                   (rego-mode . "Rego") (feature-mode . "Gherkin")
                   (gherkin-mode . "Gherkin") (pkl-mode . "Pkl")
                   (prql-mode . "PRQL") (promql-mode . "PromQL")
                   (bicep-mode . "Bicep") (ballerina-mode . "Ballerina")
                   (chapel-mode . "Chapel") (chapel-lite-mode . "Chapel") (hare-mode . "Hare")
                   (holyc-mode . "HolyC") (basic-mode . "Visual Basic")
                   (visual-basic-mode . "Visual Basic")
                   (shaderlab-mode . "ShaderLab") (vex-mode . "VEX")
                   (rsl-mode . "RSL") (agal-mode . "AGAL")
                   (erabasic-mode . "ERABasic") (coq-mode . "Coq")
                   (ps-mode . "PostScript") (pov-mode . "POV-Ray")
                   (kdl-mode . "KDL") (kdl-lite-mode . "KDL") (avro-idl-mode . "Avro IDL")
                   (gemtext-mode . "Gemtext") (ceylon-mode . "Ceylon")
                   (modula-2-mode . "Modula-2") (nroff-mode . "Groff")
                   (sed-mode . "Sed")
                   (rst-mode . "reStructuredText") (scheme-mode . "Scheme")
                   (lisp-mode . "Common Lisp") (octave-mode . "Octave")
                   (conf-mode . "INI") (conf-unix-mode . "INI")
                   (conf-windows-mode . "INI") (conf-toml-mode . "TOML")
                   (bat-mode . "Batch") (nxml-mode . "XML")
                   (html-mode . "HTML") (mhtml-mode . "HTML")
                   (text-mode . "Plain Text") (fundamental-mode . "Plain Text")
                   (makefile-gmake-mode . "Makefile") (makefile-mode . "Makefile")
                   (diff-mode . "Diff") (latex-mode . "LaTeX") (tex-mode . "TeX")
                   (dired-mode . "Directory"))
                 nil nil #'eq)
      (and (fboundp 'my/vim-ft-name) (capitalize (my/vim-ft-name)))
      "Plain Text"))

(defun my/ml-problems ()
  (when (and (bound-and-true-p flymake-mode) (fboundp 'flymake-diagnostics))
    (let ((err 0) (warn 0) (note 0))
      (dolist (d (flymake-diagnostics))
        (let ((ty (flymake-diagnostic-type d)))
          (cond ((memq ty '(error :error)) (setq err (1+ err)))
                ((memq ty '(warning :warning)) (setq warn (1+ warn)))
                (t (setq note (1+ note))))))
      (when (> (+ err warn note) 0)
        (string-trim
         (concat (and (> err 0) (format "Error %d  " err))
                 (and (> warn 0) (format "Warn %d  " warn))
                 (and (> note 0) (format "Info %d" note))))))))

(defun my/ml-file ()
  (if (not buffer-file-name)
      (buffer-name)
    (file-name-nondirectory buffer-file-name)))

(defun my/ml-size ()
  (let ((n (buffer-size)))
    (cond ((< n 1024) (format "%d B" n))
          ((< n 1048576) (format "%.1f KB" (/ n 1024.0)))
          (t (format "%.1f MB" (/ n 1048576.0))))))

(defun my/ml-dir ()
  (let ((dir (or (and buffer-file-name
                      (file-name-directory buffer-file-name))
                 default-directory)))
    (when dir
      (file-name-nondirectory (directory-file-name (expand-file-name dir))))))

(defvar-local my/ml-state "NORMAL")

(defun my/ml-state-string ()
  (cond
   ((not (bound-and-true-p evil-mode)) "EMACS")
   ((eq evil-state 'insert) "INSERT")
   ((eq evil-state 'replace) "REPLACE")
   ((eq evil-state 'emacs) "EMACS")
   ((eq evil-state 'visual)
    (pcase (and (boundp 'evil-visual-selection) evil-visual-selection)
      ('line "VISUAL LINE")
      ('block "VISUAL BLOCK")
      (_ "VISUAL")))
   ((eq evil-state 'operator) "OPERATOR")
   (t "NORMAL")))

(defun my/ml-sync-state ()
  (ignore-errors
    (setq my/ml-state (my/ml-state-string))
    (force-mode-line-update)))

(dolist (hook '(evil-insert-state-entry-hook
                evil-replace-state-entry-hook
                evil-normal-state-entry-hook
                evil-visual-state-entry-hook
                evil-emacs-state-entry-hook))
  (add-hook hook #'my/ml-sync-state))
(add-hook 'evil-insert-state-exit-hook #'my/ml-rebuild-heavy)
(add-hook 'evil-replace-state-exit-hook #'my/ml-rebuild-heavy)
(my/ml-sync-state)

(defun my/ml-live-parts (detailed)
  (let ((max (point-max))
        (pt (point)))
    (list (when-let ((id (ignore-errors (my/buffer-id))))
            (cons 'id (format "#%d" id)))
          (and (buffer-modified-p) (cons 'modified "[+]"))
          (and buffer-read-only (cons 'ro "[RO]"))
          (when (region-active-p)
            (cons 'selected
                  (format "%d selected"
                          (abs (- (region-end) (region-beginning))))))
          (cons 'line
                (format "Ln %d, Col %d"
                        (or (ignore-errors (line-number-at-pos nil t)) 1)
                        (1+ (current-column))))
          (and detailed (cons 'pos (format "Pos %d" pt)))
          (cons 'percent
                (cond ((<= max 1) "0%")
                      ((= pt 1) "Top")
                      ((>= pt max) "Bot")
                      (t (format "%d%%" (/ (* 100 pt) max)))))
          (cons 'state my/ml-state))))

(defun my/ml-tagged (detailed)
  "Status fields. A narrow window keeps the ones you still need."
  (delq nil
        (append
         (if detailed
             (list (cons 'branch (my/ml-part 'branch))
                   (cons 'problems (my/ml-part 'problems))
                   (cons 'dir (my/ml-part 'dir))
                   (cons 'file (my/ml-part 'file))
                   (cons 'size (my/ml-part 'size))
                   (cons 'lines (my/ml-part 'lines))
                   (cons 'indent (my/ml-part 'indent))
                   (cons 'wrap (my/ml-part 'wrap))
                   (cons 'narrow (my/ml-part 'narrow))
                   (cons 'overwrite (my/ml-part 'overwrite))
                   (cons 'encoding (my/ml-part 'encoding))
                   (cons 'eol (my/ml-part 'eol))
                   (cons 'lang (my/ml-part 'lang)))
           (list (cons 'branch (my/ml-part 'branch))
                 (cons 'problems (my/ml-part 'problems))
                 (cons 'file (my/ml-part 'file))
                 (cons 'narrow (my/ml-part 'narrow))
                 (cons 'overwrite (my/ml-part 'overwrite))
                 (cons 'lang (my/ml-part 'lang))))
         (my/ml-live-parts detailed))))

(defun my/ml-fit (tagged room)
  "Join TAGGED fields into at most ROOM columns.
A long file name shrinks first. Number, line, column and mode stay."
  (let* ((items (delq nil (mapcar (lambda (cell)
                                   (and (cdr cell) cell))
                                 tagged)))
         (s (my/ml-join (mapcar #'cdr items)))
         (cell (assq 'file items)))
    (when (and cell (> (string-width s) room))
      (setcdr cell (my/ml-ellipsize
                    (cdr cell)
                    (max 8 (- (string-width (cdr cell))
                              (- (string-width s) room)))))
      (setq s (my/ml-join (mapcar #'cdr items))))
    (let ((drop '(lang percent branch dir size lines indent wrap encoding eol pos)))
      (while (and drop (> (string-width s) room))
        (setq items (assq-delete-all (pop drop) items))
        (setq s (my/ml-join (mapcar #'cdr items)))))
    (let ((w (string-width s)))
      (if (<= w room)
          s
        (truncate-string-to-width s w (- w room))))))

(defun my/ml-status ()
  (unless (or my/ml-parts (minibufferp))
    (my/ml-rebuild-heavy))
  (my/ml-fit (my/ml-tagged (my/ml-detailed-p))
             (max 24 (- (window-width) 2))))

(defun my/ml-rebuild-heavy (&rest _)
  (ignore-errors
    (unless (or (minibufferp) (string-prefix-p " " (buffer-name)))
      (setq my/ml-parts
            (list (cons 'branch (my/ml-branch))
                  (cons 'problems (my/ml-problems))
                  (cons 'dir (my/ml-dir))
                  (cons 'file (my/ml-file))
                  (cons 'size (my/ml-size))
                  (cons 'lines (let ((n (and (<= (buffer-size) 200000)
                                             (line-number-at-pos (point-max) t))))
                                 (and n (format "%d lines" n))))
                  (cons 'indent (my/ml-indent))
                  (cons 'wrap (if truncate-lines "No Wrap" "Wrap"))
                  (cons 'narrow (and (buffer-narrowed-p) "Narrow"))
                  (cons 'overwrite (and (bound-and-true-p overwrite-mode) "Overwrite"))
                  (cons 'encoding (my/ml-encoding))
                  (cons 'eol (my/ml-eol))
                  (cons 'lang (my/ml-lang)))))))

(defun my/ml-on-file ()
  (my/ml-rebuild-heavy))

(defvar my/mode-line-format
  '((:eval (my/mode-line-flush-right)))
  "Status text flush right, no leading colon.")
(put 'my/mode-line-format 'risky-local-variable t)
(setq-default mode-line-format 'my/mode-line-format)
(when (boundp 'mode-line-right-align-edge)
  (setq mode-line-right-align-edge 'window))
(add-hook 'find-file-hook #'my/ml-on-file)
(add-hook 'after-save-hook #'my/ml-rebuild-heavy)
(add-hook 'after-revert-hook #'my/ml-rebuild-heavy)

;;; --- colors ---
(defun my/set-fg (face color)
  (when (facep face)
    (set-face-attribute face nil :foreground color)))

(set-face-attribute 'default nil :foreground "#d6d6dd")
(set-face-attribute 'cursor nil :background "#d6d6dd")
(set-cursor-color "#d6d6dd")
(set-face-attribute 'region nil :background "#163761" :foreground 'unspecified)
(set-face-attribute 'line-number nil :foreground "#535353" :background 'unspecified)
(set-face-attribute 'line-number-current-line nil :foreground "#c2c2c2" :background 'unspecified)
(set-face-attribute 'mode-line nil :foreground "#ffffff" :background "#000000" :box nil)
(set-face-attribute 'mode-line-inactive nil :foreground "#dddddd" :background "#000000" :box nil)
(set-face-attribute 'mode-line-buffer-id nil :foreground "#ffffff" :background "#000000" :weight 'normal)
(set-face-attribute 'vertical-border nil :foreground "#2a2a2a")
(set-face-attribute 'fringe nil :foreground "#ffffff")
(set-face-attribute 'minibuffer-prompt nil :foreground "#ffffff" :background 'unspecified)
(set-face-attribute 'show-paren-match nil :background "#163761" :foreground 'unspecified :weight 'bold)
(set-face-attribute 'show-paren-mismatch nil :foreground "#E34671" :background 'unspecified)
(set-face-attribute 'isearch nil :background "#163761" :foreground 'unspecified)
(set-face-attribute 'lazy-highlight nil :background "#163761" :foreground 'unspecified)
(set-face-attribute 'match nil :background "#163761" :foreground 'unspecified)
(set-face-attribute 'error nil :foreground "#E34671")
(set-face-attribute 'warning nil :foreground "#f8c762")
(set-face-attribute 'success nil :foreground "#82d2ce")
(set-face-attribute 'link nil :foreground "#83d6c5" :underline t)
(set-face-attribute 'link-visited nil :foreground "#e394dc" :underline t)
(set-face-attribute 'escape-glyph nil :foreground "#d6d6dd")
(set-face-attribute 'nobreak-space nil :foreground "#6d6d6d")
(set-face-attribute 'secondary-selection nil :background "#163761")
(set-face-attribute 'trailing-whitespace nil :background "#B80049")
(when (facep 'fill-column-indicator)
  (set-face-attribute 'fill-column-indicator nil :foreground "#2a2a2a"))
(when (facep 'header-line)
  (set-face-attribute 'header-line nil :foreground "#ffffff" :background "#000000" :box nil))
(when (facep 'tooltip)
  (set-face-attribute 'tooltip nil :foreground "#d6d6dd" :background "#1d1d1d"))
(when (facep 'completions-common-part)
  (set-face-attribute 'completions-common-part nil :foreground "#82d2ce"))
(when (facep 'widget-field)
  (set-face-attribute 'widget-field nil :foreground "#d6d6dd" :background "#1d1d1d"))
(when (facep 'button)
  (set-face-attribute 'button nil :foreground "#82d2ce"))

(defun my/prompt-black ()
  (when (or (minibufferp)
            (string-prefix-p " *Echo Area" (buffer-name))
            (string-prefix-p " *Minibuf-" (buffer-name)))
    (setq-local face-remapping-alist
                '((default :foreground "#ffffff" :background unspecified)
                  (minibuffer-prompt :foreground "#ffffff" :background unspecified)))))
(add-hook 'minibuffer-setup-hook #'my/prompt-black)
(add-hook 'minibuffer-inactive-mode-hook #'my/prompt-black)

(defun my/tty-clear-bg (&optional frame)
  (setq frame (or frame (selected-frame)))
  (when (and (frame-live-p frame) (not (display-graphic-p frame)))
    (when (fboundp 'xterm--maybe-update-default-face)
      (remove-hook 'after-make-frame-functions #'xterm--maybe-update-default-face))
    (ignore-errors
      (set-terminal-parameter (frame-terminal frame) 'xterm--background-color nil))
    (dolist (face '(default hl-line fringe line-number line-number-current-line minibuffer-prompt))
      (when (facep face)
        (set-face-attribute face frame :background 'unspecified)
        (set-face-attribute face nil :background 'unspecified)))))
(add-hook 'after-make-frame-functions #'my/tty-clear-bg)
(add-hook 'window-setup-hook #'my/tty-clear-bg)
(add-hook 'emacs-startup-hook #'my/tty-clear-bg)
(add-hook 'after-init-hook #'my/tty-clear-bg t)

(set-face-attribute 'font-lock-comment-face nil :foreground "#6d6d6d" :italic t)
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#6d6d6d" :italic t)
(set-face-attribute 'font-lock-doc-face nil :foreground "#6d6d6d" :italic t)
(dolist (pair '((font-lock-string-face . "#e394dc")
                (font-lock-keyword-face . "#83d6c5")
                (font-lock-builtin-face . "#82d2ce")
                (font-lock-function-name-face . "#efb080")
                (font-lock-variable-name-face . "#d6d6dd")
                (font-lock-type-face . "#87c3ff")
                (font-lock-constant-face . "#ebc88d")
                (font-lock-preprocessor-face . "#a8cc7c")
                (font-lock-warning-face . "#E34671")
                (font-lock-negation-char-face . "#d6d6dd")
                (font-lock-number-face . "#ebc88d")
                (font-lock-escape-face . "#d6d6dd")
                (font-lock-property-name-face . "#AA9BF5")
                (font-lock-property-use-face . "#AA9BF5")
                (font-lock-operator-face . "#d6d6dd")
                (font-lock-bracket-face . "#d6d6dd")
                (font-lock-delimiter-face . "#d6d6dd")
                (font-lock-punctuation-face . "#898989")
                (font-lock-misc-punctuation-face . "#898989")
                (font-lock-regexp-face . "#d6d6dd")
                (font-lock-regexp-grouping-backslash . "#d6d6dd")
                (font-lock-regexp-grouping-construct . "#83d6c5")
                (font-lock-doc-markup-face . "#a8cc7c")))
  (my/set-fg (car pair) (cdr pair)))

(with-eval-after-load 'web-mode
  (my/set-fg 'web-mode-html-tag-face "#87c3ff")
  (my/set-fg 'web-mode-html-attr-name-face "#aaa0fa")
  (my/set-fg 'web-mode-html-attr-value-face "#e394dc")
  (my/set-fg 'web-mode-keyword-face "#83d6c5")
  (my/set-fg 'web-mode-string-face "#e394dc")
  (my/set-fg 'web-mode-function-name-face "#efb080")
  (my/set-fg 'web-mode-type-face "#87c3ff")
  (set-face-attribute 'web-mode-comment-face nil :foreground "#6d6d6d" :italic t)
  (my/set-fg 'web-mode-constant-face "#ebc88d")
  (my/set-fg 'web-mode-variable-name-face "#d6d6dd"))

(with-eval-after-load 'markdown-mode
  (my/set-fg 'markdown-header-face "#d6d6dd")
  (my/set-fg 'markdown-inline-code-face "#e394dc")
  (my/set-fg 'markdown-code-face "#e394dc")
  (my/set-fg 'markdown-link-face "#83d6c5")
  (my/set-fg 'markdown-url-face "#aaa0fa")
  (my/set-fg 'markdown-bold-face "#f8c762")
  (set-face-attribute 'markdown-italic-face nil :foreground "#83d6c5" :italic t))

(with-eval-after-load 'rainbow-delimiters
  (my/set-fg 'rainbow-delimiters-depth-1-face "#82d2ce")
  (my/set-fg 'rainbow-delimiters-depth-2-face "#efb080")
  (my/set-fg 'rainbow-delimiters-depth-3-face "#aa9bf5")
  (my/set-fg 'rainbow-delimiters-depth-4-face "#87c3ff")
  (my/set-fg 'rainbow-delimiters-depth-5-face "#e394dc")
  (my/set-fg 'rainbow-delimiters-depth-6-face "#a8cc7c")
  (my/set-fg 'rainbow-delimiters-depth-7-face "#f8c762"))

(with-eval-after-load 'corfu
  (when (facep 'corfu-default)
    (set-face-attribute 'corfu-default nil :foreground "#d6d6dd" :background "#1d1d1d")
    (set-face-attribute 'corfu-current nil :foreground "#d6d6dd" :background "#163761")))

(when (facep 'diff-added)
  (set-face-attribute 'diff-added nil :foreground 'unspecified :background "#3FA266")
  (set-face-attribute 'diff-removed nil :foreground 'unspecified :background "#B80049")
  (set-face-attribute 'diff-changed nil :foreground 'unspecified :background "#D2943E")
  (set-face-attribute 'diff-refine-added nil :foreground 'unspecified :background "#3FA266"))

(dolist (pair '((flymake-error . "#E34671")
                (flymake-warning . "#f8c762")
                (flymake-note . "#82D2CE")))
  (when (facep (car pair))
    (set-face-attribute (car pair) nil :underline `(:style wave :color ,(cdr pair)))))

(defun my/runtime-gc ()
  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.2)
  (when (boundp 'my/--file-name-handler-alist)
    (setq file-name-handler-alist my/--file-name-handler-alist)))
(add-hook 'after-init-hook #'my/runtime-gc)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "NORMAL  :help 查看全部命令和语言")))

(provide 'init)
