;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Vim command line: :set and the short command list documented in :help.

(require 'subr-x)
(require 'cl-lib)
(require 'evil)

(defvar my/vim-ft-alist
  '(("python" . python-mode)
    ("py" . python-mode)
    ("ptython" . python-mode)
    ("phyton" . python-mode)
    ("lua" . lua-mode)
    ("c" . c-mode)
    ("h" . c-mode)
    ("cpp" . c++-mode)
    ("c++" . c++-mode)
    ("cc" . c++-mode)
    ("cxx" . c++-mode)
    ("hpp" . c++-mode)
    ("java" . java-mode)
    ("js" . js-mode)
    ("javascript" . js-mode)
    ("jsx" . web-mode)
    ("ts" . typescript-mode)
    ("tsx" . web-mode)
    ("typescript" . typescript-mode)
    ("json" . json-mode)
    ("html" . web-mode)
    ("htm" . web-mode)
    ("css" . css-mode)
    ("xml" . nxml-mode)
    ("sh" . sh-mode)
    ("bash" . sh-mode)
    ("zsh" . sh-mode)
    ("fish" . sh-mode)
    ("elisp" . emacs-lisp-mode)
    ("el" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("lisp" . lisp-mode)
    ("scheme" . scheme-mode)
    ("haskell" . haskell-mode)
    ("hs" . haskell-mode)
    ("hlsl" . c-mode)
    ("fx" . c-mode)
    ("shader" . c-mode)
    ("gdscript" . gdscript-mode)
    ("gd" . gdscript-mode)
    ("renpy" . python-mode)
    ("rpy" . python-mode)
    ("yaml" . yaml-mode)
    ("yml" . yaml-mode)
    ("toml" . conf-toml-mode)
    ("ini" . conf-mode)
    ("conf" . conf-mode)
    ("md" . markdown-mode)
    ("markdown" . markdown-mode)
    ("text" . text-mode)
    ("txt" . text-mode)
    ("ruby" . ruby-mode)
    ("rb" . ruby-mode)
    ("php" . php-mode)
    ("rust" . rust-mode)
    ("rs" . rust-mode)
    ("go" . go-mode)
    ("make" . makefile-mode)
    ("makefile" . makefile-mode)
    ("cmake" . cmake-mode)
    ("sql" . sql-mode)
    ("asm" . asm-mode)
    ("perl" . perl-mode)
    ("pl" . perl-mode)
    ("r" . my-r-mode)
    ("tex" . latex-mode)
    ("latex" . latex-mode)
    ("org" . org-mode)
    ("diff" . diff-mode)
    ("gitcommit" . text-mode))
  "Vim filetype names mapped to Emacs major modes.")

(defun my/vim-eol-type ()
  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
    (pcase eol
      (1 'dos)
      (2 'mac)
      (_ 'unix))))

(defun my/vim-coding-base ()
  (coding-system-base (or buffer-file-coding-system 'utf-8)))

(defun my/vim-has-bom-p ()
  (memq (my/vim-coding-base)
        '(utf-8-with-signature utf-8-sig
          utf-16le-with-signature utf-16be-with-signature
          utf-16-le-with-signature utf-16-be-with-signature)))

(defun my/vim-apply-coding (base &optional eol)
  (let ((eol (or eol (my/vim-eol-type)))
        (sym (if (symbolp base) base (intern base))))
    (unless (coding-system-p sym)
      (user-error "Unknown fileencoding: %s" base))
    (setq buffer-file-coding-system
          (coding-system-change-eol-conversion
           sym
           (pcase eol ('dos 1) ('mac 2) (_ 0))))
    (set-buffer-modified-p t)
    buffer-file-coding-system))

(defun my/vim-parse-encoding (name)
  (pcase (downcase (string-trim name))
    ((or "utf-8" "utf8") 'utf-8)
    ((or "utf-8-bom" "utf8-bom" "utf-8-sig") 'utf-8-with-signature)
    ((or "gbk" "cp936") 'gbk)
    ("gb2312" 'gb2312)
    ("gb18030" 'gb18030)
    ((or "latin1" "latin-1" "iso-8859-1") 'latin-1)
    ((or "utf-16" "utf16") 'utf-16)
    ((or "utf-16le" "utf16le") 'utf-16le)
    ((or "utf-16be" "utf16be") 'utf-16be)
    ((or "" "none") nil)
    (other (intern other))))

(defun my/vim-ft-name ()
  (let* ((mode (symbol-name major-mode))
         (bare (replace-regexp-in-string
                "\\(-ts\\)?-mode\\'" "" mode)))
    (or (car (rassq major-mode my/vim-ft-alist))
        bare)))

(defun my/vim-resolve-ft (name)
  (let* ((name (downcase (string-trim name)))
         (mapped (alist-get name my/vim-ft-alist nil nil #'string=))
         (mode (intern (format "%s-mode" name)))
         (ts (intern (format "%s-ts-mode" name))))
    (cond
     ((and mapped (fboundp mapped)) mapped)
     ((fboundp mode) mode)
     ((fboundp ts) ts)
     (t
      (let* ((keys (mapcar #'car my/vim-ft-alist))
             (best (car (seq-sort-by (lambda (k) (string-distance name k)) #'< keys)))
             (dist (and best (string-distance name best)))
             (mode (and dist (< dist 3)
                        (alist-get best my/vim-ft-alist nil nil #'string=))))
        (if (and mode (fboundp mode))
            mode
          (user-error "Unknown filetype: %s" name)))))))

(defun my/vim-set-ft (name)
  (let ((mode (my/vim-resolve-ft name)))
    (funcall mode)
    (message "filetype=%s (%s)" (my/vim-ft-name) mode)))

(defun my/vim-set-syntax (name)
  (pcase (downcase (or name ""))
    ((or "on" "enable")
     (font-lock-mode 1)
     (message "syntax=on"))
    ((or "off" "disable")
     (font-lock-mode 0)
     (message "syntax=off"))
    (""
     (message "syntax=%s" (if font-lock-mode (my/vim-ft-name) "off")))
    (_
     (font-lock-mode 1)
     (my/vim-set-ft name))))

(defun my/vim-set-bom (on)
  (let ((eol (my/vim-eol-type)))
    (my/vim-apply-coding (if on 'utf-8-with-signature 'utf-8) eol)
    (message "bomb=%s" (if on "1" ""))))

(defun my/vim-indent-locals (width)
  (setq-local tab-width width
              standard-indent width
              c-basic-offset width
              python-indent-offset width
              js-indent-level width
              css-indent-offset width
              lua-indent-level width
              sh-basic-offset width
              ruby-indent-level width)
  (when (boundp 'evil-shift-width)
    (setq-local evil-shift-width width)))

(defun my/vim-query (opt)
  (pcase opt
    ((or "ft" "filetype")
     (message "filetype=%s" (my/vim-ft-name)))
    ((or "syn" "syntax")
     (message "syntax=%s" (if font-lock-mode (my/vim-ft-name) "off")))
    ((or "bom" "bomb")
     (message "bomb=%s" (if (my/vim-has-bom-p) "1" "")))
    ((or "fenc" "fileencoding")
     (message "fileencoding=%s"
              (replace-regexp-in-string
               "\\(-with-signature\\|-sig\\|-unix\\|-dos\\|-mac\\)$" ""
               (symbol-name (my/vim-coding-base)))))
    ((or "ff" "fileformat")
     (message "fileformat=%s" (my/vim-eol-type)))
    ((or "ts" "tabstop")
     (message "tabstop=%s" tab-width))
    ((or "sw" "shiftwidth")
     (message "shiftwidth=%s" (or (and (boundp 'evil-shift-width) evil-shift-width)
                                  tab-width)))
    ((or "et" "expandtab")
     (message "%s" (if indent-tabs-mode "noexpandtab" "expandtab")))
    ((or "nu" "number")
     (message "%s" (if display-line-numbers "number" "nonumber")))
    ((or "rnu" "relativenumber")
     (message "%s" (if (eq display-line-numbers 'relative)
                       "relativenumber" "norelativenumber")))
    ("wrap"
     (message "%s" (if truncate-lines "nowrap" "wrap")))
    ((or "cul" "cursorline")
     (message "%s" (if (bound-and-true-p hl-line-mode) "cursorline" "nocursorline")))
    ((or "ai" "autoindent")
     (message "%s" (if (bound-and-true-p electric-indent-mode)
                       "autoindent" "noautoindent")))
    ("paste"
     (message "%s" (if (bound-and-true-p my/vim-paste) "paste" "nopaste")))
    ((or "hls" "hlsearch")
     (message "%s" (if (bound-and-true-p evil-ex-search-highlight-all)
                       "hlsearch" "nohlsearch")))
    ((or "ic" "ignorecase")
     (message "%s" (if (and (boundp 'evil-ex-search-case)
                            (eq evil-ex-search-case 'insensitive))
                       "ignorecase" "noignorecase")))
    ((or "ro" "readonly")
     (message "%s" (if buffer-read-only "readonly" "noreadonly")))
    ((or "mod" "modified")
     (message "%s" (if (buffer-modified-p) "modified" "nomodified")))
    (_ (user-error "Unknown option: %s" opt))))

(defun my/vim-assign (opt value local)
  (let ((set-fn (if local #'setq-local #'setq)))
    (pcase opt
      ((or "ft" "filetype")
       (if (string-empty-p value)
           (my/vim-query "ft")
         (my/vim-set-ft value)))
      ((or "syn" "syntax")
       (my/vim-set-syntax value))
      ((or "bom" "bomb")
       (my/vim-set-bom (not (member (downcase value) '("" "0" "off" "no" "false")))))
      ((or "fenc" "fileencoding")
       (if (string-empty-p value)
           (my/vim-query "fenc")
         (let ((cs (my/vim-parse-encoding value)))
           (unless cs (user-error "fileencoding= requires a value"))
           (my/vim-apply-coding cs)
           (my/vim-query "fenc"))))
      ((or "ff" "fileformat")
       (let ((eol (pcase (downcase value)
                    ((or "dos" "crlf") 'dos)
                    ((or "mac" "cr") 'mac)
                    ((or "unix" "lf" "") 'unix)
                    (_ (user-error "fileformat must be unix, dos or mac")))))
         (my/vim-apply-coding (my/vim-coding-base) eol)
         (my/vim-query "ff")))
      ((or "ts" "tabstop")
       (let ((n (string-to-number value)))
         (when (< n 1) (user-error "tabstop must be >= 1"))
         (setq-local tab-width n)
         (my/vim-query "ts")))
      ((or "sw" "shiftwidth")
       (let ((n (string-to-number value)))
         (when (< n 1) (user-error "shiftwidth must be >= 1"))
         (my/vim-indent-locals n)
         (my/vim-query "sw")))
      ((or "tw" "textwidth")
       (funcall set-fn 'fill-column (string-to-number value))
       (message "textwidth=%s" fill-column))
      ((or "so" "scrolloff")
       (funcall set-fn 'scroll-margin (string-to-number value))
       (message "scrolloff=%s" scroll-margin))
      (_ (user-error "Cannot assign option: %s" opt)))))

(defun my/vim-bool (opt on)
  (pcase opt
    ((or "et" "expandtab")
     (setq-local indent-tabs-mode (not on))
     (my/vim-query "et"))
    ((or "nu" "number")
     (setq-local display-line-numbers (if on t nil))
     (display-line-numbers-mode (if on 1 -1))
     (my/vim-query "nu"))
    ((or "rnu" "relativenumber")
     (setq-local display-line-numbers (if on 'relative t))
     (display-line-numbers-mode 1)
     (my/vim-query "rnu"))
    ("wrap"
     (setq-local truncate-lines (not on))
     (my/vim-query "wrap"))
    ((or "cul" "cursorline")
     (hl-line-mode (if on 1 -1))
     (my/vim-query "cul"))
    ((or "ai" "autoindent")
     (electric-indent-local-mode (if on 1 -1))
     (my/vim-query "ai"))
    ("paste"
     (setq-local my/vim-paste on)
     (electric-indent-local-mode (if on -1 1))
     (electric-pair-local-mode (if on -1 1))
     (my/vim-query "paste"))
    ((or "hls" "hlsearch")
     (if on
         (ignore-errors (evil-ex-search-activate-hl))
       (ignore-errors (evil-ex-nohighlight)))
     (message "%s" (if on "hlsearch" "nohlsearch")))
    ((or "ic" "ignorecase")
     (setq evil-ex-search-case (if on 'insensitive 'sensitive))
     (my/vim-query "ic"))
    ((or "ro" "readonly")
     (read-only-mode (if on 1 -1))
     (my/vim-query "ro"))
    ((or "mod" "modified")
     (set-buffer-modified-p on)
     (my/vim-query "mod"))
    ((or "bom" "bomb")
     (my/vim-set-bom on))
    ((or "syn" "syntax")
     (font-lock-mode (if on 1 0))
     (my/vim-query "syn"))
    (_ (user-error "Unknown boolean option: %s" opt))))

(defun my/vim-canonical (name)
  (pcase (downcase name)
    ((or "ft" "filetype") "ft")
    ((or "syn" "syntax") "syn")
    ((or "bom" "bomb") "bom")
    ((or "fenc" "fileencoding") "fenc")
    ((or "ff" "fileformat") "ff")
    ((or "ts" "tabstop") "ts")
    ((or "sw" "shiftwidth") "sw")
    ((or "et" "expandtab") "et")
    ((or "nu" "number") "nu")
    ((or "rnu" "relativenumber") "rnu")
    ((or "cul" "cursorline") "cul")
    ((or "ai" "autoindent") "ai")
    ((or "hls" "hlsearch") "hls")
    ((or "ic" "ignorecase") "ic")
    ((or "ro" "readonly") "ro")
    ((or "mod" "modified") "mod")
    ((or "tw" "textwidth") "tw")
    ((or "so" "scrolloff") "so")
    (other other)))

(defun my/vim-show-all ()
  (message
   (mapconcat #'identity
              (list
               (format "filetype=%s" (my/vim-ft-name))
               (format "syntax=%s" (if font-lock-mode (my/vim-ft-name) "off"))
               (format "fileencoding=%s"
                       (replace-regexp-in-string
                        "\\(-with-signature\\|-sig\\|-unix\\|-dos\\|-mac\\)$" ""
                        (symbol-name (my/vim-coding-base))))
               (format "bomb=%s" (if (my/vim-has-bom-p) "1" ""))
               (format "fileformat=%s" (my/vim-eol-type))
               (format "tabstop=%s" tab-width)
               (format "shiftwidth=%s"
                       (or (and (boundp 'evil-shift-width) evil-shift-width)
                           tab-width))
               (if indent-tabs-mode "noexpandtab" "expandtab")
               (if display-line-numbers "number" "nonumber")
               (if truncate-lines "nowrap" "wrap")
               (if (bound-and-true-p hl-line-mode) "cursorline" "nocursorline")
               (if buffer-read-only "readonly" "noreadonly"))
              "  ")))

(defun my/vim-set-token (token local)
  (cond
   ((string-match "\\`\\(no\\|inv\\)\\([a-zA-Z]+\\)\\'" token)
    (let ((flag (match-string 1 token))
          (opt (my/vim-canonical (match-string 2 token))))
      (if (string= flag "no")
          (my/vim-bool opt nil)
        (pcase opt
          ("et" (my/vim-bool opt indent-tabs-mode))
          ("nu" (my/vim-bool opt (not display-line-numbers)))
          ("wrap" (my/vim-bool opt truncate-lines))
          (_ (user-error "Cannot invert: %s" opt))))))
   ((string-match "\\`\\([a-zA-Z]+\\)!\\'" token)
    (my/vim-set-token (concat "inv" (match-string 1 token)) local))
   ((string-match "\\`\\([a-zA-Z]+\\)\\?\\'" token)
    (my/vim-query (my/vim-canonical (match-string 1 token))))
   ((string-match "\\`\\([a-zA-Z]+\\)\\([=:]\\)\\(.*\\)\\'" token)
    (my/vim-assign (my/vim-canonical (match-string 1 token))
                   (match-string 3 token)
                   local))
   ((string-match "\\`[a-zA-Z]+\\'" token)
    (let ((opt (my/vim-canonical token)))
      (pcase opt
        ((or "ft" "syn" "fenc" "ff" "ts" "sw" "tw" "so")
         (my/vim-query opt))
        (_ (my/vim-bool opt t)))))
   (t (user-error "Invalid :set argument: %s" token))))

(evil-define-command my/ex-set (&optional args)
  "Vim-compatible :set"
  :repeat nil
  (interactive "<a>")
  (if (or (null args) (string-blank-p args))
      (my/vim-show-all)
    (dolist (tok (split-string args "[ \t]+" t))
      (my/vim-set-token tok nil))))

(evil-define-command my/ex-setlocal (&optional args)
  "Vim-compatible :setlocal"
  :repeat nil
  (interactive "<a>")
  (if (or (null args) (string-blank-p args))
      (my/vim-show-all)
    (dolist (tok (split-string args "[ \t]+" t))
      (my/vim-set-token tok t))))

(evil-define-command my/ex-cd (&optional dir)
  :repeat nil
  (interactive "<f>")
  (cd (if (and dir (not (string-blank-p dir))) dir "~"))
  (message "%s" default-directory))

(defun my/ex-pwd ()
  (interactive)
  (message "%s" default-directory))

(defun my/ex-reload ()
  (interactive)
  (condition-case err
      (progn (load user-init-file nil t)
             (message "已重新加载 %s" user-init-file))
    (error (message "重新加载失败：%s" err))))

(defvar-local my/return-buffer nil)

(defun my/quit-to-previous ()
  "关掉说明或列表，回到打开它之前的缓冲区。"
  (interactive)
  (let ((ret my/return-buffer)
        (cur (current-buffer)))
    (cond
     ((and ret (buffer-live-p ret) (not (eq ret cur)))
      (switch-to-buffer ret)
      (when (buffer-live-p cur)
        (kill-buffer cur)))
     (t (kill-buffer cur)))))

(defun my/doc-setup (open-fn)
  (setq-local word-wrap t
              truncate-lines nil)
  (local-set-key (kbd "q") #'my/quit-to-previous)
  (when open-fn
    (local-set-key (kbd "RET") open-fn)
    (local-set-key (kbd "<return>") open-fn))
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "q") #'my/quit-to-previous)
    (evil-local-set-key 'motion (kbd "q") #'my/quit-to-previous)
    (when open-fn
      (evil-local-set-key 'normal (kbd "RET") open-fn)
      (evil-local-set-key 'motion (kbd "RET") open-fn)
      (evil-local-set-key 'normal (kbd "<return>") open-fn))))

(defun my/buffer-menu-open ()
  (interactive)
  (beginning-of-line)
  (if (looking-at "[[:space:]]*\\([0-9]+\\)")
      (my/switch-to-id (string-to-number (match-string 1)))
    (message "请把光标放在有编号的那一行")))

(define-derived-mode my/help-mode special-mode "说明"
  "中文使用说明。"
  (my/doc-setup nil))

(define-derived-mode my/buffer-menu-mode special-mode "缓冲区"
  "带编号的缓冲区列表。"
  (my/doc-setup #'my/buffer-menu-open))

(defun my/show-text (name mode text origin)
  (switch-to-buffer (get-buffer-create name))
  (let ((inhibit-read-only t))
    (funcall mode)
    (erase-buffer)
    (insert text)
    (setq-local my/return-buffer origin)
    (display-line-numbers-mode -1))
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (goto-char (point-min)))

(defun my/buffer-menu-text (origin)
  (with-temp-buffer
    (insert "缓冲区列表\n\n")
    (insert "回车  打开这一行\n")
    (insert "q     返回打开列表之前的缓冲区\n")
    (insert "也可在 NORMAL 输入 :b 编号\n\n")
    (insert (format "%-8s%-8s%s\n" "编号" "状态" "名称"))
    (dolist (buf (my/real-buffers))
      (unless (my/doc-buffer-p buf)
        (insert (format "%-8d%-8s%s\n"
                        (my/buffer-id buf)
                        (concat (if (eq buf origin) "%" " ")
                                (if (buffer-modified-p buf) "+" " "))
                        (buffer-name buf)))))
    (insert "\n%  打开这份列表之前所在的缓冲区\n")
    (insert "+  内容已修改，尚未保存\n")
    (insert "状态栏开头的 #数字 就是当前编号。只看编号可以输入 :b\n")
    (buffer-string)))

(defun my/show-buffer-list ()
  (interactive)
  (let ((origin (if (eq major-mode 'my/buffer-menu-mode)
                    my/return-buffer
                  (current-buffer))))
    (my/show-text "*缓冲区*" #'my/buffer-menu-mode
                  (my/buffer-menu-text origin)
                  origin)))

(defun my/region-or (fallback)
  (if (my/region-active-p)
      (cons (region-beginning) (region-end))
    (pcase fallback
      ('line (cons (line-beginning-position) (line-end-position)))
      ('line+ (cons (line-beginning-position) (line-beginning-position 2)))
      (_ (cons (point-min) (point-max))))))

(defun my/cmd-comment ()
  "有选区就切换选区注释，否则切换当前行。"
  (interactive)
  (let ((range (my/region-or 'line)))
    (comment-or-uncomment-region (car range) (cdr range))
    (message "已切换注释")))

(defun my/cmd-indent ()
  "有选区就缩进选区，否则缩进整个缓冲区。"
  (interactive)
  (when (> (buffer-size) 400000)
    (user-error "文件太大，不缩进"))
  (let ((range (my/region-or 'buffer)))
    (indent-region (car range) (cdr range)))
  (message "已按当前语言缩进"))

(defun my/cmd-trim ()
  (interactive)
  (let ((range (my/region-or 'buffer)))
    (delete-trailing-whitespace (car range) (cdr range)))
  (message "已去掉行尾空白"))

(defun my/cmd-sortlines ()
  (interactive)
  (let ((range (my/region-or 'buffer)))
    (sort-lines nil (car range) (cdr range)))
  (message "已按行排序"))

(defun my/cmd-uniq ()
  (interactive)
  (let ((range (my/region-or 'buffer)))
    (delete-duplicate-lines (car range) (cdr range)))
  (message "已去掉重复行"))

(defun my/cmd-case (fn label)
  (let ((range (my/region-or 'line)))
    (funcall fn (car range) (cdr range))
    (message "已改为%s" label)))

(defun my/cmd-upper () (interactive) (my/cmd-case #'upcase-region "大写"))
(defun my/cmd-lower () (interactive) (my/cmd-case #'downcase-region "小写"))
(defun my/cmd-title () (interactive) (my/cmd-case #'capitalize-region "词首大写"))

(defun my/cmd-dup ()
  "有选区就在选区后面再插一份，否则重复当前行。"
  (interactive)
  (let* ((range (my/region-or 'line+))
         (text (buffer-substring (car range) (cdr range))))
    (goto-char (cdr range))
    (insert text)
    (message "已重复")))

(defun my/cmd-rev ()
  (interactive)
  (let ((range (my/region-or 'buffer)))
    (reverse-region (car range) (cdr range)))
  (message "已倒序"))

(defun my/cmd-fill ()
  (interactive)
  (if (my/region-active-p)
      (fill-region (region-beginning) (region-end))
    (fill-paragraph))
  (message "已按宽度 %d 折行" fill-column))

(defun my/cmd-narrow ()
  (interactive)
  (cond
   ((my/region-active-p)
    (narrow-to-region (region-beginning) (region-end))
    (when (and (bound-and-true-p evil-mode) (eq evil-state 'visual))
      (evil-exit-visual-state))
    (message "现在只显示选区。:widen 恢复全文"))
   (t
    (condition-case nil
        (progn
          (narrow-to-defun)
          (message "现在只显示当前函数。:widen 恢复全文"))
      (error (user-error "先选中一块区域，或把光标放在函数里"))))))

(defun my/cmd-widen ()
  (interactive)
  (widen)
  (message "已恢复全文"))

(defun my/cmd-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun my/cmd-path ()
  (interactive)
  (let ((p (or buffer-file-name (expand-file-name default-directory))))
    (kill-new p)
    (message "已复制路径 %s" p)))

(defun my/cmd-rename ()
  (interactive)
  (unless buffer-file-name
    (user-error "这个缓冲区还没有文件。先 :w 路径"))
  (let ((new (expand-file-name (read-file-name "新路径： " nil buffer-file-name))))
    (when (and (file-exists-p new)
               (not (string= (file-truename new)
                             (file-truename buffer-file-name))))
      (unless (y-or-n-p "目标已存在，覆盖吗？ ")
        (user-error "已取消")))
    (rename-file buffer-file-name new t)
    (set-visited-file-name new nil t)
    (message "已改名为 %s" new)))

(defun my/cmd-revert ()
  (interactive)
  (unless buffer-file-name
    (user-error "这个缓冲区没有对应的磁盘文件"))
  (when (and (buffer-modified-p)
             (not (y-or-n-p "放弃未保存的修改，重新读入吗？ ")))
    (user-error "已取消"))
  (revert-buffer t t t)
  (message "已从磁盘重新读入"))

(defun my/cmd-wc ()
  (interactive)
  (if (my/region-active-p)
      (count-words (region-beginning) (region-end))
    (count-words (point-min) (point-max))))

(defun my/cmd-complete ()
  (interactive)
  (completion-at-point))

(defun my/cmd-fold-toggle () (interactive) (my/cmd-fold 'toggle))
(defun my/cmd-fold-show () (interactive) (my/cmd-fold 'show))
(defun my/cmd-fold-hideall () (interactive) (my/cmd-fold 'hideall))
(defun my/cmd-fold-showall () (interactive) (my/cmd-fold 'showall))

(defun my/cmd-fold (action)
  (condition-case err
      (progn
        (hs-minor-mode 1)
        (pcase action
          ('toggle (hs-toggle-hiding))
          ('show (hs-show-block))
          ('hide (hs-hide-block))
          ('showall (hs-show-all))
          ('hideall (hs-hide-all)))
        (message "折叠已更新"))
    (error (message "这里不能折叠"))))

(defun my/cmd-spell ()
  (interactive)
  (condition-case err
      (progn
        (flyspell-mode 'toggle)
        (message (if flyspell-mode "拼写检查：开" "拼写检查：关")))
    (error (message "拼写检查不可用：%s" (error-message-string err)))))

(defun my/cmd-ws ()
  (interactive)
  (whitespace-mode 'toggle)
  (message (if whitespace-mode "空白字符：显示" "空白字符：隐藏")))

(defun my/cmd-clean ()
  (interactive)
  (whitespace-cleanup)
  (message "已清理多余空白"))

(defun my/cmd-bigger ()
  (interactive)
  (text-scale-increase 1)
  (message "字号 %+d" (or text-scale-mode-amount 0)))

(defun my/cmd-smaller ()
  (interactive)
  (text-scale-decrease 1)
  (message "字号 %+d" (or text-scale-mode-amount 0)))

(defun my/cmd-zoom ()
  (interactive)
  (text-scale-set 0)
  (message "字号已恢复"))

(defun my/cmd-rec ()
  (interactive)
  (kmacro-start-macro nil)
  (message "开始记录。做完后输入 :recstop ，再用 :replay 重放"))

(defun my/cmd-recstop ()
  (interactive)
  (condition-case err
      (progn
        (kmacro-end-macro 1)
        (message "宏已记下。:replay 重放一次"))
    (error (message "没有正在记录的宏"))))

(defun my/cmd-replay ()
  (interactive)
  (condition-case err
      (kmacro-call-macro 1)
    (error (message "还没有宏。先 :rec"))))

(defun my/cmd-gcommit ()
  (interactive)
  (vc-next-action nil))

(defun my/cmd-grevert ()
  (interactive)
  (unless buffer-file-name
    (user-error "当前没有文件"))
  (vc-revert))

(defun my/show-languages ()
  (interactive)
  (let ((origin (if (eq major-mode 'my/help-mode)
                    my/return-buffer
                  (current-buffer))))
    (my/show-text "*使用说明*" #'my/help-mode
                  (concat (my/language-help)
                          "\n\n按 q 返回。:help 查看全部命令。\n")
                  origin)))

(defconst my/language-catalog
  '(("Web / 模板"
     ("HTML" "html" ".html .htm")
     ("CSS" "css" ".css")
     ("SCSS" "scss" ".scss")
     ("Sass" "sass" ".sass")
     ("Less" "less" ".less")
     ("Vue" "vue" ".vue")
     ("JavaScript JSX" "jsx" ".jsx")
     ("TypeScript TSX" "tsx" ".tsx")
     ("Svelte" "svelte" ".svelte")
     ("Astro / EJS / Handlebars / Liquid / Twig / Jinja / Blade / Razor / JSP" "html" ".astro .ejs .hbs .mustache .liquid .twig .j2 .jinja .cshtml .razor .jsp .aspx .mjml")
     ("Pug" "pug" ".pug .jade")
     ("Haml" "haml" ".haml")
     ("Slim" "slim" ".slim")
     ("MDX" "md" ".mdx"))
    ("脚本"
     ("JavaScript" "javascript" ".js .mjs .cjs")
     ("TypeScript" "typescript" ".ts")
     ("CoffeeScript" "coffee" ".coffee .cson")
     ("Python" "python" ".py .pyi .pyw")
     ("Cython" "cython" ".pyx .pxd")
     ("Ren'Py" "renpy" ".rpy .rpym  用 Python 高亮，另加 Ren'Py 关键字")
     ("Lua / Luau" "lua" ".lua .luau .moon")
     ("Shell" "sh" ".sh .bash .zsh  以及 #! 解释器")
     ("Fish" "fish" ".fish")
     ("PowerShell" "powershell" ".ps1 .psm1 .psd1")
     ("PHP" "php" ".php .phtml")
     ("Ruby" "ruby" ".rb  以及 Gemfile Rakefile Vagrantfile")
     ("Perl" "perl" ".pl")
     ("Raku" "raku" ".raku .p6")
     ("AWK" "awk" ".awk")
     ("Sed" "sh" ".sed"))
    ("数据 / 配置"
     ("JSON" "json" ".json .jsonc .json5 .jsonl .geojson")
     ("Jsonnet" "jsonnet" ".jsonnet .libsonnet")
     ("YAML" "yaml" ".yml .yaml  以及 Unity .prefab .unity .meta")
     ("TOML" "toml" ".toml Pipfile")
     ("INI" "ini" ".ini .cfg .conf .editorconfig")
     ("Dotenv" "conf" ".env")
     ("XML" "xml" ".xml .svg .plist .csproj .xaml .xsd .ui")
     ("CSV / TSV" "csv" ".csv .tsv")
     ("GraphQL" "graphql" ".graphql .gql")
     ("Protobuf" "proto" ".proto")
     ("Thrift" "thrift" ".thrift")
     ("Avro IDL" "avro" ".avdl")
     ("KDL" "kdl" ".kdl")
     ("Prisma" "prisma" ".prisma"))
    ("系统语言"
     ("C" "c" ".c .h")
     ("C++" "cpp" ".cpp .cc .cxx .hpp .hh")
     ("Objective-C" "objc" ".m .mm")
     ("Java" "java" ".java")
     ("C#" "cs" ".cs .csx")
     ("F#" "fsharp" ".fs .fsi .fsx")
     ("Kotlin" "kotlin" ".kt .kts")
     ("Scala" "scala" ".scala .sbt")
     ("Groovy" "groovy" ".groovy .gradle Jenkinsfile")
     ("Go" "go" ".go go.mod")
     ("Rust" "rust" ".rs .ron")
     ("Zig" "zig" ".zig .zon")
     ("D" "d" ".d .di")
     ("Crystal" "crystal" ".cr")
     ("Nim" "nim" ".nim .nims")
     ("Odin" "odin" ".odin")
     ("Swift" "swift" ".swift")
     ("Dart" "dart" ".dart")
     ("Haxe" "haxe" ".hx")
     ("Vala" "vala" ".vala .vapi")
     ("HolyC" "holyc" ".HC .hc  只高亮关键字")
     ("Hare" "hare" ".ha  只高亮关键字")
     ("Carbon / Jai 没有专用模式" "c" ".jai 按 C 上色"))
    ("函数式"
     ("Haskell" "haskell" ".hs .lhs .cabal")
     ("PureScript" "purescript" ".purs")
     ("Idris" "idris" ".idr .lidr")
     ("Agda" "agda" ".agda .lagda")
     ("Lean" "lean" ".lean")
     ("F*" "fstar" ".fst .fsti")
     ("OCaml" "ocaml" ".ml .mli  以及 dune")
     ("Reason" "reason" ".re .rei")
     ("ReScript" "rescript" ".res .resi")
     ("Standard ML" "sml" ".sml .sig")
     ("Emacs Lisp" "elisp" ".el")
     ("Common Lisp" "lisp" ".lisp .lsp .cl")
     ("Scheme / Racket" "scheme" ".scm .rkt .ss")
     ("Clojure" "clojure" ".clj .cljs .cljc .edn")
     ("Fennel" "fennel" ".fnl")
     ("Hy" "hy" ".hy")
     ("Janet" "janet" ".janet")
     ("LFE" "lfe" ".lfe")
     ("Elixir" "elixir" ".ex .exs")
     ("Erlang" "erlang" ".erl .hrl")
     ("Elm" "elm" ".elm")
     ("Gleam" "gleam" ".gleam")
     ("Nix" "nix" ".nix")
     ("Dhall" "dhall" ".dhall")
     ("Prolog" "prolog" ".pro"))
    ("科学计算"
     ("R" "r" ".R .r")
     ("Julia" "julia" ".jl")
     ("MATLAB / Octave" "matlab" ".m 内容像 MATLAB 时；否则按 Objective-C")
     ("Fortran" "fortran" ".f .f90 .f95")
     ("Gnuplot" "gnuplot" ".gp .plt"))
    ("游戏 / 着色器 / GPU"
     ("GDScript" "gdscript" ".gd")
     ("GLSL" "glsl" ".glsl .vert .frag .geom .comp .gdshader")
     ("HLSL" "hlsl" ".hlsl .hlsli .fx .fxh .usf .ush")
     ("WGSL" "wgsl" ".wgsl")
     ("ShaderLab" "shaderlab" ".shader")
     ("CUDA" "cuda" ".cu .cuh")
     ("OpenCL / Slang" "c" ".cl .slang 按 C 上色")
     ("Metal" "objc" ".metal")
     ("WebAssembly" "wasm" ".wat .wast")
     ("Assembly" "asm" ".s .S .asm .nasm .masm")
     ("VEX" "vex" ".vex .vfl  只高亮关键字")
     ("RSL" "rsl" ".rsl")
     ("AGAL" "agal" ".agal")
     ("Ink" "ink" ".ink")
     ("QML" "qml" ".qml")
     ("OpenSCAD" "scad" ".scad")
     ("Godot 场景" "conf" ".tscn .tres .godot")
     ("Unity YAML" "yaml" ".unity .prefab .anim .meta"))
    ("硬件描述"
     ("Verilog" "verilog" ".v .sv .svh  .v 若像 Coq 则按 Coq")
     ("VHDL" "vhdl" ".vhd .vhdl")
     ("Coq" "coq" ".v 内容是证明时，或 .coq"))
    ("标记 / 文档"
     ("Markdown" "markdown" ".md .markdown .qmd .rmd")
     ("Org" "org" ".org")
     ("reStructuredText" "rst" ".rst .rest")
     ("AsciiDoc" "asciidoc" ".adoc .asciidoc")
     ("LaTeX" "latex" ".tex .sty .cls")
     ("BibTeX" "bib" ".bib")
     ("Gemtext" "gemtext" ".gmi .gemini"))
    ("基础设施"
     ("Dockerfile" "dockerfile" "Dockerfile Containerfile")
     ("CMake" "cmake" "CMakeLists.txt .cmake")
     ("Meson" "meson" "meson.build")
     ("Make" "make" "Makefile")
     ("Terraform" "terraform" ".tf .tfvars")
     ("HCL / Nomad" "hcl" ".hcl .nomad")
     ("Just" "just" "Justfile .just")
     ("Nginx" "nginx" "nginx.conf .nginx")
     ("Apache" "apache" ".htaccess httpd.conf")
     ("systemd" "systemd" ".service .socket .timer .mount")
     ("SQL" "sql" ".sql")
     ("Graphviz" "dot" ".dot .gv")
     ("Mermaid" "mermaid" ".mmd .mermaid")
     ("PlantUML" "plantuml" ".puml")
     ("Solidity" "solidity" ".sol")
     ("Vim script" "vim" ".vim .vimrc")
     ("Git" "git" ".gitignore .gitconfig .gitattributes"))
    ("其它，打开即高亮"
     ("AutoHotkey" "ahk" ".ahk")
     ("AutoIt" "autoit" ".au3")
     ("Gherkin" "gherkin" ".feature")
     ("Rego" "rego" ".rego")
     ("Pkl" "pkl" ".pkl")
     ("PRQL" "prql" ".prql")
     ("PromQL" "promql" ".promql")
     ("Bicep" "bicep" ".bicep")
     ("Ballerina" "ballerina" ".bal")
     ("Chapel" "chapel" ".chpl")
     ("BASIC / VB" "vb" ".vb .bas .vbs")
     ("ERABasic" "erabasic" ".erb 不像 HTML 模板时，以及 .erh")
     ("POV-Ray" "pov" ".pov")
     ("Ceylon" "ceylon" ".ceylon")
     ("Pascal" "pascal" ".pas")
     ("Ada" "ada" ".ada .adb .ads")
     ("Modula-2" "modula2" ".m2")
     ("Forth" "forth" ".fth")
     ("Lisp 家族以外的 jq" "jq" ".jq")))
  "Language name, :set ft= value, and the extensions that select it.")

(defun my/language-help ()
  (concat
   "支持的语言\n"
   "\n"
   "打开文件时按后缀自动选择高亮，不用先输入命令。\n"
   "想改当前缓冲区的语言：:set ft=下面第二列的名字\n"
   "查看当前语言：:set ft?\n"
   "只关高亮、不改语言：:set syntax=off    再打开：:set syntax=on\n"
   "标成「只高亮关键字」的语言没有完整缩进，颜色仍然会上去。\n"
   "\n"
   (mapconcat
    (lambda (group)
      (concat (car group) "\n"
              (mapconcat
               (lambda (row)
                 (format "  %-52s ft=%-12s %s"
                         (nth 0 row) (nth 1 row) (nth 2 row)))
               (cdr group)
               "\n")))
    my/language-catalog
    "\n\n")))

(defun my/help-text ()
  (concat
   (mapconcat
    #'identity
    '("使用说明"
      ""
      "NORMAL 下按 : 打开底部命令栏，输入命令后回车。"
      ":help          这份全文"
      ":help 关键字   只留下包含这个词的行，例如 :help 语言"
      ":languages     只看支持的语言"
      "看完按 q 或 Ctrl+W 回到刚才的缓冲区。"
      ""
      "除了下面两类，所有功能都从这道命令栏进入，没有单独的功能键："
      "  1. INSERT / NORMAL / VISUAL 这些模式键"
      "  2. 复制、粘贴、撤销、保存、打开、查找这些编辑键"
      ""
      "屏幕上始终只有一个缓冲区。命令的结果会换掉当前这一屏，"
      "原来的文件还在，用 :b 编号 切回去。"
      ""
      "────────────────────────────────"
      "1. 启动时的界面"
      "────────────────────────────────"
      ""
      "不带文件打开时，只有一个名为 untitled 的空白缓冲区。"
      "没有欢迎页，没有 *scratch*。状态栏右侧是 NORMAL。"
      "在命令行写了文件名时，直接打开那个文件，不先给空白页。"
      "空白页可以立刻按 i 输入。还没存盘时，Ctrl+S 或 :w 会问路径。"
      ""
      "状态栏靠右。窗口够宽（约 120 列以上）时从左到右是："
      "分支  报错  目录  文件名  大小  行数  缩进  是否折行"
      "Narrow  Overwrite  编码  换行  语言  #编号  [+]已修改"
      "[RO]只读  选区长度  Ln 行, Col 列  Pos 字符位置"
      "文首/文末/百分比  模式名。"
      "窗口较窄时收成简略：分支  报错  文件名  语言"
      "#编号  [+]  [RO]  选区  Ln, Col  百分比  模式名。"
      "再窄也保留编号、文件名、行和列、模式名；有修改、只读、"
      "选区、报错、Narrow、Overwrite 时它们仍会显示。"
      "模式名只用英文：NORMAL INSERT VISUAL VISUAL LINE"
      "VISUAL BLOCK REPLACE OPERATOR。"
      ""
      "────────────────────────────────"
      "2. 模式"
      "────────────────────────────────"
      ""
      "NORMAL"
      "  启动后的模式。字母是命令，不会写进文件。"
      "  i  光标前进入 INSERT"
      "  a  光标后进入 INSERT"
      "  o  下面新开一行并进入 INSERT"
      "  O  上面新开一行并进入 INSERT"
      "  I  行首进入 INSERT"
      "  A  行尾进入 INSERT"
      "  v  进入 VISUAL，逐字选择"
      "  V  进入 VISUAL LINE，整行选择"
      "  R  进入 REPLACE，打字会覆盖原字符"
      "  -  文件管理。打开当前文件所在目录；已经在目录里则进入上一层"
      "  Esc  从 VISUAL 或操作中回到 NORMAL"
      ""
      "INSERT"
      "  正常打字。状态栏显示 INSERT。"
      "  Esc 回到 NORMAL。冒号在这里只是一个字符，不会打开命令栏。"
      "  要下命令，先 Esc，再按 : "
      ""
      "VISUAL / VISUAL LINE / VISUAL BLOCK"
      "  选中的文字可以用 Ctrl+C 复制、Ctrl+X 剪切。"
      "  Ctrl+Shift+V 进入 VISUAL BLOCK（矩形）。"
      "  终端收不到 Ctrl+Shift+V 时，用命令 :block ，再移动光标。"
      "  Esc 取消选择。"
      ""
      "REPLACE"
      "  覆盖写入。Esc 回到 NORMAL。"
      ""
      "NORMAL 里的移动："
      "  h j k l    左 下 上 右。上一行用 k，- 已留给文件管理"
      "  w b e      下一词、上一词、词尾"
      "  0 $        行首、行尾"
      "  gg         文件开头"
      "  G          文件结尾"
      "  20G        第 20 行。命令 :line 20 相同"
      "  Ctrl+U     向上半页"
      "  Ctrl+D     向下半页"
      "  Ctrl+B     向上一整页"
      "  PageUp / PageDown  整页"
      ""
      "NORMAL 里仍可使用的 Vim 编辑："
      "  x   删除光标下的字符"
      "  dd  删除当前行"
      "  yy  复制当前行到 Vim 寄存器"
      "  p   粘贴到光标后"
      "  u   撤销"
      "  Ctrl+R  重做"
      "这些和 Windows 的 Ctrl+C / Ctrl+V / Ctrl+Z 可以混用。"
      ""
      "────────────────────────────────"
      "3. 编辑键"
      "────────────────────────────────"
      ""
      "INSERT 和 NORMAL 都认这些键。"
      ""
      "Ctrl+C   复制。有选区复制选区；没有选区时复制当前行"
      "Ctrl+X   剪切。规则和复制相同"
      "Ctrl+V   粘贴到光标处。有选区则先删掉选区再贴"
      "Ctrl+Z   撤销。停在当前模式，不会跳出 INSERT"
      "Ctrl+Y   重做"
      "Ctrl+Shift+Z  重做"
      "Ctrl+A   全选，并进入 VISUAL"
      "Ctrl+S   保存。untitled 会询问路径"
      "Ctrl+O   打开文件。选目录则列出目录"
      "Ctrl+N   新建 untitled2、untitled3… 并进入 INSERT"
      "Ctrl+W   关闭当前缓冲区。已修改时："
      "         s 保存并关闭，d 不保存，c 取消"
      "Ctrl+F   增量查找，边打边跳"
      "F3       查找下一处"
      "Shift+F3 查找上一处"
      "Ctrl+H   逐处替换。y 替换，n 跳过，! 全部，q 结束"
      "Ctrl+左 / Ctrl+右     按词移动"
      "Ctrl+退格             删掉前一个词"
      "Ctrl+Delete           删掉后一个词"
      "Home / End            行首 / 行尾"
      "Ctrl+Home / Ctrl+End  文件开头 / 结尾"
      "Shift+方向键          按住选择"
      "Ctrl+Shift+左/右      按词选择"
      ""
      "查找进行中：Ctrl+S 下一处，Ctrl+R 上一处，Ctrl+G 结束。"
      "取消当前动作用 Ctrl+G 或 Esc。"
      "Ctrl+G 不是“跳到某行”。跳行用 :line 20 或 20G。"
      ""
      "若退格变成了替换，终端把退格发成了 Ctrl+H。"
      "把终端退格设为 ASCII DEL，替换改用 :replace 。"
      ""
      "命令栏里的键不同，避免把文件关掉："
      "  Ctrl+V  粘贴进命令栏"
      "  Ctrl+C  有选区就复制，没有就取消这条命令"
      "  Ctrl+A  选中已经输入的命令"
      "  Ctrl+W  删除前一个词"
      "  Ctrl+P / Ctrl+N 或上下键  翻历史"
      "  Tab     补全命令名"
      "  Esc 或 Ctrl+G  取消，回到 NORMAL"
      "  命令栏里的 Ctrl+S、Ctrl+O 不会保存或打开。"
      ""
      "────────────────────────────────"
      "4. 命令一览"
      "────────────────────────────────"
      ""
      "文件    :e :w :wa :q :q! :wq :x :qa :wqa :new :enew"
      "        :Ex :Explore :cd :pwd :revert :rename :path :Recent :r"
      "目录    NORMAL 按 -    进入后的按键见第 5 节"
      "缓冲区  :b :bn :bp :bd :ls :buffers :num :close"
      "编辑    :comment :indent :trim :sortlines :uniq"
      "        :upper :lower :title :dup :join :rev :fill"
      "        :retab :date :narrow :widen :wc :undo :redo"
      "查找    :line :replace :s :%s :g :v :noh :occur"
      "        / ? n N"
      "视图    :set :bigger :smaller :zoom :ws :clean :spell"
      "        :fold :unfold :foldall :unfoldall :block"
      "补全宏  :complete :rec :recstop :replay"
      "书签    :bmark :bjump :bmarks :bdel"
      "        :marks :delmarks :jumps :registers  是 Vim 标记"
      "项目    :Find :Rg :Project :Recent :sh :make :Make"
      "        :cnext :cprev :cfirst"
      "代码    :Lsp :LspStop :Def :Ref :Impl :Type :Doc"
      "        :Rename :Format :Action :Diag :Diags :Imenu"
      "        :back :dn :dp"
      "Git     :Git :Gdiff :Gblame :Glog :Gcommit :Grevert"
      "其它    :term :languages :help :messages :Reload :file"
      ""
      "下面按条写每条命令实际做什么。"
      ""
      "────────────────────────────────"
      "5. 文件、目录、退出"
      "────────────────────────────────"
      ""
      ":e 路径"
      "  打开文件。路径是目录时，列出这个目录。"
      "  和 Ctrl+O 相同，但路径直接写在命令后面。"
      ":w"
      "  保存当前文件。"
      ":w 路径"
      "  另存为。之后这个缓冲区就指向新路径。"
      ":wa"
      "  保存所有已修改的缓冲区。"
      ":r 路径"
      "  把文件内容插到光标下面，不切换缓冲区。"
      ":q"
      "  退出编辑器。有未保存的修改时会询问。"
      ":q!"
      "  不保存，直接退出。"
      ":wq  或  :x"
      "  保存并退出。"
      ":qa"
      "  退出。多个缓冲区有修改时会逐个询问。"
      ":wqa"
      "  全部保存后退出。"
      ":new  或  :enew  或  Ctrl+N"
      "  再建一个空白 untitled 。命令和 Ctrl+N 都会进入 INSERT。"
      ":close  或  :bd  或  Ctrl+W"
      "  只关闭当前缓冲区，不退出编辑器。"
      "  已修改：s 保存并关闭，d 丢掉修改，c 取消。"
      ":bd 3"
      "  关闭 3 号。那一个已经修改时，先 :b 3 再关。"
      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"
      ":cd 路径"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"
      ":cd 路径"
      ":cd 路径"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"      ":Ex  或  :Explore"
      "  和 NORMAL 的 - 相同：打开当前文件所在目录，光标停在这个文件上。"
      "  没有文件时打开当前工作目录。"
      "  已经在目录里时，- 进入上一层，光标停在刚才那个目录上。"
      "  这就是 Vim netrw 的文件管理。下面的键只在目录列表里生效。"
      "  本编辑器只有一个窗口，打开文件或子目录都留在当前窗口。"
      ""
      "  -  退格  h          上一层目录"
      "  回车  o  l  t  p  gf   打开文件，或进入子目录"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"
      ":cd 路径"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"
      ":cd 路径"
      "  q                   离开目录，回到打开它之前的缓冲区"
      "  u                   回到刚才看过的目录"
      "  U                   沿 u 的记录往回走"
      "  Ctrl+L  gr          重新读取当前目录"
      "  d                   新建目录，底栏询问名字"
      "  %                   新建文件并打开，底栏询问名字"
      "  D  Delete           删除光标处或已标记的文件，先询问"
      "  R                   重命名。有标记时移动这些文件，底栏询问目标"
      "  x                   用系统默认程序打开"
      "  X                   执行光标处的可执行文件"
      "  c                   把工作目录换成当前目录，相当于 :cd"
      "  i  I                thin 仅文件名 / long 详细信息"
      "  s                   排序在名称、时间、大小之间循环"
      "  r                   正序 / 逆序"
      "  gh  a               显示或隐藏以点开头的文件"
      "  mf                  标记光标下的文件"
      "  mF                  取消这个标记"
      "  mu                  取消全部标记"
      "  mt                  把当前目录设为复制和移动的目标"
      "  mc                  把标记的文件复制到 mt 设下的目录"
      "  mm                  把标记的文件移动到 mt 设下的目录"
      "  j k                 在列表里上下移动"
      "  /                   按文件名查找"
      ""
      "  目录里 v 仍是 VISUAL。要选中多个文件用 mf，不是 v。"
      "  目录外面的 o、i、d、a、R 仍是 Vim 编辑键，不会触发上面的功能。"
      "  目录默认先列出子目录，再列出文件。点文件默认显示。"
      ":cd 路径"
      "  更换工作目录。:cd 后面不写路径则回到家目录。"
      ":pwd"
      "  在底栏显示工作目录。"
      ":revert"
      "  丢掉内存里的修改，从磁盘重新读入当前文件。"
      ":rename"
      "  询问新路径，连磁盘文件一起改名。"
      ":path"
      "  把当前文件的完整路径复制到剪贴板。没有文件时复制工作目录。"
      ":Recent"
      "  打开最近文件列表。回车打开，q 返回。"
      ":file"
      "  在底栏显示当前文件的路径、行数和修改状态。"
      ""
      "────────────────────────────────"
      "6. 缓冲区编号"
      "────────────────────────────────"
      ""
      "每个文件和 untitled 有一个固定编号，显示在状态栏开头，例如 #3。"
      "关闭之后这个编号不再发给别的缓冲区。"
      "说明页和缓冲区列表本身不出现在 :ls 里。"
      ""
      ":b"
      "  底栏显示当前编号和名称。不切换。"
      ":num"
      "  和 :b 不带参数时相同。"
      ":b 3"
      "  切换到 3 号。编号写在命令后面，中间有空格。"
      ":b 名字"
      "  按名称切换。名称里包含这段文字即可。"
      "  匹配到多个时不切换，改用 :ls 看编号。"
      ":ls  或  :buffers"
      "  打开编号表。% 是打开表之前的那个，+ 是已修改。"
      "  光标移到一行按回车打开。q 返回。"
      ":bn"
      "  下一个编号。"
      ":bp"
      "  上一个编号。"
      ""
      "不能分屏，也没有标签页。:sp :vs :new 以外的分屏命令会拒绝。"
      ":new 在这里的意思是新的空白缓冲区，不是上下分屏。"
      ""
      "────────────────────────────────"
      "7. 编辑命令"
      "────────────────────────────────"
      ""
      "有选区时，这些命令作用在选区上；没有选区时，作用范围写在每条下面。"
      ""
      ":comment"
      "  切换注释。没有选区时只处理当前行。"
      "  注释符号跟语言走，例如 Python 用 # ，C 用 // 。"
      ":indent"
      "  按当前语言重新缩进。没有选区时处理整个缓冲区。"
      "  超过约 400KB 会拒绝。"
      ":trim"
      "  去掉行尾空白。没有选区时处理整个缓冲区。"
      ":clean"
      "  同时去掉行尾空白、文件末尾多余空行，并把孤立空白收干净。"
      ":sortlines"
      "  按整行字母顺序排序。没有选区时排序整个缓冲区。"
      "  Vim 的 :sort 也可以，它认行范围。"
      ":uniq"
      "  相邻的重复行只留一行。想先凑到一起就先 :sortlines 。"
      ":rev"
      "  把行的顺序倒过来。"
      ":upper"
      "  变成大写。没有选区时处理当前行。"
      ":lower"
      "  变成小写。没有选区时处理当前行。"
      ":title"
      "  每个词的首字母大写。没有选区时处理当前行。"
      ":dup"
      "  把选区再贴一份到它后面。没有选区时重复当前行。"
      ":join"
      "  把当前行和下一行并成一行。选了多行就并成一段。"
      ":fill"
      "  把过长的段落折到 textwidth。默认宽度 80，用 :set tw=100 改。"
      "  有选区时折选区。"
      ":retab"
      "  按现在的 tabstop 和 expandtab，把 Tab 和空格互相换算。"
      ":date"
      "  在光标处插入今天的日期和时间，格式 2026-10-27 22:00 。"
      ":narrow"
      "  暂时只显示选区。没有选区时，光标在函数里就只显示这个函数。"
      "  状态栏会出现 Narrow。"
      ":widen"
      "  取消 :narrow ，恢复全文。"
      ":wc"
      "  统计字数、词数和行数。有选区时只统计选区。"
      ":undo"
      "  撤销。Ctrl+Z 和 NORMAL 的 u 也是撤销。"
      ":redo"
      "  重做。Ctrl+Y 和 NORMAL 的 Ctrl+R 也是重做。"
      ":d"
      "  删除当前行。前面加范围可删多行，例如 :1,5d 删除第 1 到第 5 行。"
      ":t 行号"
      "  把当前行复制到指定行下面。"
      ":m 行号"
      "  把当前行移动到指定行下面。"
      ":co 行号"
      "  和 :t 一样，复制当前行。"
      ":y"
      "  把当前行复制进 Vim 寄存器。系统剪贴板用 Ctrl+C 。"
      ":pu"
      "  把最近一次 Vim 复制贴到当前行下面。系统剪贴板用 Ctrl+V 。"
      ":norm 按键"
      "  把一串 NORMAL 按键作用到每一行。例如 :%norm A; 在每行末尾加分号。"
      ""
      "────────────────────────────────"
      "8. 查找、替换、跳转"
      "────────────────────────────────"
      ""
      "/文字"
      "  从光标向下找，回车开始。n 下一处，N 上一处。"
      "?文字"
      "  向上找。"
      ":noh"
      "  去掉搜索留下的高亮，不移动光标。"
      ":line 20"
      "  跳到第 20 行。20G 相同。:20 也会跳到第 20 行。"
      ":s/旧/新"
      "  只换当前行的第一处。"
      ":s/旧/新/g"
      "  换当前行的每一处。"
      ":%s/旧/新/g"
      "  换整个缓冲区。先选中再输入 :s/旧/新/g 则只换选区。"
      ":replace"
      "  和 Ctrl+H 一样，一处一处问。y 替换，n 跳过，! 全部，q 结束。"
      ":g/文字/命令"
      "  对每一个匹配行执行命令。例如 :g/TODO/d 删除含 TODO 的行。"
      ":v/文字/命令"
      "  对每一个不匹配的行执行命令。"
      ":occur 文字"
      "  列出当前缓冲区里所有匹配行。回车跳到那一行，q 返回。"
      ""
      "────────────────────────────────"
      "9. :set 选项"
      "────────────────────────────────"
      ""
      ":set"
      "  在底栏一次显示下面这些当前值。"
      ":set ft?"
      "  只看当前语言。把 ? 换成 =名字 就切换语言，名字见文末语言表。"
      ":set ft=python"
      "  把当前缓冲区当成 Python 来高亮和缩进。"
      ":set syntax=off"
      "  关掉颜色。:set syntax=on 按当前语言再打开。"
      ":set nu"
      "  显示行号。:set nonu 隐藏。"
      ":set rnu"
      "  相对行号。:set nornu 改回绝对行号。"
      ":set wrap"
      "  长行折到窗口里。:set nowrap 横向延伸，不折行。"
      ":set et"
      "  缩进用空格。:set noet 改用 Tab 字符。"
      ":set ts=4"
      "  一个 Tab 等于几个空格。数字可以改。"
      ":set sw=4"
      "  每次缩进几个空格，并同时改当前语言的缩进宽度。"
      ":set tw=80"
      "  :fill 使用的行宽。"
      ":set so=5"
      "  光标离窗口上沿或下沿至少留几行。"
      ":set cul"
      "  高亮光标所在行。:set nocul 取消。终端里默认不开。"
      ":set ai"
      "  回车时延续上一行的缩进。:set noai 关闭。"
      ":set paste"
      "  粘贴模式：暂时关掉自动缩进，避免把整段代码越贴越歪。"
      "  :set nopaste 恢复。"
      ":set hls"
      "  搜索高亮。:set nohls 或 :noh 关闭当前这一次高亮。"
      ":set ic"
      "  搜索忽略大小写。:set noic 区分大小写。"
      ":set ro"
      "  只读。:set noro 允许修改。"
      ":set fenc=utf-8"
      "  下次保存使用的编码。还可以写 gbk、gb18030、gb2312、latin1、"
      "  utf-16、utf-16le、utf-16be、utf-8-bom。"
      ":set ff=unix"
      "  换行。unix 是 LF，dos 是 CRLF，mac 是 CR。"
      ":set bom"
      "  保存时带 UTF-8 BOM。:set nobom 不带。"
      ":set mod"
      "  把缓冲区标成已修改。:set nomod 标成未修改，关闭时不再询问。"
      ""
      "────────────────────────────────"
      "10. 视图、折叠、补全、宏、书签"
      "────────────────────────────────"
      ""
      ":bigger"
      "  只把当前缓冲区的字放大一档。"
      ":smaller"
      "  缩小一档。"
      ":zoom"
      "  字号回到默认。"
      ":ws"
      "  显示或隐藏空格、Tab 和行尾。"
      ":spell"
      "  打开或关闭拼写波浪线。系统没有对应词典时会在底栏说明。"
      ":fold"
      "  光标所在的代码块折叠或展开。再输入一次会反过来。"
      ":unfold"
      "  只展开光标所在的块。"
      ":foldall"
      "  折叠缓冲区内所有能识别的块。"
      ":unfoldall"
      "  全部展开。"
      ":block"
      "  进入 VISUAL BLOCK。然后用移动键拉出矩形，再用 Ctrl+C 或 Ctrl+X。"
      ":complete"
      "  按光标前的文字补全。只有一个候选就直接插入；"
      "  有多个候选时，当前窗口会暂时换成候选列表，选完用 :b 编号 回来。"
      ":rec"
      "  开始把按键记成宏。"
      ":recstop"
      "  结束记录。"
      ":replay"
      "  把刚才的宏再做一遍。"
      ":bmark 名字"
      "  把当前位置记成这个名字。名字自己起，例如 :bmark todo"
      ":bjump 名字"
      "  跳回这个书签。文件没开也会打开。"
      ":bmarks"
      "  列出全部书签。回车跳转，q 返回。"
      ":bdel 名字"
      "  删除这个书签。"
      ":marks"
      "  列出 Vim 的单字母位置标记，不是 :bmark 。"
      ":delmarks a"
      "  删除名为 a 的 Vim 标记。"
      ":jumps"
      "  列出最近的跳转位置。"
      ":registers"
      "  列出 Vim 寄存器里复制过的文字。"
      ""
      "────────────────────────────────"
      "11. 项目、编译、终端"
      "────────────────────────────────"
      ""
      "这些命令不会自己启动。结果如果占满窗口，用 :b 编号 回到文件。"
      ""
      ":Find"
      "  在当前项目里按文件名挑选并打开。项目是含 .git 或类似标记的目录。"
      ":Rg 文字"
      "  在项目里搜索这段文字，列出文件和行。"
      ":Project"
      "  换一个项目目录。"
      ":sh 命令"
      "  运行一条 shell 命令，输出显示在当前窗口。例如 :sh ls -la"
      "  看完用 :b 编号 返回。这不是交互终端。"
      ":term"
      "  打开交互终端，shell 用环境变量 SHELL。"
      ":make"
      "  在当前目录运行编译命令，默认 make。可以改成 :make cargo build 这类"
      "  需要你在提示里输入的完整命令。"
      ":Make"
      "  用项目自己的编译命令。"
      ":cnext"
      "  跳到下一条编译错误。"
      ":cprev"
      "  上一条编译错误。"
      ":cfirst"
      "  第一条编译错误。"
      ""
      "────────────────────────────────"
      "12. 语言服务和 Git"
      "────────────────────────────────"
      ""
      "语言服务不会自动连上。先 :Lsp ，成功后再用定义、重命名和格式化。"
      "机器上要有对应的语言服务器，例如 Python 的 pylsp 或 pyright，"
      "Rust 的 rust-analyzer，Go 的 gopls，C/C++ 的 clangd，"
      "JavaScript/TypeScript 的 typescript-language-server，Lua 的 lua-language-server。"
      "没有可执行文件时，:Lsp 会说明当前语言没有服务器。"
      ""
      ":Lsp"
      "  为当前语言启动服务器。"
      ":LspStop"
      "  断开当前缓冲区的服务器。"
      ":Def"
      "  跳到光标下符号的定义。"
      ":Ref"
      "  列出引用。回车跳转。"
      ":Impl"
      "  跳到实现。"
      ":Type"
      "  跳到类型定义。"
      ":Doc"
      "  显示光标下符号的说明。Emacs Lisp 会打开函数说明。"
      ":back"
      "  从 :Def :Ref :Impl :Type 跳回去。"
      ":Rename"
      "  询问新名字，并改掉这个项目里的引用。需要 :Lsp 已经连上。"
      ":Format"
      "  格式化整个缓冲区。服务器连着就用服务器的格式；否则按语言缩进。"
      "  超过约 400KB 会拒绝。"
      ":Action"
      "  弹出光标处的代码动作，例如补充 import。需要 :Lsp 。"
      ":Diag"
      "  列出当前文件的错误、警告和提示。"
      ":Diags"
      "  列出整个项目的诊断。"
      ":dn"
      "  光标跳到下一处诊断。"
      ":dp"
      "  上一处诊断。"
      ":Imenu"
      "  按函数、类和标题跳转。不需要语言服务器。"
      ""
      ":Git"
      "  打开版本库状态。回车看文件，q 返回。"
      ":Gdiff"
      "  当前文件和版本库的差异。"
      ":Gblame"
      "  每一行最后是谁改的。"
      ":Glog"
      "  当前文件的提交记录。"
      ":Gcommit"
      "  按提示登记或提交。第一次通常是登记当前文件，再执行一次才提交。"
      ":Grevert"
      "  放弃当前文件相对版本库的修改。会先询问。"
      ""
      "────────────────────────────────"
      "13. 说明和配置"
      "────────────────────────────────"
      ""
      ":help"
      "  打开本说明。"
      ":help 折叠"
      "  只显示含有“折叠”的行。其它词同样可以。"
      ":languages"
      "  只打开语言表。"
      ":messages"
      "  打开底栏曾经显示过的消息。"
      ":Reload"
      "  重新读入这份 Emacs 配置。已打开的文件还在。"
      ""
      "不能做的事："
      "  不能把这一屏拆成两个窗口。:sp 和 :vs 会直接拒绝。"
      "  没有标签栏。"
      "  Ctrl+C 只表示复制，不是一组前缀命令。"
      ""
      "────────────────────────────────"
      "14. 语言"
      "────────────────────────────────"
      "")
    "\n")
   "\n"
   (my/language-help)
   "\n"))

(defun my/help-filter (text query)
  (let* ((q (string-trim query))
         (lines (split-string text "\n"))
         (hits (cl-remove-if-not
                (lambda (line)
                  (string-match-p (regexp-quote q) line))
                lines)))
    (if hits
        (concat (format "和「%s」有关的说明：\n\n" q)
                (mapconcat #'identity hits "\n")
                "\n\n输入 :help 可查看全文。按 q 返回。\n")
      (concat (format "说明里没有「%s」。下面是全文。\n\n" q) text))))

(defun my/show-help (&optional arg)
  (interactive)
  (let* ((full (my/help-text))
         (text (if (and arg (not (string-blank-p arg)))
                   (my/help-filter full arg)
                 full))
         (origin (if (eq major-mode 'my/help-mode)
                     my/return-buffer
                   (current-buffer))))
    (my/show-text "*使用说明*" #'my/help-mode text origin)))

(defun my/no-split ()
  (interactive)
  (user-error "单窗口模式，不能分屏。用 :ls 看编号，再用 :b 编号 切换"))

(evil-define-command my/ex-help (&optional args)
  "打开中文使用说明。参数用来筛选相关行。"
  :repeat nil
  (interactive "<a>")
  (my/show-help args))

(evil-define-command my/ex-buffer (&optional args)
  "无参数时显示当前缓冲区编号；有参数时按编号或名称切换。"
  :repeat nil
  (interactive "<a>")
  (my/switch-buffer args))

(evil-define-command my/ex-bnext (&optional _args)
  :repeat nil
  (interactive "<a>")
  (my/cycle-buffer 1))

(evil-define-command my/ex-bprev (&optional _args)
  :repeat nil
  (interactive "<a>")
  (my/cycle-buffer -1))

(evil-define-command my/ex-bdelete (&optional args)
  :repeat nil
  (interactive "<a>")
  (let ((arg (string-trim (or args ""))))
    (cond
     ((string-empty-p arg) (my/close-buffer))
     ((string-match-p "\\`[0-9]+\\'" arg)
      (let* ((n (string-to-number arg))
             (buf (my/buffer-by-id n)))
        (unless (and buf (buffer-live-p buf))
          (user-error "没有编号 %d 的缓冲区" n))
        (if (eq buf (current-buffer))
            (my/close-buffer)
          (when (buffer-modified-p buf)
            (user-error "#%d 已修改。请先 :b %d ，再决定是否保存" n n))
          (kill-buffer buf)
          (message "已关闭 #%d" n))))
     (t (user-error "用法：:bd 或 :bd 编号")))))

(evil-define-command my/ex-buffers (&optional _args)
  :repeat nil
  (interactive "<a>")
  (my/show-buffer-list))

(evil-define-command my/ex-syntax (&optional args)
  :repeat nil
  (interactive "<a>")
  (my/vim-set-syntax (or args "")))

(evil-define-command my/ex-filetype (&optional args)
  :repeat nil
  (interactive "<a>")
  (cond
   ((or (null args) (string-blank-p args))
    (my/vim-query "ft"))
   ((string-match-p "\\`detect\\'" (string-trim args))
    (normal-mode)
    (message "filetype=%s" (my/vim-ft-name)))
   (t (my/vim-set-ft args))))

(defun my/ex-lsp-stop ()
  (interactive)
  (when (and (fboundp 'eglot-shutdown) (fboundp 'eglot-current-server))
    (ignore-errors (eglot-shutdown (eglot-current-server)))
    (message "已停止语言服务")))

(evil-define-command my/ex-line (&optional args)
  :repeat nil
  (interactive "<a>")
  (let ((n (string-to-number (string-trim (or args "")))))
    (when (< n 1)
      (user-error "用法：:line 行号"))
    (goto-line n)
    (message "第 %d 行" n)))

(evil-define-command my/ex-bmark (&optional args)
  :repeat nil
  (interactive "<a>")
  (let ((name (string-trim (or args ""))))
    (when (string-empty-p name)
      (user-error "用法：:bmark 名字"))
    (bookmark-set name)
    (message "已记住位置 %s" name)))

(evil-define-command my/ex-bjump (&optional args)
  :repeat nil
  (interactive "<a>")
  (let ((name (string-trim (or args ""))))
    (when (string-empty-p name)
      (user-error "用法：:bjump 名字"))
    (bookmark-jump name)))

(evil-define-command my/ex-bdel (&optional args)
  :repeat nil
  (interactive "<a>")
  (let ((name (string-trim (or args ""))))
    (when (string-empty-p name)
      (user-error "用法：:bdel 名字"))
    (bookmark-delete name)
    (message "已删除书签 %s" name)))

(evil-define-command my/ex-sh (&optional args)
  :repeat nil
  (interactive "<a>")
  (if (or (null args) (string-blank-p args))
      (user-error "用法：:sh 命令")
    (async-shell-command args)))

(evil-define-command my/ex-occur (&optional args)
  :repeat nil
  (interactive "<a>")
  (if (or (null args) (string-blank-p args))
      (user-error "用法：:occur 文字")
    (occur args)))

(defun my/install-ex-commands ()
  "Register every : command. :help explains each one in Chinese."
  (evil-ex-define-cmd "se[t]" #'my/ex-set)
  (evil-ex-define-cmd "setl[ocal]" #'my/ex-setlocal)
  (evil-ex-define-cmd "setg[lobal]" #'my/ex-set)
  (evil-ex-define-cmd "syn[tax]" #'my/ex-syntax)
  (evil-ex-define-cmd "filet[ype]" #'my/ex-filetype)
  (evil-ex-define-cmd "Ex" #'dired-jump)
  (evil-ex-define-cmd "E[xplore]" #'dired-jump)
  (evil-ex-define-cmd "cd" #'my/ex-cd)
  (evil-ex-define-cmd "pwd" #'my/ex-pwd)
  (evil-ex-define-cmd "Reload" #'my/ex-reload)
  (evil-ex-define-cmd "h[elp]" #'my/ex-help)
  (evil-ex-define-cmd "帮助" #'my/ex-help)
  (evil-ex-define-cmd "b[uffer]" #'my/ex-buffer)
  (evil-ex-define-cmd "bn[ext]" #'my/ex-bnext)
  (evil-ex-define-cmd "bp[revious]" #'my/ex-bprev)
  (evil-ex-define-cmd "bN[ext]" #'my/ex-bprev)
  (evil-ex-define-cmd "bd[elete]" #'my/ex-bdelete)
  (evil-ex-define-cmd "buffers" #'my/ex-buffers)
  (evil-ex-define-cmd "ls" #'my/ex-buffers)
  (evil-ex-define-cmd "num" #'my/show-buffer-number)
  (evil-ex-define-cmd "block" #'my/visual-block)
  (evil-ex-define-cmd "replace" #'my/replace)
  (evil-ex-define-cmd "sp[lit]" #'my/no-split)
  (evil-ex-define-cmd "vs[plit]" #'my/no-split)
  (evil-ex-define-cmd "vne[w]" #'my/no-split)
  (evil-ex-define-cmd "new" #'my/new-file)
  (evil-ex-define-cmd "ene[w]" #'my/new-file)
  (evil-ex-define-cmd "tabnew" #'my/no-split)
  (evil-ex-define-cmd "sb[uffer]" #'my/no-split)
  (evil-ex-define-cmd "sv[iew]" #'my/no-split)
  (evil-ex-define-cmd "res[ize]" #'my/no-split)
  (evil-ex-define-cmd "clo[se]" #'my/close-buffer)
  (evil-ex-define-cmd "com[ment]" #'my/cmd-comment)
  (evil-ex-define-cmd "ind[ent]" #'my/cmd-indent)
  (evil-ex-define-cmd "trim" #'my/cmd-trim)
  (evil-ex-define-cmd "sortlines" #'my/cmd-sortlines)
  (evil-ex-define-cmd "uniq" #'my/cmd-uniq)
  (evil-ex-define-cmd "upper" #'my/cmd-upper)
  (evil-ex-define-cmd "lower" #'my/cmd-lower)
  (evil-ex-define-cmd "title" #'my/cmd-title)
  (evil-ex-define-cmd "dup" #'my/cmd-dup)
  (evil-ex-define-cmd "rev" #'my/cmd-rev)
  (evil-ex-define-cmd "fill" #'my/cmd-fill)
  (evil-ex-define-cmd "narrow" #'my/cmd-narrow)
  (evil-ex-define-cmd "widen" #'my/cmd-widen)
  (evil-ex-define-cmd "date" #'my/cmd-date)
  (evil-ex-define-cmd "path" #'my/cmd-path)
  (evil-ex-define-cmd "rename" #'my/cmd-rename)
  (evil-ex-define-cmd "revert" #'my/cmd-revert)
  (evil-ex-define-cmd "wc" #'my/cmd-wc)
  (evil-ex-define-cmd "line" #'my/ex-line)
  (evil-ex-define-cmd "complete" #'my/cmd-complete)
  (evil-ex-define-cmd "fold" #'my/cmd-fold-toggle)
  (evil-ex-define-cmd "unfold" #'my/cmd-fold-show)
  (evil-ex-define-cmd "foldall" #'my/cmd-fold-hideall)
  (evil-ex-define-cmd "unfoldall" #'my/cmd-fold-showall)
  (evil-ex-define-cmd "spell" #'my/cmd-spell)
  (evil-ex-define-cmd "ws" #'my/cmd-ws)
  (evil-ex-define-cmd "clean" #'my/cmd-clean)
  (evil-ex-define-cmd "bigger" #'my/cmd-bigger)
  (evil-ex-define-cmd "smaller" #'my/cmd-smaller)
  (evil-ex-define-cmd "zoom" #'my/cmd-zoom)
  (evil-ex-define-cmd "rec" #'my/cmd-rec)
  (evil-ex-define-cmd "recstop" #'my/cmd-recstop)
  (evil-ex-define-cmd "replay" #'my/cmd-replay)
  (evil-ex-define-cmd "bmark" #'my/ex-bmark)
  (evil-ex-define-cmd "bjump" #'my/ex-bjump)
  (evil-ex-define-cmd "bmarks" #'bookmark-bmenu-list)
  (evil-ex-define-cmd "bdel" #'my/ex-bdel)
  (evil-ex-define-cmd "sh" #'my/ex-sh)
  (evil-ex-define-cmd "occur" #'my/ex-occur)
  (evil-ex-define-cmd "languages" #'my/show-languages)
  (evil-ex-define-cmd "langs" #'my/show-languages)
  (evil-ex-define-cmd "messages" #'view-echo-area-messages)
  (evil-ex-define-cmd "Gcommit" #'my/cmd-gcommit)
  (evil-ex-define-cmd "Grevert" #'my/cmd-grevert)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'my/help-mode 'normal)
    (evil-set-initial-state 'my/buffer-menu-mode 'normal)))

(provide 'vim-ex)
