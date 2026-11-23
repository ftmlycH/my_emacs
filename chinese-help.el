;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; 中文说明。C-h z 打开。按键以这一份 Emacs 的原生绑定为准。

(require 'button)

(defvar my/chinese-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (define-key map (kbd "q") #'my/help-quit)
    (define-key map (kbd "g") #'my/help-refresh)
    map)
  "按键：q 返回，g 刷新，Tab 在条目间移动，回车跳转。")

(define-derived-mode my/chinese-help-mode special-mode "说明"
  "这份配置的中文说明。"
  (setq-local my/rich-filled t)
  (setq-local revert-buffer-function #'my/help-revert))

(defvar-local my/help-origin nil
  "打开说明之前的缓冲区。")

(defun my/help-revert (&rest _)
  (my/help-render))

(defun my/help-refresh ()
  "按当前配置重新生成说明。"
  (interactive)
  (my/help-render)
  (message "已刷新"))

(defun my/help-quit ()
  "回到打开说明之前的缓冲区。"
  (interactive)
  (let ((origin my/help-origin))
    (quit-window)
    (when (and (buffer-live-p origin)
               (eq major-mode 'my/chinese-help-mode))
      (switch-to-buffer origin))))

(defun my/help-jump (label)
  "跳到标题 LABEL。"
  (goto-char (point-min))
  (if (re-search-forward (concat "^── " (regexp-quote label) " ") nil t)
      (progn
        (beginning-of-line)
        (recenter 0))
    (message "没有这一节：%s" label)))

(defun my/help-mode-symbol (cell)
  "Major-mode symbol stored in one auto-mode-alist entry."
  (let ((tail (cdr cell)))
    (cond ((symbolp tail) tail)
          ((and (consp tail) (symbolp (car tail))) (car tail)))))

(defun my/help-extensions (pattern)
  "Extensions selected by a simple file-name PATTERN, or nil."
  (when (stringp pattern)
    (cond
     ((string-match "\\`\\\\\\.\\([[:alnum:]_+.#-]+\\)\\\\'\\'" pattern)
      (list (match-string 1 pattern)))
     ((string-match "\\`\\\\\\.\\([[:alnum:]_+.#-]*\\)\\[\\([[:alnum:]]+\\)\\]\\?\\\\'\\'" pattern)
      (let ((base (match-string 1 pattern))
            (opts (match-string 2 pattern))
            (out nil))
        (push base out)
        (dotimes (i (length opts))
          (push (concat base (substring opts i (1+ i))) out))
        (nreverse out)))
     ((string-match "\\`\\\\\\.\\\\(\\([[:alnum:]_+.#|-]+\\)\\\\)\\\\'\\'" pattern)
      (split-string (match-string 1 pattern) "|")))))

(defun my/help-dispatcher-p (mode)
  (and (symbolp mode)
       (not (string-match-p "-mode\\'" (symbol-name mode)))))

(defun my/help-language-text ()
  "Text of the extension table, taken from the live auto-mode-alist."
  (let ((seen (make-hash-table :test 'equal))
        (modes nil)
        (dispatch nil))
    (dolist (cell auto-mode-alist)
      (let ((mode (my/help-mode-symbol cell)))
        (when (and mode (or (fboundp mode) (autoloadp (symbol-function mode))))
          (dolist (ext (my/help-extensions (car cell)))
            (unless (gethash ext seen)
              (puthash ext t seen)
              (if (my/help-dispatcher-p mode)
                  (push (cons ext mode) dispatch)
                (let ((bucket (assoc mode modes)))
                  (if bucket
                      (setcdr bucket (cons ext (cdr bucket)))
                    (push (list mode ext) modes)))))))))
    (setq modes (sort modes (lambda (a b)
                              (string-lessp (symbol-name (car a))
                                            (symbol-name (car b))))))
    (with-temp-buffer
      (dolist (row modes)
        (insert (format "  %-28s %s\n"
                        (symbol-name (car row))
                        (mapconcat #'identity
                                   (sort (cdr row) #'string-lessp)
                                   " "))))
      (when dispatch
        (insert "\n下面这些扩展名要看文件内容，才会选定语言：\n\n")
        (dolist (row (sort dispatch (lambda (a b) (string-lessp (car a) (car b)))))
          (insert (format "  .%-12s %s\n" (car row) (cdr row)))))
      (buffer-string))))

(defun my/help-heading (title)
  (insert (format "── %s ──\n\n" title)))

(defun my/help-key (key text)
  (insert (format "  %-18s%s\n" key text)))

(defun my/help-insert ()
  (insert "中文说明\n\n")
  (insert "q 返回     g 刷新     Tab 在上面的词之间移动     回车跳到那一节\n\n")
  (dolist (label '("基础" "编辑" "查找" "窗口" "高亮" "颜色" "缩进" "语言"))
    (let ((label label))
      (insert-text-button label
                          'action (lambda (_button) (my/help-jump label))
                          'follow-link t
                          'face 'link
                          'help-echo "回车跳到这一节")
      (insert "   ")))
  (insert "\n\n随时按 C-h z 可以再打开这份说明。\n")

  (my/help-heading "基础")
  (insert "这是 Emacs 自己的按键。底部那一行是 Emacs 原来的状态栏，右边写着当前语言。\n\n")
  (my/help-key "C-g" "取消正在做的事。不确定时多按几次")
  (my/help-key "C-x C-f" "打开一个文件")
  (my/help-key "C-x C-s" "保存当前文件")
  (my/help-key "C-x C-w" "另存为")
  (my/help-key "C-x C-c" "退出。还有文件没保存时，会先问你")
  (my/help-key "C-x b" "按名字切换缓冲区")
  (my/help-key "C-x C-b" "列出缓冲区。在列表里回车打开那一行")
  (my/help-key "C-x k" "关闭当前缓冲区。磁盘上的文件还在")
  (my/help-key "C-h z" "打开这份中文说明")
  (my/help-key "C-h k" "先按这个，再按另一个键，查看那个键做什么")
  (my/help-key "C-h m" "查看当前语言，以及这个语言自己的按键")
  (my/help-key "C-h f" "按名字查一条命令")
  (insert "\n复制和粘贴用的是 Emacs 自己的记录。\n\n")

  (my/help-heading "编辑")
  (my/help-key "C-f / C-b" "向右一个字 / 向左一个字")
  (my/help-key "C-n / C-p" "下一行 / 上一行")
  (my/help-key "C-a / C-e" "行首 / 行尾")
  (my/help-key "M-f / M-b" "下一个词 / 上一个词")
  (my/help-key "M-< / M->" "文件开头 / 文件结尾")
  (my/help-key "C-v / M-v" "下一屏 / 上一屏")
  (my/help-key "C-l" "把当前行滚到屏幕中间")
  (my/help-key "M-g g" "跳到某一行。接着输入行号，回车")
  (my/help-key "C-SPC" "从这里开始选。再移动光标，选区就出来了")
  (my/help-key "C-g" "取消选区")
  (my/help-key "C-x h" "选中整个缓冲区")
  (my/help-key "C-x C-x" "跳到选区的另一头")
  (my/help-key "C-w" "剪掉选区")
  (my/help-key "M-w" "复制选区")
  (my/help-key "C-y" "粘贴")
  (my/help-key "M-y" "刚粘贴之后按，换成更早剪过的一段")
  (my/help-key "C-k" "剪掉从光标到行尾")
  (my/help-key "C-d" "删掉光标处的一个字")
  (my/help-key "C-/" "撤销")
  (my/help-key "M-x undo-redo" "重做刚才撤销的修改")
  (my/help-key "M-;" "给这一行或选区加注释。再按一次就取消")
  (insert "\nM 是 Alt。按住 Alt 再按后面的键。C 是 Ctrl。\n")

  (my/help-heading "查找")
  (my/help-key "C-s" "向后查找。再按 C-s 找下一个")
  (my/help-key "C-r" "向前查找。再按 C-r 找上一个")
  (my/help-key "RET" "停在找到的地方")
  (my/help-key "C-g" "退出查找，回到开始的位置")
  (my/help-key "M-%" "逐处替换")
  (insert "\n替换时：y 换这一处，n 跳过，! 剩下的全部替换，q 结束。\n")

  (my/help-heading "窗口")
  (my/help-key "C-x 2" "分成上下两个窗口")
  (my/help-key "C-x 3" "分成左右两个窗口")
  (my/help-key "C-x o" "换到另一个窗口")
  (my/help-key "C-x 1" "只留下当前窗口")
  (my/help-key "C-x 0" "关掉当前这个窗口。里面的文件仍然开着")
  (insert "\n关掉窗口不会关掉文件。关文件用 C-x k。\n")

  (my/help-heading "高亮")
  (insert "打开文件就会按扩展名上色，不用再执行别的命令。\n")
  (insert "底部状态栏右边的名字就是当前语言，例如 Python、Java。\n\n")
  (insert "颜色不对，或者整篇都是同一种颜色时，先看状态栏上的语言名。\n")
  (insert "想指定语言，按 M-x，输入那种语言的模式名，再回车。\n")
  (insert "模式名以 -mode 结尾，例如 java-mode、python-mode、c++-mode、rust-mode。\n\n")
  (my/help-key "M-x font-lock-mode" "关掉高亮。再执行一次就重新打开")
  (my/help-key "C-h m" "看当前语言还提供了哪些命令")
  (insert "\n文件大约超过四十万个字符时，会自动少做一些上色，避免打开和滚动卡住。\n")
  (insert "Lisp、Scheme、Racket、Clojure、Fennel、Hy 的括号按嵌套分层上色。\n")
  (insert "一层一个颜色，顺序是粉、橙、黄、绿、蓝、靛、紫，然后循环。\n")
  (insert "对不上的括号是纯红 #FF0000。\n")
  (insert "有的扩展名几种语言都在用，会先看文件内容再决定，例如 .m、.h、.v。\n")

  (my/help-heading "颜色")
  (insert "  关键字、内置名字          #779977\n")
  (insert "  函数                      #BB9955\n")
  (insert "  字符串                    #AA4477\n")
  (insert "  数字                      #FFDD88\n")
  (insert "  常量                      #FF8899\n")
  (insert "  变量                      #7799CC\n")
  (insert "  类型                      #77BBDD\n")
  (insert "  属性                      #7777AA\n")
  (insert "  注释                      #335566\n")
  (insert "  预处理                    #77DD77\n")
  (insert "  括号、普通文字            #FFFFFF\n")
  (insert "  运算符                    #FFFFFF\n")
  (insert "\n字符串和注释里面的数字、关键字保持字符串和注释的颜色。\n")
  (insert "终端背景是透的，不另铺一层底色。\n")

  (my/help-heading "缩进")
  (insert "写代码时，回车会换到下一行，并按当前语言缩进。\n")
  (insert "一般语言用 4 个空格。Makefile 仍用 Tab。\n\n")
  (my/help-key "RET" "换行，并按这种语言缩进")
  (my/help-key "TAB" "把这一行的缩进补齐")
  (my/help-key "C-M-\\" "缩进选区")
  (my/help-key "M-x indent-region" "和 C-M-\\ 相同。终端送不出那个键时用这个")
  (insert "\n*scratch* 缓冲区里的 C-j 会执行光标前面的 Lisp 表达式，那是 Emacs 的草稿页。\n")

  (my/help-heading "语言")
  (insert "下面是现在这份配置能从扩展名认出来的语言。打开说明时现读，所以和编辑器一致。\n")
  (insert "左边是模式名，右边是扩展名。要换成其中一种时，M-x 再输入左边的名字。\n\n")
  (condition-case err
      (insert (my/help-language-text))
    (error (insert (format "语言列表没有生成：%s\n" (error-message-string err)))))
  (insert "\n"))

(defun my/help-render (&optional origin)
  "Rebuild the guide in the current buffer. Keep ORIGIN when given."
  (let ((keep (or origin (bound-and-true-p my/help-origin))))
    (my/chinese-help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (condition-case err
          (my/help-insert)
        (error
         (insert (format "说明没有生成完整：%s\n" (error-message-string err))))))
    (setq-local my/help-origin keep)
    (goto-char (point-min))))

(defun my/chinese-help ()
  "打开中文说明。q 返回刚才的缓冲区。"
  (interactive)
  (let ((origin (unless (eq major-mode 'my/chinese-help-mode)
                  (current-buffer)))
        (buf (get-buffer-create "*中文说明*")))
    (with-current-buffer buf
      (my/help-render origin))
    (switch-to-buffer buf)))

(define-key help-map (kbd "z") #'my/chinese-help)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "C-h z 打开中文说明")))

(provide 'chinese-help)
