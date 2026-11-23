;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Native Emacs. GUI keeps opaque dark background; TTY stays transparent.

(defvar my/--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      file-name-handler-alist nil
      inhibit-compacting-font-caches t
      package-enable-at-startup nil
      package-quickstart t
      native-comp-async-report-warnings-errors 'silent
      warning-minimum-level :error
      frame-resize-pixelwise t
      read-process-output-max (* 1024 1024))

;; TTY init runs before init.el. Do not query OSC 11 and do not copy the
;; terminal palette onto cells — that paints an opaque black over xfce4-terminal.
;; Do not set frame background-color; an unknown name becomes solid black.
;; modifyOtherKeys only. getSelection/setSelection query the terminal and
;; wait for an answer xfce4-terminal does not send, which freezes a key.
(setq xterm-extra-capabilities '(modifyOtherKeys)
      inhibit-default-init t)
(advice-add 'xterm--maybe-update-default-face :override #'ignore)

;; Windows GUI: solid editor background only (Linux TTY stays transparent).
(when (display-graphic-p)
  (setq default-frame-alist
        (append '((background-color . "#181818")
                  (foreground-color . "#FFFFFF"))
                default-frame-alist)))

(defun my/early-restore-runtime ()
  (setq gc-cons-threshold (* 128 1024 1024)
        gc-cons-percentage 0.2)
  (when (boundp 'my/--file-name-handler-alist)
    (setq file-name-handler-alist my/--file-name-handler-alist)))
(add-hook 'emacs-startup-hook #'my/early-restore-runtime)
