;;; -*- lexical-binding: t; coding: utf-8 -*-
;;; Debian / xfce4-terminal. Fast quiet startup. No GUI chrome.

(defvar my/--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      file-name-handler-alist nil
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      inhibit-compacting-font-caches t
      package-enable-at-startup nil
      package-quickstart t
      native-comp-async-report-warnings-errors 'silent
      warning-minimum-level :error
      frame-resize-pixelwise t
      read-process-output-max (* 1024 1024)
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      inhibit-startup-echo-area-message user-login-name)

;; TTY init runs before init.el. Do not query OSC 11 and do not copy the
;; terminal palette onto cells — that paints an opaque black over xfce4-terminal.
;; Do not set frame background-color; an unknown name becomes solid black.
;; modifyOtherKeys only. getSelection/setSelection query the terminal and
;; wait for an answer xfce4-terminal does not send, which freezes a key.
(setq xterm-extra-capabilities '(modifyOtherKeys)
      inhibit-default-init t)
(advice-add 'xterm--maybe-update-default-face :override #'ignore)

(defun my/early-restore-runtime ()
  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.2)
  (when (boundp 'my/--file-name-handler-alist)
    (setq file-name-handler-alist my/--file-name-handler-alist)))
(add-hook 'emacs-startup-hook #'my/early-restore-runtime)
