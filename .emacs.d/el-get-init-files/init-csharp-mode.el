;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-csharp-mode.el
;; Last modified: 2014/10/05 18:11:16

;; ------------------------------------------------------------------------
;; csharp-mode

(when (require 'csharp-mode nil 'noerror)
  ;; *.csx でも csharp-mode
  (setq auto-mode-alist
        (append '(("\\.csx$" . csharp-mode)) auto-mode-alist))
  )

