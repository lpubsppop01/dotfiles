;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-js2-mode.el
;; Last modified: 2013/03/31 18:50:04

;; ------------------------------------------------------------------------
;; js2-mode

(when (file-exists-p "~/.emacs.d/el-get/js2-mode")
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  )
