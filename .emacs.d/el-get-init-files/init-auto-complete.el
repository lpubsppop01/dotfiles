;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-auto-complete.el
;; Last modified: 2013/03/31 18:48:36

;; ------------------------------------------------------------------------
;; auto-complete

(when (require 'auto-complete-config nil 'noerror)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  ;; 補完ウィンドウで C-p, C-n で候補選択
  (setq ac-use-menu-map t)
  )

