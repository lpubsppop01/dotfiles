;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-popwin.el
;; Last modified: 2013/04/07 00:05:13

;; ------------------------------------------------------------------------
;; popwin

(when (require 'popwin nil 'noerror)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom) ; TODO: 効かない…。
  )

