;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-helm.el
;; Last modified: 2013/03/31 18:49:25

;; ------------------------------------------------------------------------
;; helm

(when (require 'helm nil 'noerror)
  (define-key global-map (kbd "C-x b") 'helm-mini)
  (define-key global-map (kbd "C-c h r") 'helm-resume)
  (define-key global-map (kbd "M-x") 'helm-M-x)

  (helm-mode 0) ; 0: いろいろ無効, 1: いろいろ有効
  )
