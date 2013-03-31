;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-html5.el
;; Last modified: 2013/03/31 18:49:48

;; ------------------------------------------------------------------------
;; html5

(when (file-exists-p "~/.emacs.d/el-get/html5")
  (eval-after-load "rng-loc"
    '(add-to-list 'rng-schema-locating-files "~/.emacs.d/el-get/html5/schemas.xml"))
  (require 'whattf-dt)
  )

