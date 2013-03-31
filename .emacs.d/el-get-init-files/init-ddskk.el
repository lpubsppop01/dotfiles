;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-ddskk.el
;; Last modified: 2013/03/31 18:48:11

;; ------------------------------------------------------------------------
;; ddskk

(when (require 'skk-setup nil 'noerror)
  (setq skk-user-directory "~/.ddskk")

  ;; migemo を使うから skk-isearch にはおとなしくしていて欲しい
  (setq skk-isearch-start-mode 'latin)

  ; 動的自動補完機能
  (setq skk-dcomp-activate t)

  ; 閉じ括弧の自動入力
  (setq skk-auto-insert-paren t)

  ;; インライン候補表示
  (setq skk-show-inline t)

  ;; TODO
  )
