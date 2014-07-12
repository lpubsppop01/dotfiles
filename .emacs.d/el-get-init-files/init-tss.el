;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-tss.el
;; Last modified: 2014/07/13 00:04:17

;; ------------------------------------------------------------------------
;; tss

(when (file-exists-p "~/.emacs.d/el-get/tss")
  ;; 以下は emacs-tss の設定例よりコピペ:

  ;; 同梱されたtypescript.elを使う場合
  (require 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  (require 'tss)

  ;; キーバインド
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C->")

  ;; 必要に応じて適宜カスタマイズして下さい。以下のS式を評価することで項目についての情報が得られます。
  ;; (customize-group "tss")

  ;; 推奨設定を行う
  (tss-config-default)
  )

