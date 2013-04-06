;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-guide-key.el
;; Last modified: 2013/04/07 00:04:39

;; ------------------------------------------------------------------------
;; guide-key

(when (require 'guide-key nil 'noerror)
  ;; ポップアップしてほしいプレフィックスを登録
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
  (guide-key-mode 1) ; guide-key-mode を有効にする

  ;; 正規表現に当てはまるコマンド名に色をつける例
  ;; (setq guide-key/highlight-command-regexp "rectangle")
  ;; (setq guide-key/highlight-command-regexp "rectangle\\|register")

  ;; 特定のモードに対して設定を追加する例
  ;; (defun guide-key/my-hook-function-for-org-mode ()
  ;;   (guide-key/add-local-guide-key-sequence "C-c")
  ;;   (guide-key/add-local-guide-key-sequence "C-c C-x")
  ;;   (guide-key/add-local-highlight-command-regexp "org-"))
  ;; (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
  )
