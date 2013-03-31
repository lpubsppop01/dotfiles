;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-auto-complete-clang.el
;; Last modified: 2013/03/31 18:48:28

;; ------------------------------------------------------------------------
;; auto-complete-clang

;; TODO: いろいろおかしいので要整備。

(when (require 'auto-complete-clang nil 'noerror)
  (defun my-ac-cc-mode-setup ()
    ;; 読み込むプリコンパイル済みヘッダ
    (setq ac-clang-prefix-header "stdafx.pch")
    ;; 補完を自動で開始しない
    ;; (setq ac-auto-start nil)
    (setq ac-clang-flags '("-w" "-ferror-limit" "1"))
    (setq ac-sources (append '(ac-source-clang
                               ac-source-yasnippet
                               ac-source-gtags)
                             ac-sources)))
  (defun my-ac-config ()
    ;; (global-set-key "\M-/" 'ac-start)
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-css-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t))

  (my-ac-config)
  )
