;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-jedi.el
;; Last modified: 2013/03/09 22:50:46

;; ------------------------------------------------------------------------
;; emacs-jedi

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

;; jedi:complete-on-dot はこちらでは動作しない
;; (autoload 'jedi:ac-setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)

;; ドットの後で補完候補表示
(setq jedi:complete-on-dot t)

;; 推奨キーバインドを適用
;; (setq jedi:setup-keys t)
