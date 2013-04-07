;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-js2-mode.el
;; Last modified: 2013/04/07 14:54:06

;; ------------------------------------------------------------------------
;; js2-mode

(when (file-exists-p "~/.emacs.d/el-get/js2-mode")
  (autoload 'js2-mode "js2-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  ;; JSLint 形式の /* global hoge, piyo */ コメントで列挙された名称を js2-additional-externs に追加
  ;; <http://emacswiki.org/emacs/Js2Mode>
  ;; 
  ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
  ;; add any symbols to a buffer-local var of acceptable global vars
  ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
  ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
  ;; you can;t have a symbol called "someName:false"
  (add-hook 'js2-post-parse-callbacks
            (lambda ()
              (when (> (buffer-size) 0)
                (let ((btext (replace-regexp-in-string
                              ": *true" " "
                              (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                  (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                        (split-string
                         (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                         " *, *" t))
                  ))))
  )
