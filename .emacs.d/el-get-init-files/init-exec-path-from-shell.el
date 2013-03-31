;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-exec-path-from-shell.el
;; Last modified: 2013/03/31 18:48:53

;; ------------------------------------------------------------------------
;; exec-path-from-shell

(when (require 'exec-path-from-shell nil 'noerror)
  ;; *NIX の場合は普通に呼び出し
  (when system-type-is-unix-like
    (exec-path-from-shell-initialize))
  ;; Windows の場合は cygpath で変換してから設定
  (when system-type-is-windows-nt
    (let* ((shell-path (exec-path-from-shell-getenv "PATH"))
           (unix-paths (split-string shell-path ":"))
           (windows-paths (mapcar #'cygpath-wml unix-paths)))
      (setq exec-path windows-paths)))
  )
