;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-powershell.el
;; Last modified: 2013/03/31 18:48:00

;; ------------------------------------------------------------------------
;; Powershell

(when (require 'powershell nil 'noerror)
  (setq powershell-location-of-exe
        (cygpath-u "C:/Windows/SysWOW64/WindowsPowerShell/v1.0/powershell.exe"))
  (add-to-list 'process-coding-system-alist '("powershell.exe$" sjis . sjis))
  )
