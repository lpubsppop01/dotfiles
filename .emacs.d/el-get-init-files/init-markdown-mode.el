;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-markdown-mode.el
;; Last modified: 2013/03/31 18:50:11

;; ------------------------------------------------------------------------
;; markdown-mode

(when (file-exists-p "~/.emacs.d/el-get/markdown-mode")
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)

  (add-hook 'markdown-mode-hook
            '(lambda () (auto-fill-mode t)))

  (add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))
  )

