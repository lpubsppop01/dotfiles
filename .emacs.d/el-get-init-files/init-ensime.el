;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-ensime.el
;; Last modified: 2013/03/31 18:48:42

;; ------------------------------------------------------------------------
;; ensime

(when (require 'ensime nil 'noerror)

  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; ドットが入力された想定で補完候補を表示
  (defun my-ensime-dot-complete ()
    (interactive)
    ;; 若干もたつくため非同期実行
    (require 'deferred)
    (deferred:$
      (deferred:next
        (lambda ()
          (insert ".")))
      (deferred:nextc it
        (lambda ()
          (let ((ac-expand-on-auto-complete :expand))
            (auto-complete '(ac-source-ensime-completions)))))))

  ;; ドットの後で補完候補を表示
  (add-hook 'ensime-mode-hook
            '(lambda ()
               (define-key ensime-mode-map "." 'my-ensime-dot-complete)))
  )
