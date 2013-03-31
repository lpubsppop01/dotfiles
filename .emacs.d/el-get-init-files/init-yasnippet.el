;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-yasnippet.el
;; Last modified: 2013/03/31 18:50:21

;; ------------------------------------------------------------------------
;; yasnippet

(when (require 'yasnippet nil 'noerror)
  (yas-global-mode 1)

  ;; helm interface
  ;; <https://github.com/sugyan/dotfiles/blob/master/.emacs.d/conf/04-yasnippet.el>
  (eval-after-load "helm-config"
    '(progn
       (defun my-yas/prompt (prompt choices &optional display-fn)
         (let* ((names (loop for choice in choices
                             collect (or (and display-fn (funcall display-fn choice))
                                         choice)))
                (selected (helm-other-buffer
                           `(((name . ,(format "%s" prompt))
                              (candidates . names)
                              (action . (("Insert snippet" . (lambda (arg) arg))))))
                           "*helm yas/prompt*")))
           (if selected
               (let ((n (position selected names :test 'equal)))
                 (nth n choices))
             (signal 'quit "user quit!"))))
       (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
       (define-key helm-command-map (kbd "y") 'yas/insert-snippet)))
  )

