;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/el-get-init-files/init-howm.el
;; Last modified: 2017/06/10 09:56:16

;; ------------------------------------------------------------------------
;; howm

(when (require 'howm nil 'noerror)
  ;; メモファイル置場
  (setq my:memo-directory "~/howm_memos")

  ;; メニューのファイル名を指定
  (setq howm-menu-top nil)
  ;; 場所変更
  (setq howm-directory (concat my:memo-directory "/howm"))
  ;; メニューのファイル名を変更
  (setq howm-menu-file (concat howm-directory "/menu.howm"))
  ;; キーワードを保存するファイルを変更
  (setq howm-keyword-file (concat howm-directory "/.keys"))

  ;; リンクを TAB で辿る
  (eval-after-load "howm-mode"
    '(progn
       (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
       (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
  ;; 「最近のメモ」一覧時にタイトル表示
  (setq howm-list-recent-title t)
  ;; 全メモ一覧時にタイトル表示
  (setq howm-list-all-title t)
  ;; メニューを 2 時間キャッシュ
  (setq howm-menu-expiry-hours 2)

  ;; howm の時は auto-fill を有効化
  (add-hook 'howm-mode-on-hook (function (lambda () (auto-fill-mode t))))

  ;; RET でファイルを開く際, 一覧バッファを消す
  ;; C-u RET なら残る
  (setq howm-view-summary-persistent nil)

  ;; 「最近のメモ」の表示数
  (setq howm-menu-recent-num 10)

  ;; メニューの予定表の表示範囲
  ;; 7 日前から
  (setq howm-menu-schedule-days-before 7)
  ;; 14 日後まで
  (setq howm-menu-schedule-days 14)

  ;; howm のファイル名
  ;; 1 メモ 1 ファイル (デフォルト)
  ;;(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
  ;; 1 日 1 ファイル
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

  (setq howm-view-grep-parse-line
        "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
  ;; 検索しないファイルの正規表現
  (setq
   howm-excluded-file-regexp
   "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

  ;; いちいち消すのも面倒なので内容が 0 ならファイルごと削除する
  (if (not (memq 'delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
            (cons 'delete-file-if-no-contents after-save-hook)))
  (defun delete-file-if-no-contents ()
    (when (and
           (buffer-file-name (current-buffer))
           (string-match "\\.howm" (buffer-file-name (current-buffer)))
           (= (point-min) (point-max)))
      (delete-file
       (buffer-file-name (current-buffer)))))

  ;; C-cC-c で保存してバッファをキルする
  ;; <http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer>
  (defun my-save-and-kill-buffer ()
    (interactive)
    (when (and
           (buffer-file-name)
           (string-match "\\.howm"
                         (buffer-file-name)))
      (save-buffer)
      (kill-buffer nil)))
  (eval-after-load "howm"
    '(progn
       (define-key howm-mode-map
         "\C-c\C-c" 'my-save-and-kill-buffer)))

  ;; メニューを自動更新しない
  (setq howm-menu-refresh-after-save nil)
  ;; 下線を引き直さない
  (setq howm-refresh-after-save nil)

  ;; カレンダーを見て日付選択
  (eval-after-load "calendar"
    '(progn
       (define-key calendar-mode-map
         "\C-m" 'my-insert-day)
       (defun my-insert-day ()
         (interactive)
         (let ((day nil)
               (calendar-date-display-form
                '("[" year "-" (format "%02d" (string-to-int month))
                  "-" (format "%02d" (string-to-int day)) "]")))
           (setq day (calendar-date-string
                      (calendar-cursor-to-date t)))
           (exit-calendar)
           (insert day)))))

  ;; SwitchMemoDirectory
  ;; <http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SwitchMemoDirectory>
  (defun my-howm-set-directory (dir &optional keyfile)
    `(lambda ()
       (interactive)
       (setq howm-directory ,dir)
       (when ,keyfile
         (setq howm-keyword-file ,keyfile))
       (setq howm-menu-next-expiry-time (current-time))
       (message "%s" ,dir)))

  ;; 切り替えてメニューを呼ぶ (thx > [[2ch:619]]さん)
  (defun my-howm-switch-directory (dir &optional keyfile)
    (funcall (my-howm-set-directory dir keyfile))
    (howm-menu)
    (howm-menu-refresh))

  (global-set-key "\C-c,1" (my-howm-set-directory howm-directory (concat howm-directory "/.keys")))
  (global-set-key "\C-c,2" (my-howm-set-directory
                            (concat my:memo-directory "/howm_diary")
                            (concat my:memo-directory "/howm_diary/.keys_diary")))
  (global-set-key "\C-c,3" (my-howm-set-directory
                            (concat my:memo-directory "/howm_jobsearch")
                            (concat my:memo-directory "/howm_jobsearch/.keys_jobsearch")))
  )
