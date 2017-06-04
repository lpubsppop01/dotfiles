;; -*- mode: emacs-lisp ; coding: utf-8-unix -*-
;; ~/.emacs.d/init.el
;; Last modified: 2017/06/04 20:16:55

;; 想定する環境:
;; * Windows 10 + emacs 25.2
;; 過去に想定していた環境:
;; * Mac OS X EI Capitan + emacs 24.5
;; * Windows 7/8/8.1 + Cygwin emacs-w32 24.3

;; ------------------------------------------------------------------------
;; ユーザー情報

;; (setq user-full-name "")
;; (setq user-mail-address "")

;; ------------------------------------------------------------------------
;; 動作環境の判別

(setq system-type-is-darwin (eq system-type 'darwin)
      system-type-is-gnu-linux (eq system-type 'gnu/linux)
      system-type-is-cygwin (eq system-type 'cygwin)
      system-type-is-windows-nt (eq system-type 'windows-nt)
      system-type-is-unix-like (or system-type-is-darwin system-type-is-gnu-linux
                                   system-type-is-cygwin)
      system-type-is-windows (or system-type-is-cygwin system-type-is-windows-nt))

;; ------------------------------------------------------------------------
;; パス

;; -l オプションの対応 <http://d.hatena.ne.jp/peccu/20130218/trial_init>
;; - locate-user-emacs-file に相対パスを渡すと user-emacs-directory 基準のパスが返る
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Cygwin
(let* ((cygwin-root-directory-auto (concat (getenv "HOME") "/../.."))
       (cygwin-root-directory cygwin-root-directory-auto))
  (when (file-exists-p (concat cygwin-root-directory "/Cygwin.bat"))
    ;; cygpath, uname
    (when system-type-is-windows
      (let ((cygwin-bin-directory (expand-file-name (concat cygwin-root-directory "/bin"))))
        (setq my:cygpath-program (concat cygwin-bin-directory "/cygpath"))
        (defun cygpath-wml (unix-path)
          (let* ((quoted-unix-path (shell-quote-argument unix-path))
                 (command (concat my:cygpath-program " -wml " quoted-unix-path)))
            (substring (shell-command-to-string command) 0 -1)))
        (defun cygpath-u (windows-path)
          (let* ((quoted-windows-path (shell-quote-argument windows-path))
                 (command (concat my:cygpath-program " -u " quoted-windows-path)))
            (substring (shell-command-to-string command) 0 -1)))
        (setq my:uname-program (concat cygwin-bin-directory "/uname"))
        (defun uname-m ()
          (let ((command (concat my:uname-program " -m")))
            (substring (shell-command-to-string command) 0 -1)))))

    ;; Cygwin の環境変数 PATH の内容を exec-path に設定
    (when system-type-is-windows-nt
      (let* ((bash-command (concat cygwin-root-directory "/bin/bash"))
             (bash-args "--login -c 'echo $PATH'")
             (bash-env-path (shell-command-to-string (concat bash-command " " bash-args)))
             (unix-paths (split-string bash-env-path ":"))
             (windows-paths (mapcar #'cygpath-wml unix-paths)))
        (setq exec-path windows-paths)))

    ;; git が動くようにいろいろ設定
    (setenv "LANG" "ja_JP.UTF-8")
    (setenv "SSH_ASKPASS" "/usr/local/bin/win-ssh-askpass.exe")
    ))

;; load-path に ~/.emacs.d/site-lisp を追加
;; normal-top-level-add-subdirs-to-load-path で default-directory を参照しているため一時的に変更
(let ((default-directory (locate-user-emacs-file "site-lisp")))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; exec-path に ~/.emacs.d/bin を追加
(setq exec-path (cons (file-name-as-directory (locate-user-emacs-file "bin")) exec-path))

;; プラットフォーム依存のファイルを格納しているディレクトリパスを設定
(when (and system-type-is-windows (fboundp 'uname-m))
  (let* ((cygwin-platform-is-x86_64 (string-equal "x86_64" (uname-m)))
         (dir-name (if cygwin-platform-is-x86_64 "cygwin64" "cygwin")))
    (setq platform-dependent-directory (file-name-as-directory (locate-user-emacs-file dir-name)))))
(when system-type-is-gnu-linux
  (setq platform-dependent-directory (file-name-as-directory (locate-user-emacs-file "gnu-linux"))))
(when system-type-is-darwin
  (setq platform-dependent-directory (file-name-as-directory (locate-user-emacs-file "darwin"))))

;; "~" を HOME の値に置換
;; * Mac OS X にて、migemo-dictionary が ~ を含むパスだと cmigemo が辞書を見つけられないことがあったため。
(let ((home-path (getenv "HOME")))
  (setq platform-dependent-directory (replace-regexp-in-string "~" home-path platform-dependent-directory)))

;; exec-path に ~/.emacs.d/<platform>/bin を追加
(setq exec-path (cons (concat platform-dependent-directory "bin") exec-path))

;; exec-path を環境変数 PATH に反映
;; shell-command や migemo では exec-path は考慮されないため
(setenv "PATH" (mapconcat 'identity exec-path (if system-type-is-windows-nt ";" ":")))

;; Mac OS X では環境変数 DYLD_FALLBACK_LIBRARY_PATH に ~/.emacs.d/darwin/lib を追加
(when system-type-is-darwin
  (let ((dyld-fallback-library-path (getenv "DYLD_FALLBACK_LIBRARY_PATH"))
        (additinal-directory (concat platform-dependent-directory "/lib")))
    (setenv "DYLD_FALLBACK_LIBRARY_PATH"
            (if dyld-fallback-library-path
                (concat additinal-directory ":" dyld-fallback-library-path)
              additinal-directory))))

;; 環境変数 RUBYLIB に ~/.emacs.d/lib/ruby/site_ruby/1.9.1 を追加
;; (let ((rubylib (getenv "RUBYLIB"))
;;       (additinal-directory (locate-user-emacs-file "lib/ruby/site_ruby/1.9.1")))
;;   (setenv "RUBYLIB" (if rubylib (concat additinal-directory ":" rubylib) additinal-directory)))

;; 環境変数 PYTHONPATH に ~/.emacs.d/lib/python を追加
(let ((pythonpath (getenv "PYTHONPATH"))
      (additinal-directory (locate-user-emacs-file "lib/python")))
  (setenv "PYTHONPATH" (if pythonpath (concat additinal-directory ":" pythonpath) additinal-directory)))

;; ------------------------------------------------------------------------
;; 言語

(set-language-environment "Japanese")
;;(setq grep-command "lgrep -n ")

(when system-type-is-unix-like
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))
(when system-type-is-cygwin
  (set-clipboard-coding-system 'cp932)
  (setq locale-coding-system 'utf-8))

;; ------------------------------------------------------------------------
;; IME

(when system-type-is-windows
  (setq default-input-method "W32-IME")

  ;; IME 状態のモードライン表示
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  (w32-ime-initialize)

  ;; 初期カーソルカラー（IME OFF）
  (set-cursor-color "black")

  ;; IME ON/OFF 時のカーソルカラー
  (add-hook 'input-method-activate-hook
            (lambda() (set-cursor-color "blue")))
  (add-hook 'input-method-inactivate-hook
            (lambda() (set-cursor-color "black")))

  ;; バッファ切り替え時に IME 状態を引き継ぐ
  (setq w32-ime-buffer-switch-p nil)
  )

;; (when system-type-is-cygwin
;;   (require 'w32-ime)
;;   (setq default-input-method "W32-IME"))

(when system-type-is-gnu-linux
  ;; anthy-el
  (load-library "anthy")
  (setq default-input-method "japanese-anthy"))

(when system-type-is-darwin
  ;; ことえり
  (setq default-input-method "MacOSX")

  ;; shift なしの _ を \ にする KeyRemap4MacBook private.xml
  ;; 標準添付の Underscore(Ro) to Backslash(\) では C, M 付きは変換してくれない
  ;; ---
  ;; <?xml version="1.0"?>
  ;; <root>
  ;;   <item>
  ;;     <name>Underscore(Ro) to Backslash(\)</name>
  ;;     <identifier>private.app_excel_command2_to_ctrlu</identifier>
  ;;     <appendix>Underscore(Ro) to Backslash(\)</appendix>
  ;;     <appendix>Control+Underscore(Ro) to Control+Backslash(\)</appendix>
  ;;     <appendix>Option+Underscore(Ro) to Option+Backslash(\)</appendix>
  ;;     <appendix>Control+Option+Underscore(Ro) to Control+Option+Backslash(\)</appendix>
  ;;     <autogen>--KeyToKey-- KeyCode::JIS_UNDERSCORE, ModifierFlag::NONE, KeyCode::VK_JIS_BACKSLASH</autogen>
  ;;     <autogen>--KeyToKey-- KeyCode::JIS_UNDERSCORE, VK_CONTROL, KeyCode::VK_JIS_BACKSLASH, VK_CONTROL</autogen>
  ;;     <autogen>--KeyToKey-- KeyCode::JIS_UNDERSCORE, VK_OPTION, KeyCode::VK_JIS_BACKSLASH, VK_OPTION</autogen>
  ;;     <autogen>--KeyToKey-- KeyCode::JIS_UNDERSCORE, VK_CONTROL | VK_OPTION, KeyCode::VK_JIS_BACKSLASH, VK_CONTROL | VK_OPTION</autogen>
  ;;   </item>
  ;; </root>
  ;; ---
  )

;; ------------------------------------------------------------------------
;; 関数

(defun comment-or-compile (arg)
  "リージョンがあれば comment-region，なければ compile。"
  (interactive "p")
  (if mark-active
      (if (eq arg 1)
          (comment-region (region-beginning) (region-end))
        (uncomment-region (region-beginning) (region-end)))
    (compile compile-command)
    ))

(defun kill-all-buffers-aux (a)
  (cond ((eq a nil))
        ((equal (buffer-name (car a)) "*scratch*")
         (kill-all-buffers-aux (cdr a)))
        (t (kill-buffer (car a))
           (kill-all-buffers-aux (cdr a)))))
(defun kill-all-buffers ()
  "全バッファを閉じる。"
  (interactive)
  (kill-all-buffers-aux (buffer-list))
  (delete-other-windows))

(defun shell-command-replace-region (command)
  "リージョンをシェルコマンドでフィルタリング。"
  (interactive (list (read-shell-command "Shell command replace region: ")))
  (save-excursion
    (shell-command-on-region (point) (mark) command nil t)))

;; ag
;; <http://wata-jp.ldblog.jp/archives/1811534.html>
(defun ag ()
  (interactive)
  (let ((grep-find-command "ag --nocolor --nogroup "))
    (call-interactively 'grep-find)))

;; 大文字小文字を区別しない行ソート
;; <http://stackoverflow.com/questions/20967818/emacs-function-to-case-insensitive-sort-lines>
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun update-user-site-lisp ()
  "~/.emacs.d/site-lisp に配置している elisp ファイルを更新。"
  (interactive)
  (url-copy-file "https://raw.githubusercontent.com/emacs-jp/migemo/master/migemo.el"
                 (locate-user-emacs-file "site-lisp/migemo.el"))
  (url-copy-file "https://raw.githubusercontent.com/knu/elscreen/master/elscreen.el"
                 (locate-user-emacs-file "site-lisp/elscreen/elscreen.el") t)
  (url-copy-file "https://raw.githubusercontent.com/knu/elscreen/master/elscreen-dired.el"
                 (locate-user-emacs-file "site-lisp/elscreen/elscreen-dired.el") t)
  (url-copy-file "https://raw.githubusercontent.com/knu/elscreen/master/elscreen-howm.el"
                 (locate-user-emacs-file "site-lisp/elscreen/elscreen-howm.el") t)
  (url-copy-file "https://raw.githubusercontent.com/knu/elscreen/master/elscreen-server.el"
                 (locate-user-emacs-file "site-lisp/elscreen/elscreen-server.el") t)
  )

;; ------------------------------------------------------------------------
;; 標準機能のキーバインド

;; M-g で goto-line
(global-set-key "\M-g" 'goto-line)

;; C-h でバックスペース
;; (global-unset-key "\C-h")
;; (global-set-key "\C-?" 'help)
;; (global-set-key "\C-h" 'backward-delete-char)

;; C-h でバックスペース（おそらくこちらがベター）
(load "term/bobcat")
(when (fboundp 'terminal-init-bobcat)
  (terminal-init-bobcat))

;; C-, と C-. でバッファの切り替え
(global-set-key [?\C-,] 'next-buffer)
(global-set-key [?\C-.] 'previous-buffer)

;; C-c C-c で comment-or-compile
(global-set-key "\C-c\C-c" 'comment-or-compile)

;; ------------------------------------------------------------------------
;; GUI

(when window-system
  ;; ツールバーを表示しない
  (tool-bar-mode 0)
  ;; メニューバーを表示しない
  (menu-bar-mode 0)
  ;; スクロールバーは右側
  (set-scroll-bar-mode 'right)

  ;; 起動時スプラッシュを表示しない
  (setq inhibit-startup-message t)

  ;; フォントセット定義
  ;; - サイズを指定しない方が DPI に適当に合わせてくれているような？
  ;; - ウィンドウ位置も覚えているようなので default-frame 設定も消してみる
  (create-fontset-from-ascii-font "Migu 1m:weight=normal:slant=normal" nil "migu1m")
  (set-fontset-font "fontset-migu1m" 'ascii (font-spec :family "Migu 1m"))
  (set-fontset-font "fontset-migu1m" 'unicode (font-spec :family "Migu 1m"))
  (set-fontset-font "fontset-migu1m" 'japanese-jisx0208 (font-spec :family "Migu 1m"))
  (set-fontset-font "fontset-migu1m" 'katakana-jisx0201 (font-spec :family "Migu 1m"))
  )

;; ------------------------------------------------------------------------
;; マウス

;; マウス範囲選択でクリップボードにコピー（Emacs23 以前の動きにする）
(setq x-select-enable-clipboard t)
(setq select-active-regions t)
(setq mouse-drag-copy-region t)
(setq x-select-enable-primary t)

(global-set-key [mouse-2] 'mouse-yank-at-click)

;; ------------------------------------------------------------------------
;; 色

;; 選択中のリージョンの色
(set-face-foreground 'region "white")
(set-face-background 'region "lightsteelblue")
;; 余分のスペースやタブのハイライト
(set-face-background 'trailing-whitespace "red")
;; 検索にマッチした文字列のハイライト
(set-face-foreground 'isearch "white")
(set-face-background 'isearch "steelblue")

;; Font Lock の設定
(require 'font-lock)
(global-font-lock-mode t)
(set-face-foreground 'font-lock-comment-face "mediumseagreen")
(set-face-foreground 'font-lock-string-face  "purple")
(set-face-foreground 'font-lock-keyword-face "mediumblue")
(set-face-foreground 'font-lock-function-name-face "mediumblue")
(set-face-bold-p 'font-lock-function-name-face t)
(set-face-foreground 'font-lock-variable-name-face "black")
(set-face-foreground 'font-lock-type-face "lightseagreen")
(set-face-foreground 'font-lock-builtin-face "purple")
(set-face-foreground 'font-lock-constant-face "black")
(set-face-foreground 'font-lock-warning-face "mediumblue")
(set-face-bold-p 'font-lock-warning-face nil)


;; ------------------------------------------------------------------------
;; いろいろ

;; リージョンをハイライト表示
(transient-mark-mode t)

;; モードラインに行番号と列番号の両方を表示
(line-number-mode t)
(column-number-mode t)

;; 行間のスペースを調整
;; (setq-default line-spacing 4)

;; 現在の関数名をモードラインに表示
(which-function-mode 1)

;; 画像ファイルを表示
(auto-image-file-mode)

;; 行番号表示
(global-linum-mode t)
(setq linum-format "%5d ")

;; インデントにタブを用いない
(setq-default indent-tabs-mode nil)
;; インデント幅
(setq c-basic-offset 4)

;; バックアップファイルを指定場所に保存
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (locate-user-emacs-file "backups"))
            backup-directory-alist))

;; デフォルトで無効になっている機能を有効化
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; compile のウインドウサイズ
(setq compilation-window-height 10)
;; compile のウインドウ自動スクロール
(setq compilation-scroll-output t)

;; 括弧の対応表示
(show-paren-mode t)
(when window-system
  ;; 括弧内を強調表示
  (setq show-paren-style 'expression)
  ;; マッチした場合の表示 (ボールド)
  (set-face-bold-p 'show-paren-match-face t)
  (set-face-foreground 'show-paren-match-face nil)
  (set-face-background 'show-paren-match-face nil)
  ;; マッチしていない場合の表示 (赤ボールド)
  (set-face-bold-p 'show-paren-mismatch-face t)
  (set-face-foreground 'show-paren-mismatch-face "red")
  (set-face-background 'show-paren-mismatch-face nil))

;; デバッグ有効
(setq debug-on-error t)

;; ファイル末尾の改行がないとき確認
(setq mode-require-final-newline 0)

;; Cygwin grep で NUL ではなく /dev/null を見るように
(setq null-device "/dev/null")

;; ------------------------------------------------------------------------
;; スクロール

;; 通常のバッファ
;; キャレットを再表示する行数の閾値（これだけ通りすぎると再表示になる）
(setq scroll-conservatively 35)
;; キャレットを上下端に近づけられる行数
(setq scroll-margin 1)
;; 画面から出たときにスクロールさせる行数
(setq scroll-step 1)
;; シェルモード
(setq comint-scroll-show-maximum-output t)

;; 1 画面スクロールの設定
;; この行数だけ以前の表示を残す
(setq next-screen-context-lines 25)

;; ------------------------------------------------------------------------
;; タイムスタンプ

(require 'time-stamp)
(setq time-stamp-active t)

;; フォーマット
(setq time-stamp-start "Last modified: ")
(setq time-stamp-end "$")
(setq time-stamp-format "%:y/%02m/%02d %02H:%02M:%02S")

;; ファイル保存時に更新
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks (cons 'time-stamp write-file-hooks)))

;; ファイル先頭から 25 行まで検索
(setq time-stamp-line-limit 25)
;; ファイル末尾から 5 行まで検索
;; (setq time-stamp-line-limit -5)

;; ------------------------------------------------------------------------
;; ido-mode

;; (require 'ido)
;; (ido-mode t)

;; ------------------------------------------------------------------------
;; el-get

;; インストールに失敗したときの作業が楽なように, ひと通り標準機能の設定が終わった後に実行

;; ~/.emacs.d が mklink で作成したシンボリックリンクである場合に
;; el-get パッケージインストール後の autoload でファイルの比較に失敗する対策
(when (and system-type-is-windows-nt (file-symlink-p (locate-user-emacs-file "")))
  (let* ((base-dir (file-name-directory (locate-user-emacs-file "")))
         (refered-dir-name (file-symlink-p (locate-user-emacs-file "")))
         (true-user-emacs-dir (expand-file-name (file-name-as-directory (concat base-dir refered-dir-name))))
         (true-el-get-dir (concat true-user-emacs-dir (file-name-as-directory "el-get"))))
    (setq el-get-dir true-el-get-dir)
    ))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-local-recipes"))
(setq el-get-user-package-directory (locate-user-emacs-file "el-get-init-files"))

(setq my:el-get-packages
      '(auto-complete
        ;; yasnippet
        ;; jedi
        ;; markdown-mode
        ;; Powershell
        ;; scala-mode2
        helm
        ;; helm-ag
        ;; ag
        ;; auto-complete-clang
        ;; quickrun
        ;; ensime
        howm))

;; (el-get 'sync my:el-get-packages)
;; (el-get 'sync '(tss json-mode)) ; TypeScript
;; (el-get 'sync '(js2-mode js2-refactor ac-js2)) ; JavaScript
;; (el-get 'sync '(csharp-mode))) ; C#
(el-get 'sync)

;; package からレシピ自動生成
;; (el-get-elpa-build-local-recipes)

;; Cygwin git で "git submodule update *" 実行時に
;; "error while loading shared libraries" となった場合は gettext があるか要確認:
;; http://cygwin.1069669.n5.nabble.com/Re-shared-object-file-not-found-with-git-submodule-update-init-recursive-in-Cygwin-64-bit-td104123.html

;; ------------------------------------------------------------------------
;; auto-insert

(require 'autoinsert)

;; テンプレートファイルを置くディレクトリ
;; (setq auto-insert-directory "~/template/")

;; 新規作成ファイルにはテンプレートを挿入する
(add-hook 'find-file-not-found-hooks 'auto-insert)

;; Cocoa Emacs では time-stamp.el がないためここで定義
(when (not (fboundp 'time-stamp-yyyy/mm/dd))
  (defun time-stamp-yyyy/mm/dd ()
    "Return the current date as a string in \"YYYY/MM/DD\" form."
    (format-time-string "%Y/%m/%d")))
(when (not (fboundp 'time-stamp-hh:mm:ss))
  (defun time-stamp-hh:mm:ss ()
    "Return the current time as a string in \"HH:MM:SS\" form."
    (format-time-string "%T")))

(defun c-include-guard-name (filename)
  "C/C++ のインクルードガード名を取得。"
  (let* ((pattern "\\(\\.\\|-\\)")
        (upcase-filename (upcase (file-name-nondirectory filename)))
        (upcase-underscore-separated-filename
         (replace-regexp-in-string pattern  "_" upcase-filename)))
    (concat "__" upcase-underscore-separated-filename "__")))

;; テンプレート定義
(setq auto-insert-alist
      (append
       '((("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\)\\'" . "C/C++ program")
          "C/C++ program: "
          "// -*- coding: utf-8-unix -*-\n"
          "\n"
          "/** @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " *  \n"
          " *  @author " user-full-name "\n"
          " *  Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " *  Last modified: \n"
          " */\n"
          "\n"
          "\n"
          "\n"
          "// " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.[Hh]\\'" . "C header")
          "C header: "
          "// -*- coding: utf-8-unix -*-\n"
          "\n"
          "/** @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " *  \n"
          " *  @author " user-full-name "\n"
          " *  Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " *  Last modified: \n"
          " */\n"
          "\n"
          "#ifndef " (c-include-guard-name (buffer-file-name)) "\n"
          "#define " (c-include-guard-name (buffer-file-name)) "\n"
          "\n"
          "#ifdef __cplusplus\n"
          "extern \"C\" {\n"
          "#endif\n"
          "\n"
          "\n"
          "\n"
          "#ifdef __cplusplus\n"
          "}\n"
          "#endif\n"
          "\n"
          "#endif // " (c-include-guard-name (buffer-file-name)) "\n"
          "\n"
          "// " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.\\(hh\\|hpp\\|hxx\\)\\'" . "C++ header")
          "C++ header: "
          "// -*- coding: utf-8-unix -*-\n"
          "\n"
          "/** @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " *  \n"
          " *  @author " user-full-name "\n"
          " *  Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " *  Last modified: \n" 
          " */\n"
          "\n"
          "#ifndef " (c-include-guard-name (buffer-file-name)) "\n"
          "#define " (c-include-guard-name (buffer-file-name)) "\n"
          "\n"
          "\n"
          "\n"
          "#endif // " (c-include-guard-name (buffer-file-name)) "\n"
          "\n"
          "// " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.java$" . "Java")
          "Java: "
          "// -*- coding: utf-8-unix -*-\n"
          "\n"
          "/** @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " *  \n"
          " *  @author " user-full-name "\n"
          " *  Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " *  Last modified: \n" 
          " */\n"
          "\n"
          "\n"
          "\n"
          "// " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.py$" . "Python")
          "Python: "
          "#! /usr/bin/env python\n"
          "# -*- coding: utf-8-unix -*-\n"
          "# Filename: " (file-name-nondirectory (buffer-file-name)) "\n"
          "# Author: " user-full-name "\n"
          "# Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          "# Last modified:  \n"
          "\n"
          "\"\"\"\n"
          "\n"
          "\"\"\"\n"
          "\n"
          "import sys\n"
          "import os\n"
          "from optparse import OptionParser\n"
          "\n"
          "\n"
          "\n"
          "if __name__ == \"__main__\":\n"
          "    option_parser = OptionParser(usage=\"%prog [options]\",\n"
          "                                 version=\"%prog 0.0\")\n"
          "    (options, args) = option_parser.parse_args()\n"
          "\n"
          "\n"
          "\n"
          "# " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.sh$" . "Shell script")
          "Shell script: "
          "#! /bin/sh\n"
          "# -*- coding: utf-8-unix -*-\n"
          "\n"
          "# Filename: " (file-name-nondirectory (buffer-file-name)) "\n"
          "# Author: " user-full-name "\n"
          "# Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          "# Last modified:  \n"
          "\n"
          "# \n"
          "\n"
          "\n"
          "\n"
          "# " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.js$" . "JavaScript")
          "JavaScript: "
          "// -*- coding: utf-8-unix -*-\n"
          "\n"
          "/** @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " *  \n"
          " *  @author " user-full-name "\n"
          " *  Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " *  Last modified: \n"
          " */\n"
          "\n"
          "\n"
          "\n"
          "// " (file-name-nondirectory (buffer-file-name)) " ends here\n")
         (("\\.css$" . "CSS")
          "CSS: "
          "/*\n"
          " * -*- coding: utf-8-unix -*-\n"
          " * ------------------------------------\n"
          " * File name: " (file-name-nondirectory (buffer-file-name)) "\n"
          " * Author: " user-full-name "\n"
          " * Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          " * Last modified: \n"
          " */\n"
          "\n")
         (("[Mm]akefile" . "Makefile")
          "Makefile: "
          "# -*- coding: euc-jp-unix -*-\n"
          "\n"
          "# Filename: " (file-name-nondirectory (buffer-file-name)) "\n"
          "# Author: " user-full-name "\n"
          "# Created: " (concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss)) "\n"
          "# Last modified: \n"
          "\n"
          "# Description:\n"
          "# \n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Environmental settings\n"
          "\n"
          "BIN_SUFFIX = .exe\n"
          "LIB_PREFIX = lib\n"
          "LIB_SUFFIX = .so\n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Filename settings\n"
          "\n"
          "C_SRCS = hoge.c piyo.c\n"
          "CXX_SRCS = \n"
          "OBJS = $(C_SRCS:.c=.o) $(CXX_SRCS:.cpp=.o)\n"
          "DEPS = $(C_SRCS:.c=.d) $(CXX_SRCS:.cpp=.d)\n"
          "TARGET = hoge$(BIN_SUFFIX)\n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Include path settings\n"
          "\n"
          "C_INCLUDE = -Ifuga -Ihogera\n"
          "CXX_INCLUDE = \n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Command settings\n"
          "\n"
          "CC = gcc\n"
          "CXX = g++\n"
          "LD = g++\n"
          "RM = rm -f\n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Flag settings\n"
          "\n"
          "# for release build\n"
          "CFLAGS = -Wall -Werror -O2 -DNDEBUG\n"
          "CXXFLAGS = -Wall -Werror -O2 -DNDEBUG\n"
          "LDFLAGS = -Wall -Werror -O2\n"
          "\n"
          "# for debug build\n"
          "#CFLAGS = -Wall -g\n"
          "#CXXFLAGS = -Wall -g\n"
          "#LDFLAGS = -Wall -g\n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Libraries to be used by this program\n"
          "\n"
          "LIBS = \n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Common rules\n"
          "\n"
          ".PHONY: all clean\n"
          ".SUFFIXES: .o .d .c .cpp\n"
          "\n"
          "all: $(TARGET)\n"
          "\n"
          "$(TARGET): $(OBJS)\n"
          "	$(LD) $(LDFLAGS) $(OBJS) -o $@ $(LIBS)\n"
          "\n"
          ".c.o:\n"
          "	$(CC) $(CFLAGS) $(C_INCLUDE) -c $< -o $@\n"
          "\n"
          ".cpp.o:\n"
          "	$(CXX) $(CXXFLAGS) $(CXX_INCLUDE) -c $< -o $@\n"
          "\n"
          "clean:\n"
          "	$(RM) $(TARGET) $(OBJS) $(DEPS)\n"
          "	$(RM) *~ \\#*\\#\n"
          "\n"
          "# ------------------------------------------------------------------------\n"
          "# Dependency rules\n"
          "\n"
          "# you can append some rules above this line\n"
          "\n"
          ".c.d:\n"
          "	@$(CC) -MM $(CFLAGS) $(C_INCLUDE) $< > $@.$$$$; \\\n"
          "	sed 's,\\($*\\)\\.o[ :]*,\\1.o $@ : ,g' < $@.$$$$ > $@; \\\n"
          "	rm -f $@.$$$$"
          "\n"
          ".cpp.d:\n"
          "	@$(CXX) -MM $(CXXFLAGS) $(CXX_INCLUDE) $< > $@.$$$$; \\\n"
          "	sed 's,\\($*\\)\\.o[ :]*,\\1.o $@ : ,g' < $@.$$$$ > $@; \\\n"
          "	rm -f $@.$$$$\n"
          "\n"
          "-include $(DEPS)\n"
          "\n"
          "## " (file-name-nondirectory (buffer-file-name)) " ends here\n"))
       auto-insert-alist))

;; ------------------------------------------------------------------------
;; dired-x

(require 'dired-x)

;; 再帰処理を有効化
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; 2 画面ファイラーライク
(setq dired-dwin-target t)

;; 日付フォーマットを変更
(require 'ls-lisp)
(setq ls-lisp-use-localized-time-format t)
(setq ls-lisp-format-time-list (quote ("%Y/%m/%d %H:%M" "%Y/%m/%d %H:%M")))

;; 意味はよくわからないがそのうち役に立つかも
;; <http://stackoverflow.com/questions/9985313/copy-files-with-folders>
;; (setq ls-lisp-use-insert-directory-program nil)

;; ddskk とのキーバインド衝突回避
(when (fboundp 'skk-mode)
  (global-set-key "\C-x\C-j" 'skk-mode))

;; ------------------------------------------------------------------------
;; dired+

(require 'dired+)

;; ------------------------------------------------------------------------
;; elscreen

;; alist はたぶん APEL に含まれているので howm を el-get で入れれば入るはず
(when (require 'alist nil 'noerror)
  (require 'elscreen)
  (require 'elscreen-dired)
  (require 'elscreen-howm)

  (elscreen-start)

  ;; C-\t と C-S-\t で elscreen 間を移動
  (global-set-key (quote [C-tab]) 'elscreen-next)
  (global-set-key (quote [C-S-tab]) 'elscreen-previous)
  )

;; ------------------------------------------------------------------------
;; jaspace

;; whitespace-mode に置き換えたためコメントアウト
;; (require 'jaspace)

;; jaspace-mode を常に有効化
;; (jaspace-mode-on)
;; 各モードで jaspace-mode を有効化
;; (add-hook 'c-mode-common-hook '(lambda () (jaspace-mode-on)))
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (jaspace-mode-on)))
;; (add-hook 'makefile-mode-hook '(lambda () (jaspace-mode-on)))
;; (add-hook 'scala-mode-hook '(lambda () (jaspace-mode-on)))

;; 全角空白を表す文字
;; (setq jaspace-alternate-jaspace-string "□")
;; 改行文字を表す文字
;; (setq jaspace-alternate-eol-string "\xab\n")
;; タブ文字を表示
;; (setq jaspace-highlight-tabs t)

;; -----------------------------------------------------------------------------------
;; whitespace-mode

;; jaspace ライクな表示＋常時オン
;; <http://piyolian.blogspot.jp/2011/12/emacs-whitespace-like-jaspace.html>
(when (and (>= emacs-major-version 23)
           (require 'whitespace nil t))
  (setq whitespace-style
        '(face
          tabs spaces newline trailing space-before-tab space-after-tab
          space-mark tab-mark newline-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil
                        :foreground (if dark "pink4" "azure3")
                        :background 'unspecified)
    (set-face-attribute 'whitespace-tab nil
                        :foreground (if dark "gray20" "gray80")
                        :background 'unspecified
                        :strike-through t)
    (set-face-attribute 'whitespace-newline nil
                        :foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924] [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24] [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24] [?_]) ; hard space - currency
          (space-mark   ?　    [?□]    [?＿]) ; full-width space - square
          (newline-mark ?\n    [?\xAB ?\n])   ; eol - right quote mark
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))

;; ------------------------------------------------------------------------
;; migemo

(require 'migemo)

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary (concat platform-dependent-directory "share/migemo/utf-8/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)

;; キャッシュを有効化
;; NTEmacs24 + Cygwin 環境では有効にすると動作しなかった
;; (setq migemo-use-pattern-alist t)
;; (setq migemo-use-frequent-pattern-alist t)
;; (setq migemo-pattern-alist-length 1024)

(migemo-init)

;; ------------------------------------------------------------------------
;; text-mode

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode t)))

;; ------------------------------------------------------------------------
;; c-mode

(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map "\C-c\C-c" 'comment-or-compile)
             ;; コメントを "//" にする
             (setq comment-start "// ")
             (setq comment-end "")))

;; ------------------------------------------------------------------------
;; c++-mode

(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c++-mode-map "\C-c\C-c" 'comment-or-compile)
             (c-set-offset 'innamespace 0)))

;; ------------------------------------------------------------------------
;; objc-mode

;; *.m, *.mm
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))

;; *.h は内容で判定
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; ------------------------------------------------------------------------
;; flymake

(require 'flymake)

;; ------------------------------------------------------------------------
;; flymake-elisp

;; 外部ファイルを使わない版
;; <http://www.lunaport.net/blog/2010/02/windowsflymake-elisp-1.html>
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval"
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (emacs-lisp-mode)
              (let ((parse-sexp-ignore-comments t))
                (condition-case data
                    (scan-sexps (point-min) (point-max))
                  (scan-error
                   (goto-char(nth 2 data))
                   (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                  file (line-number-at-pos))))))))
          )
         )
        local-file)))))
(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
(push '("\\.emacs$" flymake-elisp-init) flymake-allowed-file-name-masks)
(add-hook 'emacs-lisp-mode-hook
          ;; workaround for (eq buffer-file-name nil)
          (function (lambda () (if buffer-file-name (flymake-mode)))))

;; ------------------------------------------------------------------------
;; Pymacs

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; (eval-after-load "pymacs"
;;   '(add-to-list 'pymacs-load-path (locate-user-emacs-file "my-pymacs")))

;; Cygwin 版 Python の python は Cygwin スタイルのシンボリックリンクのため
;; NTEmacs からは実行できない。
;; 参照先の python2.6.exe を使用するようにする。
;; (when system-type-is-windows
;;   ;; (setq pymacs-python-command "python2.6") ; 環境変数 PYMACS_PYTHON がないとき使用
;;   (setenv "PYMACS_PYTHON" "python2.6"))

;; ------------------------------------------------------------------------
;; ropemacs

;; auto-complete による補完
;; (ac-ropemacs-initialize)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; ac-ropemacs-initialize を呼び出す場合はこちらは不要
;; 補完は ac-python, jedi で行う場合はこちらで
(add-hook 'python-mode-hook
          '(lambda () (ac-ropemacs-require)))

;; python-mode 開始時点でプロジェクトファイルが存在すれば開く
(add-hook 'python-mode-hook
          '(lambda ()
             ;; ロードのタイミングがいまいちわからないため単に存在チェック
             (when (fboundp 'rope-open-project)
               (cond ((file-exists-p ".ropeproject")
                      (rope-open-project "."))
                      ((file-exists-p "../.ropeproject")
                       (rope-open-project ".."))))))

;; ------------------------------------------------------------------------
;; nXML-mode

(add-hook 'nxml-mode-hook
          '(lambda ()
             (setq nxml-slash-auto-complete-flag t) ; "</" で自動補完
             (setq nxml-child-indent 2)
             (auto-fill-mode t)))

(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

;; ------------------------------------------------------------------------
;; css-mode

(autoload 'css-mode "css-mode")

;; C スタイルのインデント
(setq cssm-indent-function #'cssm-c-style-indenter)

(setq auto-mode-alist
      (cons '("\\.\\(css\\|CSS\\)$" . css-mode) auto-mode-alist))

;; ------------------------------------------------------------------------
;; cmd-mode

(autoload 'cmd-mode "cmd-mode" "CMD mode." t)

(setq auto-mode-alist
      (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode)) auto-mode-alist))

;; ------------------------------------------------------------------------
;; evernote-mode

;; 参考:
;; <http://d.hatena.ne.jp/a_bicky/20120226/1330265529>

;; (require 'evernote-mode)

;; (setq enh-enclient-command (locate-user-emacs-file "bin/enclient.rb"))
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (setq evernote-username "lpubsppop01")

;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)

;; ------------------------------------------------------------------------
;; emacsclient

(server-start)

(require 'elscreen-server) ; (server-start) より後ろがよいらしい
(global-auto-revert-mode t) ; 上に移動した方がよいかなー

;; ------------------------------------------------------------------------
;; customize

(setq custom-file (locate-user-emacs-file "custom.el"))
(if (file-exists-p (expand-file-name custom-file))
    (load (expand-file-name custom-file) t nil nil))

;; ------------------------------------------------------------------------
;; desktop

(setq desktop-path (list (locate-user-emacs-file "."))
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.desktop.lock")
(desktop-save-mode t)

