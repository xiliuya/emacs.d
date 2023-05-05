;;; init-local.el --- my set -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'sdcv)
(require-package 'evil)
(require-package 'evil-escape)
;; (require-package 'evil-collection)

(require-package 'pyim)
(require-package 'pyim-basedict)
(require-package 'posframe)

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

(require-package 'org-roam)
(require-package 'org-roam-ui)

(require-package 'org-download)

(require-package 'mpv)
(require-package 'org-mpv-notes)

(require-package 'org-modern)
(require-package 'valign)
(require-package 'org-transclusion)

(require-package 'kind-icon)
(require-package 'format-all)

(require-package 'yapfify)
(require-package 'ob-ipython)
(require-package 'elquery)

(require-package 'protobuf-mode)

(require-package 'cape)

(require-package 'nov)
(require-package 'bongo)

(require-package 'code-cells)

(require-package 'live-py-mode)
(require-package 'go-mode)

(require-package 'gdscript-mode)
;;; 配置 sdcv
;;; (require 'sdcv)

;;; 修复悬浮窗口问题
;;;(setq sdcv-popup-function 'showtip)
;;;(setq sdcv-popup-function 'tooltip-show)
(setq sdcv-popup-function 'pos-tip-show)
  ;;;(showtip "helll")
;;;(setq sdcv-word-pronounce 'nil)
  ;;; (sdcv-search-simple "hello")
  ;;; (sdcv-search-with-dictionary-args "say" sdcv-dictionary-simple-list)
;;; 定义键绑定
(global-set-key (kbd "<f9>") 'sdcv-search-pointer)
(global-set-key (kbd "<f8>") 'sdcv-search-pointer+)

;;; 配置修复 espeak 使用时出现的白屏
(with-eval-after-load 'sdcv
  (defun sdcv-search-simple (&optional word)
    "Search WORD simple translate result."
    (funcall
     sdcv-popup-function
     (sdcv-filter
      (shell-command-to-string
       (mapconcat #'identity
                  (cons "sdcv" (sdcv-search-with-dictionary-args
                                (or word (sdcv-region-or-word))
                                sdcv-dictionary-simple-list))
                  " "))))

    ;; pronounce the word (Add by me)
    (when sdcv-word-pronounce
      ;; `sleep-for', `sit-for'.
      (sit-for 0.8)
      (sdcv-pronounce-word word)
      )
    )
  )

;;; 配置 源镜像
(with-eval-after-load 'init-elpa
  (setq package-enable-at-startup nil)
  (setq package-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))))

;;; 配置 evil

;;; (require 'evil)
;; evil-collection 配置
;; (setq evil-want-keybinding 'nil)
(evil-mode)
;;; (local-set-key (kbd "jj") 'evil-normal-state)
(with-eval-after-load 'evil
  (evil-escape-mode)
  ;; 配置 g* 查找 symbol
  (setq  evil-symbol-word-search  20)

  ;; 某些模式不使用 evil
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  )
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
;;; 配置 org-mode 下正常模式的 tab 切换
(defun my/evil-org-tab ()
  "Run 'org-cycle When in 'org-mode."
  (interactive)
  (when (memq major-mode '(org-mode))
    ;;;(progn (message "hello"))
    (org-cycle)
    )
  )
(define-key evil-normal-state-map (kbd "<tab>") 'my/evil-org-tab)
;;;(define-key evil-normal-state-map (kbd "<tab>") 'org-cycle)

;;; 配置 插入模式键 移动绑定

(with-eval-after-load 'evil
  (keymap-set evil-insert-state-map "M-h" #'left-char)
  (keymap-set evil-insert-state-map "M-l" #'right-char)
  (keymap-set evil-insert-state-map "M-j" #'next-line)
  (keymap-set evil-insert-state-map "M-k" #'previous-line))
;;; 配置 evil-ex

(evil-ex-define-cmd "k[illbuffer]" 'kill-this-buffer)

;;; 配置 pyim

;;; 禁用掉 gtk 的 im
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (pgtk-use-im-context nil)))

;; (require 'pyim)
;; (require 'pyim-basedict)
;; (require 'pyim-cregexp-utils)

;; 如果使用 popup page tooltip, 就需要加载 popup 包。
;; (require 'popup nil t)
;; (setq pyim-page-tooltip 'popup)

;; 如果使用 pyim-dregcache dcache 后端，就需要加载 pyim-dregcache 包。
;; (require 'pyim-dregcache)
;; (setq pyim-dcache-backend 'pyim-dregcache)

(pyim-basedict-enable)

(setq default-input-method "pyim")

;; 显示5个候选词。
(setq pyim-page-length 5)

;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
(global-set-key (kbd "M-'") 'pyim-convert-string-at-point)

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

;; 我使用全拼
(pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'wubi)
;; (pyim-default-scheme 'cangjie)

;; 我使用云拼音
;;; (setq pyim-cloudim 'baidu)

;; pyim 探针设置
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

;; 用不到这个功能, 由于会出现代码模式干扰, 暂时关闭掉.
(setq  pyim-outcome-trigger 'nil)
;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)
;;; 配置 全角半角字符 yes|no|auto
(setq-default pyim-punctuation-translate-p '(no))

;;; hack async 解决出现多个 *emacs*:err buffer

;;; 配置 org 导出添加markdown 格式
(with-eval-after-load 'org
  (add-to-list 'org-export-backends 'md))

;; 配置 html 导出头
(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/org.css\"/>")

 ;;; org 导出到 html , 并将 org-roam-node 转换为 org-transclude 块
(defun xiliuya/org-roam-to-html ()
  "Export org to html covert org-roam-node to org-transclude."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "[[id:" nil t)
    (progn (replace-match "\n#+transclude: [[id:")
           ;;(org-transclusion-add)
           ))
  (org-transclusion-add-all)
  (org-html-export-to-html)
  (org-transclusion-remove-all)
  (goto-char (point-min))
  (sit-for 1)
  (while (search-forward "\n#+transclude: [[id:" nil t)
    (replace-match "[[id:"))
  )
;;(xiliuya/org-roam-to-html)

;;; 配置 org 下的 ditaa plantuml
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")


;;; 配置 org-agenda
;; 配置目录
(setq org-agenda-files '("~/myday/daily/"))

;;; 配置 org-roam

(global-set-key (kbd "C-c n l")  'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n u") 'org-roam-ui-open)

(global-set-key (kbd "C-c n d") 'org-id-get-create)

(setq org-roam-directory "~/myday")
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))
(org-roam-db-autosync-mode)
;; If using org-roam-protocol
(require 'org-roam-protocol)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+TITLE: %<%Y-%m-%d>\n#+AUTHOR: [[https://xiliuya.github.io/][xiliuya]]\n#+EMAIL: xiliuya@aliyun.com\n#+LANGUAGE: zh-CN\n#+OPTIONS: todo:nil num:3 H:4 ^:{} pri:t\n#+COLUMNS: %10ITEM %10PRIORITY %15TODO %65TAGS"
                            )
         :unnarrowed t)))

(defun zp/org-protocol-insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    ;;(format "#+begin_quote\n%s\n#+end_quote" selection)
    (format "\n%s\n" selection)
    ))

(setq org-roam-capture-ref-templates

      '(("r" "ref" plain "* %U\n
%(zp/org-protocol-insert-selection-dwim \"%i\")%?"
         :target (file+head "web/${slug}.org"
                            "#+title: ${title}\n
#+roam_key: ${ref}\n
#+created: %u\n"
                            )
         :unnarrowed t))
      )

(setq find-file-visit-truename t)

(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)


;;; 配置 eglot
;; mode-hook
(dolist (hook-mode '(python-mode-hook
                     c-mode-hook
                     rust-mode-hook
                     haskell-mode-hook
                     gdscript-mode
                     ))
  (add-hook hook-mode 'eglot-ensure)
  )

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e p") #'yapfify-buffer)
  ;; 配置 lsp server
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan

  (setq
   ;; 自动关闭
   eglot-autoshutdown t)

  )
;;关闭 c 模式 flycheck backend

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(c/c++-clang c/c++-gcc c/c++-cppcheck haskell-ghc))))
;;; 配置 yasnippet
;;; 配置 auto-insert
;; 由于出现了很多问题(pyim/yasinppets),暂时屏蔽掉 symbol-overlay
;;(remove-hook 'prog-mode-hook 'symbol-overlay-mode)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'find-file-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  ;;;(add-hook 'prog-mode-hook #'yas-minor-mode)
  (yas-global-mode 1)
  )
;;; (auto-insert-mode 1)
(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
(custom-set-variables
 '(auto-insert 'other)
 '(auto-insert-directory "~/.emacs.d/templates/")
 '(auto-insert-alist '(
                       (("\\.\\([Hh]\\|hh\\|hpp\\)\\'"
                         . "C / C++ header")
                        . ["template.h"
                           c++-mode my/autoinsert-yas-expand])
                       (("\\.\\([cC]\\|cc\\|cpp\\)\\'"
                         . "C++ source")
                        . ["template.c" my/autoinsert-yas-expand])
                       (("\\.rs\\'"
                         . "Rust source")
                        . ["template.rs" my/autoinsert-yas-expand])
                       (("\\.sh\\'"
                         . "Shell script")
                        . ["template.sh" my/autoinsert-yas-expand])
                       (("\\.el\\'" . "Emacs Lisp")
                        . ["template.el" my/autoinsert-yas-expand])
                       (("\\.pl\\'"
                         . "Perl script")
                        . ["template.pl" my/autoinsert-yas-expand])
                       (("\\.pm\\'"
                         . "Perl module")
                        . ["template.pm" my/autoinsert-yas-expand])
                       (("\\.py\\'"
                         . "Python script")
                        . ["template.py" my/autoinsert-yas-expand])
                       (("\\.go\\'"
                         . "Go source")
                        . ["template.go" my/autoinsert-yas-expand])
                       (("[mM]akefile\\'"
                         . "Makefile")
                        . ["Makefile" my/autoinsert-yas-expand])
                       (("\\.tex\\'"
                         . "TeX/LaTeX")
                        . ["template.tex"
                           my/autoinsert-yas-expand]))))

;;; 配置 header2
;; 绑定快捷键
(require 'header2)
(dolist (key-tmp '(("C-c h m" . make-header)
                   ("C-c h v" . make-revision)
                   ("C-c h u" . update-file-header)
                   ("C-c h l" . update-last-modifier)
                   ("C-c h d" . make-divider)
                   ("C-c h b" . make-box-comment)
                   ))
  (global-set-key (kbd (car key-tmp)) (cdr key-tmp))
  )

;;; 配置 erc
(require 'erc-backend)
(require 'erc-sasl)
(add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")
(with-eval-after-load 'erc
  (defun erc-login ()
    "Perform user authentication at the IRC server. (PATCHED)"
    (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                     (erc-current-nick)
                     (user-login-name)
                     (or erc-system-name (system-name))
                     erc-session-server
                     erc-session-user-full-name))
    (if erc-session-password
        (erc-server-send (format "PASS %s" erc-session-password))
      (message "Logging in without password"))
    (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
      (erc-server-send "CAP REQ :sasl"))
    (erc-server-send (format "NICK %s" (erc-current-nick)))
    (erc-server-send
     (format "USER %s %s %s :%s"
             ;; hacked - S.B.
             (if erc-anonymous-login erc-email-userid (user-login-name))
             "0" "*"
             erc-session-user-full-name))
    (erc-update-mode-line))
  )
(setq erc-autojoin-channels-alist
      '((Libera.Chat "#libera" "#list" "#linux" "$emacs")))

;;; 配置 gif 截图
(require 'gif-screencast)
(with-eval-after-load 'gif-screencast
  (define-key gif-screencast-mode-map (kbd "C-<f12>") 'gif-screencast-toggle-pause)
  (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-stop)
  (global-set-key (kbd "<f12>") 'gif-screencast)
  )

;;; org-download 配置
(require 'org-download)
;;;(setq org-download-screenshot-method "flameshot gui --raw >%s")
(setq org-download-screenshot-method "grim -g \"$(slurp)\" %s")
;;;(setq org-download-method 'directory)

;;; 屏蔽默认的图片保存, 直接保存到 ./images/xxx.png
(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "./images")
;;; 屏蔽默认的插入头
(defun dummy-org-download-annotate-function (link)
  "")
(setq org-download-annotate-function
      #'dummy-org-download-annotate-function)
;;; 取消默认的显示图片
(setq org-download-display-inline-images nil)

;;; 定义快捷键
(define-key org-mode-map (kbd "C-c s s") #'org-download-screenshot)
(define-key org-mode-map (kbd "C-c s c") #'org-download-clipboard)

;;; 配置 org-mpv-mode
(with-eval-after-load 'org-mpv-notes
  (define-minor-mode org-mpv-notes
    "Org minor mode for Note taking alongside audio and video.
Uses mpv.el to control mpv process"
    :keymap `((,(kbd "M-n i") . mpv-insert-playback-position)
              (,(kbd "M-n M-i") . org-mpv-notes-insert-note)
              (,(kbd "M-n u") . mpv-revert-seek)
              (,(kbd "M-n s") . org-mpv-notes-save-screenshot)
              (,(kbd "M-n o") . org-mpv-notes-open)
              (,(kbd "M-n k") . mpv-kill)
              (,(kbd "M-n t") . org-mpv-notes-this-timestamp)
              (,(kbd "M-n M-s") . org-mpv-notes-screenshot-ocr))
    (if org-mpv-notes
        ()
      (mpv-kill))))

(add-hook 'org-mode-hook 'org-mpv-notes)

;;; 配置 org-modern
(add-hook 'org-mode-hook 'org-modern-mode)
(setq org-modern-star nil)
(setq org-modern-table nil)

;; 配置 org 显示上下标
(setq org-use-sub-superscripts '{})
(setq org-pretty-entities 't
      org-pretty-entities-include-sub-superscripts 't)
;;; 配置 org-mode
;; 配置 todo 颜色
(setq org-todo-keyword-faces '(("TODO" . "red")
                               ("NEXT" . "yellow")
                               ("DONE" . "green")))

;;; 配置 org-modern-todo 配色

(setq org-modern-todo-faces
      '(("TODO" :foreground "silver" :inverse-video t)
        ("NEXT" :foreground "white" :inverse-video t)
        ;;("DONE" :foreground "gray" :inverse-video t)
        ("PROJECT" :foreground "violet" :inverse-video t)
        ("CANCELLED" :foreground "tan" :inverse-video t)
        ))

(add-hook 'org-mode-hook
          (lambda ()
            ;; 自动换行
            (setq truncate-lines nil)
            ))

(setq
 ;; 自动折叠列表
 org-cycle-include-plain-lists 'integrate
 ;; 自动折叠代码块
 org-hide-block-startup t
 ;; 自动启动 overview
 org-startup-folded t
 ;; 自动启动 indent
 org-startup-indented t
 )

;;; 配置 org-mode inline 图片宽度
(setq org-image-actual-width '(400))

;;; 配置 org src_block eglot 补全

;; 自定义函数 实现在 org-src-mode 和 列表内的模式启用 eglot
(defun xiliuya/org-src-eglot (mode-list)
  "Set '(buffer-file-name) when in 'org-src-mode (as MODE-LIST)"
  (interactive)
  (dolist (mode mode-list)
    (if (and (eq mode major-mode) org-src-mode)
        (progn (setq-local buffer-file-name (expand-file-name (assoc-default :tangle (nth 2 org-src--babel-info))))
               (eglot-ensure))
      )
    )
  )
;; 设定在 org-edit-src-code 末尾调用
(defun org-babel-edit-prep:C (babel-info)
  "After (org-edit-src-code) run when in C src code."
  (interactive)
  (xiliuya/org-src-eglot '(c-mode))
  )
(defun org-babel-edit-prep:python (babel-info)
  "After (org-edit-src-code) run when in python src code."
  (interactive)
  (xiliuya/org-src-eglot '(python-mode))
  )

;;; 配置 ob-ipython
;;(require 'ob-ipython)
;;换为修改 init-org.el

;; 配置代码块补全为 python-mode
(defun org-babel-edit-prep:ipython (babel-info)
  "After (org-edit-src-code) run when in python src code."
  (interactive)
  (xiliuya/org-src-eglot '(python-mode))
  )
;; hack ob-ipython
;; 加入 example python 头防止 [[]] 变成链接
;; 加入 html 音频保存为 wav
(with-eval-after-load 'ob-ipython
  (require 'elquery)
  (defun ob-ipython--render (file-or-nil values)
    ;;(setq testa (message "%S" values))
    (let ((org (lambda (value) value))
          (png (lambda (value)
                 (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
                   (ob-ipython--write-base64-string file value)
                   (format "[[file:%s]]" file))))
          (svg (lambda (value)
                 (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
                   (ob-ipython--write-string-to-file file value)
                   (format "[[file:%s]]" file))))
          (html (lambda (value)
                  (let ((file (or file-or-nil (ob-ipython--generate-file-name ".wav"))))
                    (ob-ipython--write-base64-string file (substring (plist-get (plist-get (car (elquery-$ "source" (elquery-read-string value))) :props) :src) 22))
                    (format "[[file:%s]]" file))
                  ))
          (txt (lambda (value)
                 (let ((lines (s-lines value)))
                   (if (cdr lines)
                       (->> lines
                            (-map 's-trim)
                            (s-join "\n  ")
                            (s-concat "  ")
                            (format "%s"))
                     (s-concat " " (car lines)))))))
      (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
          (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
          (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
          (-when-let (val (cdr (assoc 'text/html values))) (funcall html val))
          (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val))

          )))
  ;; 修复 python 3.11 更新后, jupyter 输出会警告.
  (setq ob-ipython-command "PYDEVD_DISABLE_FILE_VALIDATION=1 jupyter"))

;;(add-hook 'org-mode-hook 'prose-mode)
;;; 配置 ipython 使用 code-cell

;; 配置使用 pandoc
(setq code-cells-convert-ipynb-style
      '(("pandoc" "--to" "ipynb" "--from" "org")
        ("pandoc" "--to" "org" "--from" "ipynb")
        org-mode))

;;; 配置编译成功后,隐藏弹窗
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
  close the *compilation* buffer if the compilation is successful,
  and set the focus back to Emacs frame"

  (if (string-match "^finished" msg)
      (progn
        ;;(delete-windows-on buffer)
        ;;(bury-buffer buffer)
        ;;(kill-buffer buffer)
        (switch-to-prev-buffer (xiliuya/find-buffer (window-list) "*compilation*") 'bury )
        (if (and
             (cdr (window-list))
             (not (cdr (cdr (window-list))))
             (eq (window-buffer (nth 0 (window-list)))
                 (window-buffer (nth 1 (window-list)))))
            (delete-window (car (window-list)))
          )
        (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n "))
  )
;;; 找到活动窗口里,字符串对应的 window
(defun xiliuya/find-buffer (w-list str)
  "Find str in (window-list) buffer with full buffer name"

  (if (null w-list)
      nil
    (if (string-match (buffer-name (window-buffer (car w-list))) str)
        (car w-list)
      ;;(window-buffer (car w-list))
      (xiliuya/find-buffer (cdr w-list) str)
      )
    )
  ;;(setq w (car w-list))
  )
;;(xiliuya/find-buffer (window-list) "*scratch*")

;;; 配置 c-mode 编译命令
(add-to-list 'compilation-finish-functions
             'notify-compilation-result)
(add-hook 'c-mode-hook
          (lambda ()
            (unless (or (file-exists-p "Makefile") (not buffer-file-name))
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s %s"
                             ;;(format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

;;; 配置 c / python / elisp-mode 自动折叠
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;; 配置 format-all
(with-eval-after-load 'format-all
  (global-set-key (kbd "C-c f b") 'format-all-buffer)
  (global-set-key (kbd "C-c f m") 'format-all-mode)
  )

;;; 配置 treesiter
;;; 配置 c-ts-mode python-ts-mode
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; 直接将 hook 赋值给新的 hook
(setq c-ts-mode-hook c-mode-hook)
(setq python-ts-mode-hook python-mode-hook)


;;; 配置 c-mode 关闭 flycheck(eglot 自带的 check 足够用了
;; (add-hook 'c-mode-hook 'flymake-mode-off)
;; (add-hook 'eglot--managed-mode-hook 'flymake-mode-off)
;; (add-hook 'eglot--managed-mode-hook 'flycheck-mode)

;;; 配置 eldoc 自定义高亮文本
;; 输入关键字列表和字符串,返回 text-property 字符
(defun xiliuya/hl-mystring (str-key s-string)
  "Return a text property string."
  (dolist (keyword str-key)
    (setq key-pos (string-match keyword s-string))
    (if key-pos
        (put-text-property key-pos (+ key-pos (length keyword)) 'face 'font-lock-keyword-face s-string)
      )
    )
  (message s-string)
  )

;; 配置 advice 在 eldoc-display-in-echo-area 后执行下列函数
(defun xiliuya/eldoc-display-in-echo-area-after (string1)
  "Advice after run (eldoc-display-in-echo-area) ."
  (setq hl-key '("def" "file" "print" "False" "None" "True" "assert" "break" "class" "continue" "elif" "else" "except" "finally" "from" "global" "import" "lambda" "nonlocal" "raise" "return" "while" "yield" "EOF"))
  ;;(setq hl-key '("def" "file" "print" "False" "None" "True" "and" "as" "assert" "break" "class" "continue" "def" "del" "elif" "else" "except" "finally" "for" "from" "global" "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try" "while" "with" "yield" "---" "***"))
  (setq teststr "int a bcd defun a ; this test")
  (if (stringp string1)
      (xiliuya/hl-mystring hl-key string1))
  )
;; 绑定函数输出为自定义函数的输入
(advice-add 'eldoc-display-in-echo-area :filter-return #'xiliuya/eldoc-display-in-echo-area-after)

;;; 配置 kind-icon 美化 corfu
;; Quite beautiful
(with-eval-after-load 'corfu
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )

;;; 配置 cape 补全
;; (with-eval-after-load 'cape

;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-history)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;   )

(dolist (key-tmp '(("C-c f p" . completion-at-point) ;; capf
                   ("C-c f t" . complete-tag)        ;; etags
                   ("C-c f d" . cape-dabbrev) ;; or dabbrev-completion
                   ("C-c f h" . cape-history)
                   ("C-c f f" . cape-file)
                   ("C-c f k" . cape-keyword)
                   ("C-c f s" . cape-symbol)
                   ("C-c f a" . cape-abbrev)
                   ("C-c f i" . cape-ispell)
                   ("C-c f l" . cape-line)
                   ("C-c f w" . cape-dict)
                   ("C-c f \\" . cape-tex)
                   ("C-c f _" . cape-tex)
                   ("C-c f ^" . cape-tex)
                   ("C-c f &" . cape-sgml)
                   ("C-c f r" . cape-rfc1345)))
  (global-set-key (kbd (car key-tmp)) (cdr key-tmp))
  )

;;; magit gpg tty sign
;; 目前只能手动使用, 未找到合适 hook 点.
;; 已找到替代方案 目前不需要 hook
(defun magit-sign-with-tty ()
  (interactive)
  (if (processp magit-this-process)
      (progn (process-send-string
              magit-this-process
              (read-passwd
               "git sign password:"))
             (process-send-string magit-this-process "\n"))))

;;; 配置 magit
(with-eval-after-load 'magit
  ;; 加入正则匹配中文 "输入密码"
  (add-to-list 'magit-process-password-prompt-regexps "\u8F93\u5165\u5BC6\u7801")
  )

;;; 配置 bongo
(with-eval-after-load 'bongo
  (setq
   bongo-default-directory "~/Music"
   bongo-prefer-library-buffers nil
   bongo-insert-whole-directory-trees t
   bongo-logo nil
   bongo-display-track-icons nil
   bongo-display-track-lengths nil
   bongo-display-header-icons nil
   bongo-display-playback-mode-indicator t
   bongo-display-inline-playback-progress t
   bongo-join-inserted-tracks nil
   bongo-field-separator (propertize " · " 'face 'shadow)
   bongo-mark-played-tracks t
   bongo-header-line-mode nil
   bongo-mode-line-indicator-mode nil
   bongo-enabled-backends '(timidity mpv)
   ;;bongo-vlc-program-name "cvlc"
   )
  (add-hook 'bongo-playlist-mode-hook 'turn-off-evil-mode)
  (add-hook 'bongo-playlist-mode-hook 'bongo-random-playback-mode)
  (global-set-key (kbd "C-c b r") 'bongo-play-random)
  (global-set-key (kbd "C-c b s") 'bongo-start/stop)
  )
(global-set-key (kbd "C-c b p") 'bongo-playlist)

(when (file-exists-p "/home/xiliuya/test/python/openai/trans_rpc/trans_rpc_client.py")
  (require 'trans-rpc)
  (global-set-key (kbd "C-<f8>") 'trans-rpc-form-point-en)
  (global-set-key (kbd "C-<f9>") 'trans-rpc-form-point-zh)
  )

;;; 配置 epub nov-mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename))
  )
;;; 配置 gdscript-mode


;;; 配置为英语 time string
(setq system-time-locale "C")
;; 配置 mu4e 邮箱
(require 'init-mu4e)
;; 配置 backup
(require 'init-backup)

;;; 配置自动保存
(auto-save-visited-mode 1)

;; 配置 org-crypt
(require 'init-org-crypt)
;; 配置 evil
(require 'init-evil)

(provide 'init-local)
;;; init-local.el ends here
