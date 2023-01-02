;;; init-local.el --- my set -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'sdcv)
(require-package 'evil)
(require-package 'evil-escape)

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

;;; 配置 sdcv
(require 'sdcv)

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

(require 'evil)
(evil-mode)
;;; (local-set-key (kbd "jj") 'evil-normal-state)
(with-eval-after-load 'evil
  (evil-escape-mode))
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

;;; 配置 插入模式键绑定
(defun maple/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
          def (pop bindings))))

(maple/define-key evil-insert-state-map
                  (kbd "M-h") (kbd "<left>")
                  (kbd "M-l") (kbd "<right>")
                  (kbd "M-j") (kbd "<down>")
                  (kbd "M-k") (kbd "<up>"))

;;; 配置 evil-ex

(evil-ex-define-cmd "k[illbuffer]" 'kill-this-buffer)

;;; 配置 pyim

;;; 禁用掉 gtk 的 im
(add-hook 'prog-mode-hook
          (lambda ()
            (pgtk-use-im-context nil)))

(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-cregexp-utils)

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

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)
;;; 配置 全角半角字符 yes|no|auto
(setq-default pyim-punctuation-translate-p '(no))

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
                            "#+TITLE: %<%Y-%m-%d>\n#+AUTHOR: [[https://xiliuya.github.io/][xiliuya]]\n#+EMAIL: xiliuya@163.com\n#+LANGUAGE: zh-CN\n#+OPTIONS: todo:nil num:3 H:4 ^:nil pri:t\n#+TAGS:  { 糟糕(1) 凑合(2) 不错(3) 很好(4) 极品(5) }\n#+COLUMNS: %10ITEM %10PRIORITY %15TODO %65TAGS"
                            )
         :unnarrowed t)))

(setq find-file-visit-truename t)

(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)


;;; 配置 eglot
;;;(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-common-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e f") #'eglot-format)
  )

;;; 配置 yasnippet
;;; 配置 auto-insert
;; 由于出现了很多问题(pyim/yasinppets),暂时屏蔽掉 symbol-overlay
(remove-hook 'prog-mode-hook 'symbol-overlay-mode)
(add-hook 'find-file-hook 'auto-insert)
(add-hook 'find-file-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  ;;;(add-hook 'prog-mode-hook #'yas-minor-mode)
  (yas-global-mode 1)
  )
(setq user-full-name "xiliuya")
(setq user-mail-address "xiliuya@163.com")
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

;;(add-hook 'org-mode-hook 'prose-mode)

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
        (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n "))
  )
;;; 找到活动窗口里,字符串对应的 window
(defun xiliuya/find-buffer (w-list str)
  "Find a window-list buffer with full buffer name"

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
            (unless (file-exists-p "Makefile")
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

(provide 'init-local)
;;; init-local.el ends here
