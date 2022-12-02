;;; init-local.el --- my set -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'sdcv)
(require-package 'evil)
(require-package 'evil-escape)

(require-package 'pyim)
(require-package 'pyim-basedict)
(require-package 'yasnippet)

;;; 配置 sdcv 
(require 'sdcv)

;;; 修复悬浮窗口问题
;;;(setq sdcv-popup-function 'showtip)
;;;(setq sdcv-popup-function 'tooltip-show)
(setq sdcv-popup-function 'pos-tip-show)
  ;;;(showtip "helll")
(setq sdcv-word-pronounce 'nil)
  ;;; (sdcv-search-simple "hello")
  ;;; (sdcv-search-with-dictionary-args "say" sdcv-dictionary-simple-list)
;;; 定义键绑定
(global-set-key (kbd "<f9>") 'sdcv-search-pointer)
(global-set-key (kbd "<f8>") 'sdcv-search-pointer+)


;;; 配置 源镜像
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;;; 配置 evil 

(require 'evil)
(evil-mode)
;;; (local-set-key (kbd "jj") 'evil-normal-state)
(with-eval-after-load 'evil
  (evil-escape-mode))
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)
;;; 配置org-mode 下正常模式的 tab 切换
(define-key evil-normal-state-map (kbd "<tab>") 'org-cycle)

;;; 配置 pyim 

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
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

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

;;; 配置 org 下的 ditaa plantuml 
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")



;;; 配置 eglot
;;;(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-common-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)

;;; (add-hook 'prog-mode-hook #'yas-minor-mode)

;;; 配置 auto-insert 

(add-hook 'find-file-hook 'auto-insert)
(add-hook 'find-file-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  (yas-global-mode 1))
(setq user-full-name "xiliuya")
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


(provide 'init-local)
;;; init-local.el ends here
