;;; init-plantuml.el --- Summary -*- lexical-binding: t -*-

;; Copyright (C) 2023 xiliuya

;; Author: xiliuya <xiliuya@aliyun.com>
;; URL: https://github.com/xiliuya/
;; Maintainer:  xiliuya <xiliuya@aliyun.com>
;; Created: 2023-09-04 09:40:23

;; Keywords: extensions
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.4"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; plantuml and ditaa's init file.
;;; Code:


;;; 配置 org 下的 ditaa plantuml
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
;; 配置 java 环境变量
(setenv "_JAVA_OPTIONS" 'nil)

;; 配置为离线关键词
(with-eval-after-load 'plantuml-mode
  (setq plantuml-server-url
        (concat  "file:"
                 (expand-file-name user-emacs-directory)
                 "plantuml")))


(defun cape-plantuml-backend (action &optional arg &rest _)
  "plantuml 的 cape 补全后端"
  (pcase action
    ;; 当前坐标为symbol,时进行补全 返回需要补全的字符串
    ('prefix (and (thing-at-point 'symbol)
                  (cons (thing-at-point 'symbol) t)))
    ;; 返回补全的列表
    ('candidates (all-completions arg plantuml-kwdList))

    ;; 返回补全的备注信息,应当有符号类型,但这里未使用
    ('annotation (format " "))

    ;; 返回补全的图标
    ('kind (car '(keyword)))))

;;; 加入hook 令其生效
;; Register emoji backend with `completion-at-point'
(add-hook 'plantuml-mode-hook
          (lambda ()
            (setq completion-at-point-functions
                  (list (cape-company-to-capf
                         #'cape-plantuml-backend)))))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
