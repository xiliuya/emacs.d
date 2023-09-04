;;; init-cape.el --- Summary -*- lexical-binding: t -*-

;; Copyright (C) 2023 xiliuya

;; Author: xiliuya <xiliuya@aliyun.com>
;; URL: https://github.com/xiliuya/
;; Maintainer:  xiliuya <xiliuya@aliyun.com>
;; Created: 2023-09-04 08:47:10

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
;; cape's init file
;;; Code:



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
  (global-set-key (kbd (car key-tmp)) (cdr key-tmp)))

(provide 'init-cape)
;;; init-cape.el ends here
