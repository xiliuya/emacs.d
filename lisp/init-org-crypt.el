;;; init-org-crypt.el --- Summary -*- lexical-binding: t -*-

;; Copyright (C) 2023 xiliuya
;;; Commentary:
;;; Code:

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-tag-matcher "crypt")
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; 配置密钥 nil 为对称加密
;;;请配置自己的私人密钥!
;;(setq org-crypt-key nil)
(setq org-crypt-key "00B4A3B86D060E68679EA20FDE800D3354D096DF")


(provide 'init-org-crypt)
;;; init-org-crypt.el ends here
