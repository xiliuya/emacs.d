;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; 配置 native-compile 包安装时启用
(setq package-native-compile t)
;;不编译 elc
(setq native-comp-jit-compilation nil)
;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
