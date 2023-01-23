;;; init-mu4e.el ---  mu4e setting -*- lexical-binding: t -*-

;; Copyright (C) 2014 xiliuya

;; Author: xiliuya <xiliuya@aliyun.com>
;; Version: 0

;;; Commentary: my email

;;; Code:

;; 配置邮箱地址
(setq user-mail-address	"xiliuya@aliyun.com"
      ;;(setq user-mail-address	"xiliuya@outlook.com"
      user-full-name	"xiliuya")

;;; 配置 EasyPG
(setq epa-pinentry-mode 'loopback)

;;; 配置 mu4e 收发邮件
(require 'mu4e)
;; assumed Maildir layout
;; ~/Maildir/Account0/{Inbox,Sent,Trash}
;; ~/Maildir/Account1/{Inbox,Sent,Trash}
;; where Account0 is context name
(defun my-make-mu4e-context (context-name full-name mail-address signature)
  "Return a mu4e context named CONTEXT-NAME with :match-func matching
  folder name CONTEXT-NAME in Maildir. The context's `user-mail-address',
  `user-full-name' and `mu4e-compose-signature' is set to MAIL-ADDRESS
  FULL-NAME and SIGNATURE respectively.
  Special folders are set to context specific folders."
  (let ((dir-name (concat "/" context-name)))
    (make-mu4e-context
     :name context-name
     ;; we match based on the maildir of the message
     ;; this matches maildir /Arkham and its sub-directories
     :match-func
     `(lambda (msg)
        (when msg
          (string-match-p
           ,(concat "^" dir-name)
           (mu4e-message-field msg :maildir))))
     :vars
     `((user-mail-address    . ,mail-address)
       (user-full-name       . ,full-name)
       (mu4e-sent-folder     . ,(concat dir-name "/Sent"))
       (mu4e-drafts-folder   . ,(concat dir-name "/Drafts"))
       (mu4e-trash-folder    . ,(concat dir-name "/Trash"))
       (mu4e-refile-folder   . ,(concat dir-name "/Archive"))
       (mu4e-compose-signature . ,signature)))))

(with-eval-after-load 'mu4e
  ;;Fixing duplicate UID errors when using mbsync and mu4e
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-contexts
        `(,(my-make-mu4e-context
            "aliyun" "xiliuya"
            "xiliuya@aliyun.com" "xiliuya :)")))

  (setq
   mu4e-get-mail-command "offlineimap" ;; or fetchmail, or ...
   mu4e-update-interval 300)           ;; update every 5 minutes

  ;; 绑定 evil 键位
  (evil-collection-init 'mu4e)

  )

;;; 配置邮件发送
(require 'smtpmail)
;; 配置 smtp 发送
(with-eval-after-load 'smtpmail
  (setq send-mail-function	'smtpmail-send-it
        message-send-mail-function	'smtpmail-send-it
        smtpmail-smtp-server		"smtp.aliyun.com"
        smtpmail-smtp-service		465
        smtpmail-stream-type 'ssl
        )

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  )




(provide 'init-mu4e)
;;; init-mu4e.el ends here
