;;; init-mu4e.el ---  mu4e setting -*- lexical-binding: t -*-
;;; Commentary: my email
;;; Code:

(require-package 'org-msg)
(require-package 'mu4e-column-faces)
(require-package 'message-view-patch)

;; 配置邮箱地址
(setq user-mail-address	"xiliuya@aliyun.com"
      ;;(setq user-mail-address	"xiliuya@outlook.com"
      user-full-name	"xiliuya")

;;; 配置 EasyPG
(setq epg-pinentry-mode 'loopback)

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

;;; 配置 mu4e 收发邮件
(require 'mu4e)

;;; 配置为默认邮件工具
(setq mail-user-agent 'mu4e-user-agent)
(set-variable 'read-mail-command 'mu4e)

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

  ;; ;; 绑定 evil 键位
  ;; (evil-collection-init 'mu4e)
  ;; 配置邮件地址高亮
  (mu4e-column-faces-mode)

  )

;;; 配置 patch 高亮
;; (require 'message-view-patch)
;; (add-hook 'mu4e-view-mode-hook #'message-view-patch-highlight)

(add-hook 'gnus-part-display-hook 'message-view-patch-highlight)

(require 'authinfo)

;; (authinfo-copy-password "smtp.aliyun.com" "xiliuya@aliyun.com")

(defun offlineimap-get-password (host user)
  "Return authinfo password."
  (authinfo-copy-password host user)
  (car authinfo-copied-passwords)
  )

;; 自定义邮件分类 mail-list emacs-devel 7 日内邮件.从邮件 raw 内查询关键字
(add-to-list 'mu4e-bookmarks
             ;; add bookmark for recent messages on the Mu mailing list.
             '( :name "emacs-devel 7 day"
                :key  ?e
                ;; :hide-unread t
                :query "list:emacs-devel.gnu.org AND date:7d..now"))

;; 自定义文件夹 aliyun 邮箱
(setq mu4e-maildir-shortcuts
      '( (:maildir "/INBOX"              :key ?i)
         (:maildir "/草稿"                :key ?c)
         (:maildir "/垃圾邮件"             :key ?l)
         (:maildir "/已删除邮件"           :key ?d)
         (:maildir "/已发送"              :key ?f)
         (:maildir "/aliyun/Archive"     :key ?a :hide t)
         (:maildir "/aliyun/Sent"        :key ?s :hide t)
         (:maildir "/aliyun/Drafts"      :key ?D :hide t)
         (:maildir "/aliyun/Trash"       :key ?t :hide t)
         ))
;; 配置附件下载目录
(setq mu4e-attachment-dir "~/Desktop")

;;; org-msg 暂时不做考虑使用

(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-greeting-fmt "\nHi%s,\n\n"
      org-msg-recipient-names '(("xiliuya@aliyun.com" . "Xiliuya"))
      org-msg-greeting-name-limit 3
      org-msg-default-alternatives '((new		. (text html))
                                     (reply-to-html	. (text html))
                                     (reply-to-text	. (text)))
      org-msg-convert-citation t
      org-msg-signature "

 Regards,

 #+begin_signature
 --
 *Xiliuya*
 /One Emacs to rule them all/
 #+end_signature")

(provide 'init-mu4e)
;;; init-mu4e.el ends here
