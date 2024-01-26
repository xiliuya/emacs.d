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
            "xiliuya@aliyun.com" "xiliuya :)")
          ,(my-make-mu4e-context
            "xyz" "xiliuya"
            "xiliuya@xiliuya.xyz" "xiliuya :-)")))

  (setq
   mu4e-get-mail-command "offlineimap" ;; or fetchmail, or ...
   mu4e-update-interval 300)           ;; update every 5 minutes

  ;; ;; 绑定 evil 键位
  ;; (evil-collection-init 'mu4e)
  ;; 配置邮件地址高亮
  (mu4e-column-faces-mode)

  ;; 自定义邮件分类 mail-list emacs-devel 7 日内邮件.从邮件 raw 内查询关键字
  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               '( :name "emacs-devel 7 day"
                  :key  ?e
                  ;; :hide-unread t
                  :query "list:emacs-devel.gnu.org AND date:7d..now"))


  ;; 自定义邮件分类 非 emacs-devel 7 日内邮件.从邮件 raw 内查询关键字
  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               '( :name "other 7 day"
                  :key  ?o
                  ;; :hide-unread t
                  :query "NOT list:emacs-devel.gnu.org AND date:7d..now"))

  ;; 自定义文件夹 aliyun 邮箱
  (setq mu4e-maildir-shortcuts
        '( (:maildir "/ali/INBOX"              :key ?i)
           (:maildir "/ali/草稿"                :key ?c)
           (:maildir "/ali/垃圾邮件"             :key ?l)
           (:maildir "/ali/已删除邮件"           :key ?d)
           (:maildir "/ali/已发送"              :key ?f)
           (:maildir "/xyzdir/INBOX"              :key ?I)
           (:maildir "/xyzdir/Sent/"              :key ?S)
           (:maildir "/xyzdir/Trash/"              :key ?T)
           (:maildir "/aliyun/Archive"     :key ?a :hide t)
           (:maildir "/aliyun/Sent"        :key ?s :hide t)
           (:maildir "/aliyun/Drafts"      :key ?D :hide t)
           (:maildir "/aliyun/Trash"       :key ?t :hide t)
           ))
  ;; 配置附件下载目录
  (setq mu4e-attachment-dir "~/Desktop")

  )

;;; 配置 patch 高亮
;; (require 'message-view-patch)
;; (add-hook 'mu4e-view-mode-hook #'message-view-patch-highlight)

;;; 配置 smtp 跟随 mu4e 切换邮箱

(setq mail-lists-p '(("xiliuya@aliyun.com" "smtp.aliyun.com" 465)
                     ("xiliuya@xiliuya.xyz" "mx.xiliuya.xyz" 465)))
(defun xiliuya/smtpmail-set-address ()
  "Set smtpmail address from mail-lists-p and mu4e-context-current"
  (let ((mail-p (mu4e-context-vars (mu4e-context-current))))
    (dolist (mail-list-p mail-lists-p)
      (if (equal (alist-get 'user-mail-address mail-p)
                 (car mail-list-p))
          (setq user-mail-address (car mail-list-p)
                smtpmail-smtp-server (nth 1 mail-list-p)
                smtpmail-smtp-service (nth 2 mail-list-p))))))

(add-hook 'mu4e-context-changed-hook 'xiliuya/smtpmail-set-address)

(add-hook 'gnus-part-display-hook 'message-view-patch-highlight)

(require 'authinfo)

;; (authinfo-copy-password "smtp.aliyun.com" "xiliuya@aliyun.com")

(defun offlineimap-get-password (host user)
  "Return authinfo password."
  (authinfo-copy-password host user)
  (car authinfo-copied-passwords))

;;; org-msg 暂时不做考虑使用
(with-eval-after-load 'org-msg
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
  )

;;; 配置 thread 折叠
(defun mu4e~headers-msg-unread (msg)
  "Check if the given message is unread."
  (let ((flags (mu4e-message-field msg :flags)))
    (and (member 'unread flags) (not (member 'trashed flags)))))


(defun mu4e-headers-toggle-thread-folding (&optional subthread fold-and-move)
  "Checks if the thread at point is folded or not and toggles its
folding state.  Folding is achieved using overlays and the
invisible property.  With the optional argument SUBTHREAD it only
folds the subthread and not the whole thread.  With the optional
argument FOLD-AND-MOVE it moves to the next thread after
folding."
  (interactive "P")
  (let ((last-marked-point (point)) ; Hold our starting position
        (first-marked-point)
        (msg-count 0)               ; Count folded messages
        (unread-msg-count 0)        ; Count unread folded messages
        (folded))
    (save-excursion
      (end-of-line)
      ;; Check overlays at point
      (let ((overlays (overlays-at (+ (point) 1))))
        (while overlays
          (let ((o (car overlays)))
            ;; If folded, unfold it
            (when (overlay-get o 'mu4e-folded-thread)
              (delete-overlay o) ; Deleting the overlay removes all its
                                        ; properties
              (setq folded t)
              (setq overlays '(t))))    ; exit the loop
          (setq overlays (cdr overlays))))
      (unless folded            ; If we found something to unfold ignore
        (let* ((msg (mu4e-message-at-point))
               ;; note: the thread id is shared by all messages in a thread
               (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
               (path (mu4e~headers-get-thread-info msg 'path)))
          (if subthread
              (mu4e-headers-for-each
               (lambda (mymsg)
                 (when (string-match (concat "^" path)
                                     (mu4e~headers-get-thread-info mymsg 'path))
                   (setq msg-count (+ msg-count 1))
                   (when (mu4e~headers-msg-unread mymsg)
                     (setq unread-msg-count (+ unread-msg-count 1)))
                   (end-of-line)
                   ;; We need to move one point left to avoid weird
                   ;; behavior, I think this is happening because
                   ;; (end-of-line) takes us after the linebreak.
                   (setq last-marked-point (- (point) 1))
                   (unless first-marked-point
                     (setq first-marked-point last-marked-point)))))
            (mu4e-headers-for-each
             (lambda (mymsg)
               (when (string= thread-id
                              (mu4e~headers-get-thread-info mymsg 'thread-id))
                 (setq msg-count (+ msg-count 1))
                 (when (mu4e~headers-msg-unread mymsg)
                   (setq unread-msg-count (+ unread-msg-count 1)))
                 (end-of-line)
                 (setq last-marked-point (- (point) 1))
                 (unless first-marked-point
                   (setq first-marked-point last-marked-point))))))
          ;; If it contains more than one messages, then fold it
          (when (/= first-marked-point last-marked-point)
            (let ((o (make-overlay first-marked-point last-marked-point)))
              (overlay-put o 'mu4e-folded-thread t) ; Mark it as folded
              (if (/= unread-msg-count 0)
                  (overlay-put o 'display (format " \n  <> +%d(%d)" msg-count unread-msg-count))
                (overlay-put o 'display (format " \n  <> +%d" msg-count)))
              (overlay-put o 'evaporate t)
              (overlay-put o 'invisible t) ; Make it disappear
              (unless fold-and-move        ; Move to next thread?
                ;; If not take us to the first message in the folded thread
                (goto-char first-marked-point)
                (beginning-of-line))))
          (when fold-and-move            ; Move to next thread?
            (goto-char last-marked-point)
            (mu4e-headers-next)))))))


(defun mu4e-switch-update-buffer ()
  "当 mu4e 在进行后台更新时,将当前 window 切换到 update buffer."
  (interactive)
  (if (buffer-live-p mu4e--update-buffer)
      (switch-to-buffer mu4e--update-buffer)))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
