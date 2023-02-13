;;; init-backup.el --- init backup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

(setq backup-each-save-filter-function 'backup-each-save-filter)
(setq backup-each-save-mirror-location "~/.emacs.d/.backups")

(defun backup-each-save-filter (filename)
  (let ((ignored-filenames
         '("^/tmp" "semantic.cache$" "\\.emacs-places$"
           "\\.recentf$" ".newsrc\\(\\.eld\\)?"))
        (matched-ignored-filename nil))
    (mapc
     (lambda (x)
       (when (string-match x filename)
         (setq matched-ignored-filename t)))
     ignored-filenames)
    (not matched-ignored-filename)))

;;删除一周以上的备份文件
(defun backup-delete-oldfile ()
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist
        (file (directory-files-recursively
               backup-each-save-mirror-location
               "[0-9]\\{4\\}\\(_[0-9]\\{2\\}\\)\\{5\\}"))
      (when (and (string-match "[0-9]\\{4\\}\\(_[0-9]\\{2\\}\\)\\{5\\}" file)
                 (> (- current (float-time (fifth (file-attributes file))))
                    week))
        (message "%s" file)
        (delete-file file)
        ))))

(provide 'init-backup)
;;; init-backup.el ends here
