;;; init-evil.el --- Summary -*- lexical-binding: t -*-

;; Copyright (C) 2023 xiliuya
;;; Commentary:
;;; Code:

(defun xiliuya/evil-replace-number (startcol endcol &rest args)
  ;;(message "%S" args)
  (let ( (beg (save-excursion (evil-move-to-column startcol)))
         (end (save-excursion (evil-move-to-column endcol t)))
         )
    (let (
          (yank-str (buffer-substring-no-properties beg end))
          (add-number (1+ (- (line-number-at-pos beg)
                             (line-number-at-pos (car args))
                             )))
          )
      (delete-region beg end)
      ;; (message "%S_%S_%S" beg end add-number)
      (save-excursion (goto-char beg)
                      ;; (message "%d"  (+ add-number
                      ;;                   (string-to-number
                      ;;                    yank-str)))
                      (insert (format "%d"  (+ add-number
                                               (string-to-number yank-str))))))))

(defun xiliuya/evil-replace-number-addone (startcol endcol &rest args)
  (let ( (beg (save-excursion (evil-move-to-column startcol)))
         (end (save-excursion (evil-move-to-column endcol t)))
         )
    (xiliuya/evil-replace-number startcol endcol beg end)))

(evil-define-operator xiliuya/evil-replace-region (beg end type register yank-handler)
  "Add text number with line from BEG to END with TYPE.
  Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  ;; (message ":%S_%S" beg end)
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'xiliuya/evil-replace-number beg end 't beg end))))

(evil-define-operator xiliuya/evil-replace-region-one (beg end type register yank-handler)
  "Add text number with line from BEG to END with TYPE.
  Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  ;; (message ":%S_%S" beg end)
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'xiliuya/evil-replace-number-addone beg end 't beg end))))

(provide 'init-evil)
;;; init-evil.el ends here
