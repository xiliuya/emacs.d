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
          (add-number (- (line-number-at-pos beg)
                         (line-number-at-pos (car args))
                         ))
          )
      (delete-region beg end)
      ;; (message "%S_%S_%S" beg end add-number)
      (save-excursion (goto-char beg)
                      (message "%d"  (+ add-number
                                        (string-to-number
                                         yank-str)))
                      (insert (format "%d"  (+ add-number
                                               (string-to-number yank-str))))))))

(evil-define-operator xiliuya/evil-replace-region (beg end type register yank-handler)
  "Add text number with line from BEG to END with TYPE.
  Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (when (and (memq type '(inclusive exclusive))
             (not (evil-visual-state-p))
             (eq 'evil-haha evil-this-operator)
             (save-excursion (goto-char beg) (bolp))
             (save-excursion (goto-char end) (eolp))
             (<= 1 (evil-count-lines beg end)))
    ;; Imitate Vi strangeness: if motion meets above criteria,
    ;; delete linewise. Not for change operator or visual state.
    (let ((new-range (evil-line-expand beg end)))
      (setq beg (car new-range)
            end (cadr new-range)
            type 'line)))
  (message ":%S_%S" beg end)
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'xiliuya/evil-replace-number beg end 't beg end))))


(provide 'init-evil)
;;; init-evil.el ends here
