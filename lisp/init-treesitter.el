;; Install third-party treesitter-based modes
(require-package 'clojure-ts-mode)


;; You can download per-architecture pre-compiled release from
;; https://github.com/emacs-tree-sitter/tree-sitter-langs Rename
;; contained grammars to add prefix "libtree-sitter-", place in
;; ~/.emacs.d/tree-sitter.
;;
;; Nix users can pre-install all grammars alongside their Emacs, see
;; https://github.com/nix-community/emacs-overlay/issues/341
;;
;; Note that grammar files from different sources can be differently
;; named and configured, so there could be different results. Some
;; common remappings are included below.
(setq treesit-load-name-override-list nil
      major-mode-remap-alist nil)

(defun sanityinc/auto-configure-treesitter ()
  "Find and configure installed grammars, remap to matching -ts-modes if present.
Return a list of languages seen along the way."
  (let ((grammar-name-to-emacs-lang '(("c-sharp" . "csharp")
                                      ("cpp" . "c++")
                                      ("gomod" . "go-mod")
                                      ("javascript" . "js")))
        seen-grammars)
    (dolist (dir (cons (expand-file-name "tree-sitter" user-emacs-directory)
                       treesit-extra-load-path))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir))
          (let ((fname (file-name-sans-extension (file-name-nondirectory file))))
            (when (string-match "libtree-sitter-\\(.*\\)" fname)
              (let* ((file-lang (match-string 1 fname))
                     (emacs-lang (or (cdr (assoc-string file-lang grammar-name-to-emacs-lang)) file-lang)))
                ;; Override library if its filename doesn't match the Emacs name
                (unless (or (memq (intern emacs-lang) seen-grammars)
                            (string-equal file-lang emacs-lang))
                  (let ((libname (concat "tree_sitter_" (replace-regexp-in-string "-" "_" file-lang))))
                    (add-to-list 'treesit-load-name-override-list
                                 (list (intern emacs-lang) fname libname))))
                ;; If there's a corresponding -ts mode, remap the standard mode to it
                (let ((ts-mode-name (intern (concat emacs-lang "-ts-mode")))
                      (regular-mode-name (intern (concat emacs-lang "-mode"))))
                  (when (fboundp ts-mode-name)
                    (add-to-list 'major-mode-remap-alist
                                 (cons regular-mode-name ts-mode-name))))
                ;; Remember we saw this language so we don't squash its config when we
                ;; find another lib later in the treesit load path
                (push (intern emacs-lang) seen-grammars)))))))
    seen-grammars))

(sanityinc/auto-configure-treesitter)

;; When there's js-ts-mode, we prefer it to js2-mode
(when-let ((jsmap (alist-get 'js-mode major-mode-remap-alist)))
  (add-to-list 'major-mode-remap-alist (cons 'js2-mode jsmap)))

(when-let ((jsmap (alist-get 'clojure-mode major-mode-remap-alist)))
  (add-to-list 'major-mode-remap-alist (cons 'clojurescript-mode 'clojurescript-ts-mode)))



;; Default
(setq treesit-font-lock-level 4)



(provide 'init-treesitter)
