;;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;;###autoload
(defun jojo/haskell-bootstrap ()
  "Bootstrap `jojo-haskell'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/haskell-bootstrap auto-mode-alist))
  (haskell-mode))

(provide 'jojo-haskell)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-haskell.el ends here
