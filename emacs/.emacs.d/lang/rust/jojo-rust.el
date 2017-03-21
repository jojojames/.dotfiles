;;;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

;;;###autoload
(defun jojo/rust-bootstrap ()
  "Bootstrap `jojo-rust'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/rust-bootstrap auto-mode-alist))
  (rust-mode))

(provide 'jojo-rust)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-rust.el ends here
