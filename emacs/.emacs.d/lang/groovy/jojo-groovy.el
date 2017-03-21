;;;; -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode
  ("\\.gradle\\'" . groovy-mode)
  ("\\.groovy\\'" . groovy-mode))

;;;###autoload
(defun jojo/groovy-bootstrap ()
  "Bootstrap `jojo-groovy'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/groovy-bootstrap auto-mode-alist))
  (groovy-mode))

(provide 'jojo-groovy)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-groovy.el ends here
