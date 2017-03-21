;;;; -*- lexical-binding: t; -*-

(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;;;###autoload
(defun jojo/php-bootstrap ()
  "Bootstrap `jojo-php'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/php-bootstrap auto-mode-alist))
  (php-mode))

(provide 'jojo-php)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-php.el ends here
