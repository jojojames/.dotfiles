;;;; -*- lexical-binding: t; -*-

(use-package swift-mode
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :config
  ;; (add-to-list 'flycheck-checkers 'swift)
  (evil-define-key 'normal swift-mode-map
    (kbd "g/") 'jojo/find-references)
  (defhydra hydra-swift (:color blue :columns 3)
    "Swift"
    ("ez" swift-mode-run-repl)
    ("eb" swift-mode-send-buffer)
    ("er" swift-mode-send-region))
  (jojo/add-hydra 'mode 'swift-mode)
  (define-key swift-repl-mode-map [(shift return)] 'evil-jump-forward))

(use-package company-sourcekit
  :ensure t
  :mode ("\\.swift\\'" . swift-mode)
  :init
  ;; (add-hook 'swift-mode-hook #'jojo/company-setup-sourcekit)
  (defun jojo/company-setup-sourcekit ()
    "Setting up company sourcekit."
    (jojo/company-push-backend-local 'company-sourcekit))
  :config
  (setq company-sourcekit-verbose t)
  (setq sourcekit-verbose t))

;;;###autoload
(defun jojo/swift-bootstrap ()
  "Bootstrap `jojo-swift'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/swift-bootstrap auto-mode-alist))
  (swift-mode))

(provide 'jojo-swift)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-swift.el ends here
