;;;; -*- lexical-binding: t; -*-

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  ;; ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter
  ("node" . js2-mode)
  ;; ("node" . js2-jsx-mode)
  :config
  (setq js2-highlight-level 3)
  (setq js2-idle-timer-delay 0.5)
  ;; Use another linter instead.
  ;; https://github.com/feross/standard
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;; (use-package xref-js2) Look into this for Emacs 25.

  ;; https://www.reddit.com/r/emacs/comments/4xiaym/packages_for_javascript/
  ;; js2-mode
  ;; js2-refactor for refactorings
  ;; xref-js2 for navigation to references/definitions
  ;; jade for a REPL, inspector & stepping debugger
  ;; company-tern for autocompletion

  (add-hook 'js2-mode-hook (lambda ()
                             (when (jojo/use-2-spaces-p)
                               (setq js2-basic-offset 2))))

  (evil-define-key 'normal js-mode-map
    (kbd "gr") 'jojo/find-references
    (kbd "g.") 'js2-jump-to-definition
    (kbd "g/") 'pop-tag-mark))

(use-package ac-js2
  :ensure t
  :commands
  (ac-js2-mode)
  :init
  (defun jojo/ac-js2-hook ()
    "Sets up ac-js2."
    (ac-js2-mode)
    (jojo/company-push-backend-local 'ac-js2-company))
  (add-hook 'js2-mode-hook #'jojo/ac-js2-hook))

;;;###autoload
(defun jojo/javascript-bootstrap ()
  "Bootstrap `jojo-javascript'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/javascript-bootstrap auto-mode-alist))
  (js2-mode))

(provide 'jojo-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-javascript.el ends here
