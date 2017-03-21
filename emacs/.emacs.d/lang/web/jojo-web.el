;;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when (jojo/use-2-spaces-p)
                (setq-local web-mode-markup-indent-offset 2)
                (setq-local web-mode-css-indent-offset 2)
                (setq-local web-mode-code-indent-offset 2))))
  :config
  (global-unset-key (kbd "C-d"))
  (global-set-key (kbd "C-d") 'evil-scroll-down)
  ;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-web.el#L50
  (eval-after-load 'smartparens
    (lambda ()
      (setq web-mode-enable-auto-pairing nil)
      (sp-with-modes '(web-mode)
        (sp-local-pair "%" "%"
                       :unless '(sp-in-string-p)
                       :post-handlers '(((lambda (&rest _ignored)
                                           (just-one-space)
                                           (save-excursion (insert " ")))
                                         "SPC" "=" "#")))
        (sp-local-tag "%" "<% "  " %>")
        (sp-local-tag "=" "<%= " " %>")
        (sp-local-tag "#" "<%# " " %>")))))

;;;###autoload
(defun jojo/web-bootstrap ()
  "Bootstrap `jojo-web'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/web-bootstrap auto-mode-alist))
  (web-mode))

(provide 'jojo-web)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-web.el ends here
