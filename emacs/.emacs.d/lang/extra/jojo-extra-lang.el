;;;; -*- lexical-binding: t; -*-

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

;; Json
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :config
  (defhydra hydra-json-mode (:color blue)
    "Json"
    ("u" json-mode-beautify "Beautify"))
  (jojo/add-hydra 'mode 'json-mode))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode))

;; Vimscript
(use-package vimrc-mode
  :ensure t
  :mode ("\\.vimrc\\'" . vimrc-mode))

;; HTTP
(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :config
  (defhydra hydra-restclient-mode (:color blue)
    "HTTP"
    ("e" restclient-http-send-current "Send Current")
    ("r" restclient-http-send-current-raw "Send Raw")
    ("v" restclient-http-send-current-stay-in-window "Send Current Stay")
    ("n" restclient-jump-next "Jump Next")
    ("p" restclient-jump-prev "Jump Prev")
    ("c" restclient-copy-curl-command "Copy Curl"))
  (jojo/add-hydra 'mode 'restclient-mode))

;; Shell
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(use-package nxml-mode
  :ensure nil
  :mode
  ("\\.xml\\'" . nxml-mode)
  :init
  (add-hook 'nxml-mode-hook '(lambda ()
                               (setq tab-width 4)))
  :config
  (setq nxml-child-indent 4))

(use-package make-mode
  :ensure nil
  :config
  (jojo/define-make-function ("all" "test") 'makefile-mode)

  (defhydra hydra-makefile-mode (:color teal :hint nil)
    "C Mode"
    ("u" jojo/makefile-mode-make-all "Make")
    ("t" jojo/makefile-mode-make-test "Make Test")
    ("q" nil "Cancel"))

  (jojo/add-hydra '(mode) 'makefile-mode))

(provide 'jojo-extra-lang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-extra-lang.el ends here
