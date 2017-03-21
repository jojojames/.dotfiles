;;;; -*- lexical-binding: t; -*-

(evil-define-multiple
 (c-mode-map objc-mode-map c++-mode-map)
 (normal visual)
 (kbd "go") 'ff-find-other-file)

(use-package cc-mode
  :mode ("\\.c\\'" . c-mode)
  :config
  (jojo/define-make-function "all" 'c-mode)
  (defhydra hydra-c-mode (:color teal :hint nil)
    "C Mode"
    ("u" jojo/c-mode-make-all "Make")
    ("q" nil "Cancel"))

  (jojo/add-hydra '(mode) 'c-mode))

(use-package cmake-mode
  :ensure t
  :mode ("\\.cmake\\'" . cmake-mode))

(use-package dummy-h-mode
  :ensure t
  :init
  :mode ("\\.h$" . dummy-h-mode))

(use-package dtrt-indent
  :ensure t
  :diminish dtrt-indent-mode
  :commands (dtrt-indent-mode)
  :init
  ;; Linux coding style
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only))))

  (add-hook 'prog-mode-hook
            (lambda ()
              (cond
               ((derived-mode-p 'c-mode)
                (setq c-indentation-style (jojo/project-c-style))
                (cond
                 ((string-equal c-indentation-style "linux-tabs-only")
                  (setq dtrt-indent-mode nil)
                  (setq indent-tabs-mode t))
                 (t
                  (dtrt-indent-mode))))
               (t
                (setq dtrt-indent-mode nil))))))

(use-package clang-format
  :ensure t
  :commands (clang-format-buffer clang-format-region)
  :init
  (defun jojo/clang-format-and-save ()
    "Format buffer and then save buffer."
    (when (derived-mode-p 'objc-mode)
      (clang-format-buffer)
      (save-buffer)))

  (defun clang-format-region-or-buffer()
    "If clang-format is not available, do the default indenting.
Otherwise try to use clang-format. Indents region if there's a selection,
otherwise buffer is formatted."
    (interactive)
    (if (and (executable-find "clang-format")
             (locate-dominating-file default-directory ".clang-format"))
        (if (region-active-p)
            (call-interactively 'clang-format-region)
          (clang-format-buffer))
      (indent-region-or-buffer)))

  (jojo/add-hydra 'indent '(c-mode c++-mode objc-mode) #'clang-format-region-or-buffer))

(use-package irony
  ;; Run ~/.emacs.d/tools/irony_setup.sh
  :ensure t
  :commands (irony-mode irony-install-server)
  :init
  ;; Windows performance tweaks
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; Set the buffer size to 64K on Windows (from the original 4K)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :commands (company-irony)
  :init
  (setq company-irony-ignore-case t)
  (defun jojo/irony-mode-hook ()
    "Hook for irony mode."
    (jojo/company-push-backend-local '(company-irony-c-headers company-irony))
    (jojo/company-set-delay 0)
    (jojo/company-set-prefix-length 1))
  (add-hook 'irony-mode-hook #'jojo/irony-mode-hook))

(use-package company-irony-c-headers
  :ensure t
  :commands (company-irony-c-headers))

(use-package flycheck-irony
  :ensure t
  :commands (flycheck-irony-setup)
  :init
  (add-hook 'irony-mode-hook
            (lambda ()
              (eval-after-load 'flycheck
                '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

(provide 'jojo-c)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-c.el ends here
