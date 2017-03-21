(use-package rtags
  :ensure t
  :commands (rtags-start-process-unless-running)
  :init
  (add-hook 'objc-mode-hook #'jojo/rtags-setup)
  (add-hook 'c-mode-hook #'jojo/rtags-setup)
  (add-hook 'c++-mode-hook #'jojo/rtags-setup)
  (jojo/evil-default-state 'motion '(rtags-mode
                                     rtags-references-tree-mode
                                     rtags-dependency-tree-mode))
  (defun jojo/rtags-setup ()
    "Set up rtags."
    (unless (or
             (derived-mode-p 'csharp-mode)
             (not (executable-find "rtags")))
      (setq rtags-path "~/.bin/rtags/bin")
      (rtags-start-process-unless-running)
      (setq rtags-autostart-diagnostics t)
      (rtags-diagnostics)))
  :config
  ;; Rewriting rtags keybindings
  (define-key rtags-mode-map (kbd "RET") nil)
  (define-key rtags-mode-map (kbd "M-RET") nil)
  (define-key rtags-mode-map [mouse-1] nil)
  (define-key rtags-mode-map [mouse-2] nil)
  (define-key rtags-mode-map (kbd "M-o") nil)
  (define-key rtags-mode-map (kbd "s") nil)
  (define-key rtags-mode-map (kbd "SPC") nil)
  (define-key rtags-mode-map (kbd "q") nil)
  (define-key rtags-mode-map (kbd "j") nil)
  (define-key rtags-mode-map (kbd "k") nil)
  (define-key rtags-mode-map (kbd "n") nil)
  (define-key rtags-mode-map (kbd "p") nil)
  (evil-add-hjkl-bindings rtags-mode-map 'normal)
  (evil-define-key 'motion rtags-mode-map
    (kbd "q") 'rtags-bury-or-delete
    (kbd "RET") 'rtags-select-other-window
    (kbd "M-RET") 'rtags-select)

  (defun xcode-index-project-with-rtags ()
    "Index xcode project with rtags."
    (interactive)
    (xcode-in-root
     (compile "~/.bin/rtags/bin/rc -J")))

  (defhydra hydra-rtags-mode (:color blue :columns 3)
    "RTags"
    ("i" xcode-index-project-with-rtags)
    ("R" rtags-restart-process)
    ("Q" rtags-quit-rdm)
    ("f" rtags-fixit)
    ("ps" rtags-print-symbol-info)
    ("pt" rtags-symbol-type)
    ("pd" rtags-print-dependencies)
    ("r" rtags-rename-symbol)
    ("pp" rtags-reparse-file)
    ("z" rtags-show-rtags-buffer)))

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :commands (ggtags-mode)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  :config
  (evil-define-key 'normal ggtags-global-mode-map
    (kbd "h") 'evil-backward-char)
  (ggtags-mode 1))

(use-package etags-select
  :ensure t
  :commands (etags-select-find-tag-at-point)
  :config
  (setq etags-select-use-short-name-completion t))

(defun use-rtags (&optional useFileManager)
  "Check if project is indexed."
  (and (rtags-executable-find "rc")
       (cond
        ((and (not (eq major-mode 'c++-mode))
              (not (eq major-mode 'c-mode)))
         (rtags-has-filemanager))
        (useFileManager
         (rtags-has-filemanager))
        (t
         (rtags-is-indexed)))))

(defun jojo/c-find-symbol-at-point (&optional prefix)
  "DWIM Find Symbol."
  (interactive "P")
  (cond
   ((and (not (derived-mode-p 'java-mode))
         (and (rtags-find-symbol-at-point prefix)
              (not rtags-last-request-not-indexed)))
    (push 'rtags jojo/c-navigation-stack))
   ((bound-and-true-p ggtags-mode)
    (push 'ggtags jojo/c-navigation-stack)
    (call-interactively 'ggtags-find-tag-dwim))
   (t
    (when (etags-select-find-tag-at-point)
      (push 'etags jojo/c-navigation-stack)))))

(defun jojo/c-pop-symbol-at-point ()
  "Return to previous point in navigation stack."
  (interactive)
  (let ((method (pop jojo/c-navigation-stack)))
    (cond
     ((eq method 'rtags)
      (rtags-location-stack-back))
     ((eq method 'ggtags)
      (ggtags-prev-mark))
     ((eq method 'etags)
      (pop-tag-mark))
     (t
      (message "No tag found to pop.")))))

(defun jojo/c-find-references-at-point (&optional prefix)
  "DWIM Find References."
  (interactive "P")
  (cond
   ((and (not (derived-mode-p 'java-mode))
         (and (rtags-find-references-at-point prefix)
              (not rtags-last-request-not-indexed)))
    (push 'rtags jojo/c-navigation-stack))
   ((bound-and-true-p ggtags-mode)
    (push 'ggtags jojo/c-navigation-stack)
    (call-interactively 'ggtags-find-reference))
   (t
    (jojo/find-references))))

(defun jojo/c-find-symbol ()
  "DWIM Find Symbol."
  (interactive)
  (cond
   ((and (not (derived-mode-p 'java-mode)) (use-rtags))
    (push 'rtags jojo/c-navigation-stack)
    (call-interactively 'rtags-find-symbol))
   ((bound-and-true-p ggtags-mode)
    (call-interactively 'ggtags-find-tag-dwim))
   (t
    (push 'etags jojo/c-navigation-stack)
    (call-interactively 'etags-select-find-tag-at-point))))

(defun jojo/c-find-references ()
  "DWIM Find References."
  (interactive)
  (cond
   ((and (not (derived-mode-p 'java-mode))
         (use-rtags))
    (push 'rtags jojo/c-navigation-stack)
    (call-interactively 'rtags-find-references))
   ((bound-and-true-p ggtags-mode)
    (push 'ggtags jojo/c-navigation-stack)
    (call-interactively 'ggtags-find-reference))
   (t
    (jojo/find-references))))

(defun jojo/c-find-file ()
  "DWIM Find File."
  (interactive)
  (cond
   ((and (not (derived-mode-p 'java-mode))
         (use-rtags))
    (call-interactively 'rtags-find-file))
   ((bound-and-true-p ggtags-mode)
    (call-interactively 'ggtags-find-file))
   (t
    (call-interactively 'find-file-at-point))))

(defun jojo/c-find-virtuals ()
  "DWIM Find Virtuals."
  (interactive)
  (rtags-find-virtuals-at-point))

(defun jojo/c-imenu ()
  "DWIM Imenu."
  (interactive)
  (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

(defun jojo/c-setup ()
  "Set up common c modes."
  (defvar jojo/c-navigation-stack '() "Stack used to navigate tags.")
  (add-hook 'c-mode-common-hook
            (lambda ()
              (evil-define-key 'normal c-mode-base-map
                (kbd "g.") 'jojo/c-find-symbol-at-point
                (kbd "g/") 'jojo/c-pop-symbol-at-point
                (kbd "gr") 'jojo/c-find-references-at-point
                (kbd "gR") 'jojo/c-find-references
                (kbd "gd") 'jojo/c-find-symbol
                (kbd "gv") 'jojo/c-find-virtuals
                (kbd "gf") 'jojo/c-find-file))))

(jojo/c-setup)

(provide 'jojo-tags)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-tags.el ends here
