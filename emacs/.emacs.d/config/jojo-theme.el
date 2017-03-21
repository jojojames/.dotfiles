;;;; -*- lexical-binding: t; -*-

(use-package gotham-theme :defer :ensure t)
(use-package moe-theme :defer :ensure t)
(use-package color-theme-solarized :defer :ensure t)
(use-package github-theme :defer :ensure t)

(use-package apropospriate-theme :defer :ensure t
  :init
  (setq apropospriate-mode-line-height 1.0))

(use-package spacemacs-theme
  :defer
  :ensure t
  :init
  (setq spacemacs-theme-comment-bg nil))

;; https://stackoverflow.com/questions/9840558/why-cant-emacs-24-find-a-custom-theme-i-added
;; Add wildcard matching to themes in elpa folder.
(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

(use-package theme-changer
  :ensure t
  :init
  ;; Dallas
  (setq calendar-latitude 32.85)
  (setq calendar-longitude -96.85)
  ;; Japan
  ;; (setq calendar-latitude 36.3147)
  ;; (setq calendar-longitude 139.8)
  :config
  (defun jojo/make-modeline-taller ()
    "Make the mode line taller."
    (dolist (sym '(mode-line mode-line-inactive))
      (set-face-attribute
       sym nil
       :box `(:line-width 5 :color ,(face-attribute `,sym :background)))))

  (defun jojo/daytime-p ()
    "Figuring out day or night."
    (let*
        ((today-times
          (theme-changer-sunrise-sunset-times (theme-changer-today)))
         (sunrise-today (first today-times))
         (sunset-today (second today-times)))
      (theme-changer-daytime-p sunrise-today sunset-today)))

  (defun jojo/update-theme (&rest _args)
    "Update various UI elements when theme changes"
    (jojo/make-modeline-taller)
    (eval-after-load 'org-mode
      (lambda ()
        (jojo/customize-org-ui)))
    (when (fboundp 'org-reload)
      (eval-after-load 'org-faces
        (lambda ()
          (set-face-background 'org-hide (face-attribute 'default :background))
          (set-face-foreground 'org-hide (face-attribute 'default :background))))
      (when (let (has-org-mode)
              (dolist (b (buffer-list) has-org-mode)
                (with-current-buffer b
                  (when (eq major-mode 'org-mode)
                    (setq has-org-mode t)))))
        (org-reload)))
    (eval-after-load 'company
      (lambda ()
        (cond
         ((eq 'zonokai-blue jojo/current-theme)
          (set-face-attribute
           'company-preview-common
           nil
           :background
           (face-attribute 'company-preview :background)))
         (t
          (set-face-attribute
           'company-preview
           nil
           :background
           (face-attribute 'company-preview-common :background)))))))

  (advice-add 'change-theme :after #'jojo/update-theme)
  (set-frame-parameter nil 'background-mode 'dark)

  (defvar jojo/current-theme nil "Current theme.")

  ;; Redefine theme-changer-switch-theme to track current theme.
  (defun theme-changer-switch-theme (old new)
    "Change the theme from OLD to NEW, using Emacs 24's built-in
theme facility (\"deftheme\").

If NEW is set to nil, shall switch to default Emacs theme."
    (disable-theme old)
    (setq jojo/current-theme new)
    (when new
      (load-theme new t)))

  (cond
   ((jojo/osx-p)
    (change-theme 'dichromacy 'dichromacy))
   ((jojo/linux-p)
    (change-theme 'spacemacs-light 'spacemacs-dark))
   (t
    (change-theme 'moe-light 'moe-dark))))

;; Use colorful parens.
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Diminish modeline clutter.
(when (require 'diminish nil 'noerror)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev-mode)
  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode))
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode))
  (eval-after-load "autorevert"
    '(diminish 'auto-revert-mode)))

(provide 'jojo-theme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-theme.el ends here
