;;;; -*- lexical-binding: t; -*-

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :config
  ;; Disable for now while trying grizzl.
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-x") 'counsel-M-x))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :config
  (if (jojo/imac-p)
      (setq ivy-flx-limit 100)
    (setq ivy-flx-limit 500))
  ;; (ivy-mode)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  (setq ivy-initial-inputs-alist nil)

  ;; swapping behavior
  (define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  ;; default: "ag --nocolor --nogroup %s -- ."
  (setq counsel-ag-base-command "ag -U --nocolor --nogroup %s -- .")
  (setq ivy-count-format "")
  (setq ivy-height 15))

(use-package flx
  :ensure t)

(use-package smex
  :ensure t)

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (setq ag-ignore-list '(;; Binary Directories
                         "obj/"
                         "bin/"
                         ;; Elisp
                         "backups/*"
                         "elpa/archives/*"
                         "elpa/archives/melpa/archive-contents"
                         "company-cache.el"
                         "company-cache.el~"
                         "company-statistics-cache.el"
                         "project-explorer-cache/*"
                         "projectile.cache"
                         ;; Xamarin
                         "assets/"
                         ;; Android
                         "build/"
                         "target/"
                         ;; iOS
                         "compile_commands.json"
                         ;; CSharp
                         "Resources/Resource.designer.cs"))
  (add-to-list 'ag-arguments "-U") ;; ignore .ignore files by default
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t))

(use-package projectile
  :ensure t
  :commands (projectile-project-p
             projectile-project-root
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-ag
             projectile-recentf
             jojo/projectile-find)
  :diminish projectile-mode
  :init
  (setq projectile-file-exists-local-cache-expire (* 5 60))
  (when (jojo/windows-p)
    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t))
  (defun jojo/projectile-hydra-root ()
    "Return path `hydra-projectile' can print in heading."
    (if (projectile-project-p)
        (projectile-project-root)
      "Not in Project"))

  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f" projectile-find-file-other-window "File")
    ("g" projectile-find-file-dwim-other-window "File DWIM")
    ("d" projectile-find-dir-other-window "Directory")
    ("b" projectile-switch-to-buffer-other-window "Buffer")
    ("q" nil "Cancel" :color blue))
  (defhydra hydra-projectile (:color teal :hint nil)
    "
     Projectile: %(jojo/projectile-hydra-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
 _fF_: File            _a_: Ag                _i_: Ibuffer           _c_: Clear Cache
 _ff_: File DWIM       _g_: Update Gtags      _b_: Switch to Buffer  _x_: Remove Known Project
 _fd_: Current Dir..   _o_: Multi Occur       _K_: Kill all Buffers  _X_: Cleanup Non-existing
 _fr_: Recent          _T_: Regenerate Tags                        _z_: Cache Current
  _d_: Directory       _r_: Find and Replace
                     _R_: Find and Replace Regexp
"
    ("T" projectile-regenerate-tags)
    ("a" projectile-ag)
    ("b" projectile-switch-to-buffer)
    ("c" projectile-invalidate-cache)
    ("d" projectile-find-dir)
    ("fF" projectile-find-file)
    ("ff" projectile-find-file-dwim)
    ("fd" projectile-find-file-in-directory)
    ("fr" projectile-recentf)
    ("g" ggtags-update-tags)
    ("i" projectile-ibuffer)
    ("K" projectile-kill-buffers)
    ("m" projectile-multi-occur)
    ("o" projectile-multi-occur)
    ("p" projectile-switch-project "Switch Project")
    ("r" projectile-replace)
    ("R" projectile-replace-regexp)
    ("s" projectile-switch-project "Switch Project")
    ("x" projectile-remove-known-project)
    ("X" projectile-cleanup-known-projects)
    ("z" projectile-cache-current-file)
    ("`" hydra-projectile-other-window/body "Other Window")
    ("q" nil "Cancel" :color blue))
  :config
  (defvar jojo/projectile-invalidate-cache-timer nil)
  (defun jojo/projectile-schedule-invalidate-cache (orig-fun &rest args)
    "Schedule invalidating project cache when idle."
    (when jojo/projectile-invalidate-cache-timer
      (cancel-timer jojo/projectile-invalidate-cache-timer))
    (let ((result (apply orig-fun args))
          (jojo/projectile-invalidate-cache-timer
           (run-with-idle-timer 3 nil
                                (lambda ()
                                  (call-interactively #'projectile-invalidate-cache)))))
      result))
  (advice-add 'alchemist-mix :around #'jojo/projectile-schedule-invalidate-cache)

  (setq projectile-enable-caching t)
  (projectile-mode)
  (eval-after-load 'ivy
    (lambda ()
      (setq projectile-completion-system 'ivy))))

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        ;; https://www.reddit.com/r/emacs/comments/3g468d/stop_recent_files_showing_elpa_packages/
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; git contents
                              "/elpa/.*\\'"   ; package files
                              ".*\\.gz\\'"
                              "TAGS"
                              "company-statistics-cache.el"
                              "company-cache.el"
                              ".*-autoloads\\.el\\'"
                              ;; John Wiegley
                              "~\\'"
                              "\\`out\\'"
                              "\\.log\\'"
                              "^/[^/]*:"
                              "\\.el\\.gz\\'"))
  (recentf-mode 1))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-auto-save-directory "~/.backups"))

(provide 'jojo-project)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-project.el ends here
