;;;; -*- lexical-binding: t; -*-

;; Silence ad-handle-definition about advised functions getting redefined.
(eval-after-load 'evil
  (lambda ()
    (setq ad-redefinition-action 'accept)))

;; Disable startup screen.
(setq inhibit-startup-screen t)

;; Mute system sound.
(setq ring-bell-function #'ignore)

;; Set default frame size.
(when (jojo/macbook-retina-p)
  (setq initial-frame-alist '((width . 90) (height . 45))))

(when (jojo/imac-p)
  (setq initial-frame-alist '((width . 132) (height . 86))))

(when (jojo/windows-p)
  (setq initial-frame-alist '((width . 92) (height . 46))))

;; Set title of window to current file.
(setq frame-title-format '("%f"))

(use-package nlinum
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook
                  nxml-mode-hook))
    (add-hook hook #'nlinum-mode)))

;; Save window configurations.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; https://superuser.com/questions/1133436/way-too-fast-scrolling-in-emacs-on-osx
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Disable pixel scrolling on Yamamoto's port.
(setq mac-mouse-wheel-mode nil)
(setq mac-mouse-wheel-smooth-scroll nil)

;; Wrap line when it reaches end.
(setq-default truncate-lines 1)
(global-visual-line-mode 0)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Hide the comments too when doing `hs-hide-all'.
(setq hs-hide-comments nil)

;; Set whether isearch opens folded comments, code, or both.
(setq hs-isearch-open t)

;; Enable transient mark mode.
(transient-mark-mode 1)

;; Kills entire line if at the beginning.
(setq kill-whole-line t)
;; yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the column number show up.
(column-number-mode 1)
;; Don't ask to kill compilation buffer.
(setq compilation-always-kill t)

;; Let Emacs color Ansi symbols.
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun jojo/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'jojo/colorize-compilation-buffer))

;; emacs 24+ auto indents by default if `electric-indent-mode' is on
;; so disable automatic indent by default
;; but enable it in all programming modes.
(electric-indent-mode 0)

(dolist (mode '(prog-mode-hook
                yaml-mode-hook
                css-mode-hook
                html-mode-hook
                nxml-mode-hook))
  (add-hook mode (lambda ()
                   (interactive)
                   (electric-indent-local-mode 1))))

;; Indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)
(setq c-default-style
      '((java-mode . "intellij")
        (awk-mode . "awk")
        (c-mode . "k&r")
        (csharp-mode . "csharp")
        (other . "gnu")))

;; http://emacs.stackexchange.com/questions/508/how-to-configure-specific-java-indentation
(c-add-style
 "intellij"
 '("Java"
   (c-basic-offset . 4)
   (c-offsets-alist
    (inline-open . 0)
    (topmost-intro-cont    . +)
    (statement-block-intro . +)
    (knr-argdecl-intro     . 5)
    (substatement-open     . +)
    (substatement-label    . +)
    (label                 . +)
    (statement-case-open   . +)
    (statement-cont        . ++)
    (arglist-intro  . +)
    (arglist-close . c-lineup-arglist)
    (access-label   . 0)
    (inher-cont     . ++)
    (func-decl-cont . ++))))

;; https://github.com/josteink/csharp-mode/issues/72
;; https://emacs.stackexchange.com/questions/22490/how-do-i-prevent-indentation-of-braces-in-function-arguments-with-csharp-mode

(c-add-style
 "csharp"
 '("Java"
   (c-basic-offset . 4)
   (c-comment-only-line-offset 0 . 0)
   (c-offsets-alist
    (access-label . -)
    (arglist-close . c-lineup-arglist)
    (arglist-cont . 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-intro  . +)
    (block-close . 0)
    (block-open . 0)
    (brace-entry-open . 0)
    (brace-list-close . 0)
    (brace-list-entry . 0)
    (brace-list-intro . +)
    (brace-list-open . 0)
    (c . c-lineup-C-comments)
    (case-label . +)
    (catch-clause . 0)
    (class-close . 0)
    (class-open . 0)
    (comment-intro . c-lineup-comment)
    (cpp-macro . 0)
    (cpp-macro-cont . c-lineup-dont-change)
    (defun-block-intro . +)
    (defun-close . 0)
    (defun-open . 0)
    (do-while-closure . 0)
    (else-clause . 0)
    (extern-lang-close . 0)
    (extern-lang-open . 0)
    (friend . 0)
    (func-decl-cont . 0)
    (inclass . +)
    (inexpr-class . 0)
    (inexpr-statement . 0)
    (inextern-lang . +)
    (inher-cont . c-lineup-multi-inher)
    (inher-intro . +)
    (inlambda . c-lineup-inexpr-block)
    (inline-close . 0)
    (inline-open . 0)
    (innamespace . +)
    (knr-argdecl . 0)
    (knr-argdecl-intro . 5)
    (label . 0)
    (lambda-intro-cont . +)
    (member-init-cont . c-lineup-multi-inher)
    (member-init-intro . +)
    (namespace-close . 0)
    (namespace-open . 0)
    (statement . 0)
    (statement-block-intro . +)
    (statement-case-intro . +)
    (statement-case-open . +)
    (statement-cont . +)
    (stream-op . c-lineup-streamop)
    (string . c-lineup-dont-change)
    (substatement . 0)
    (substatement-label . +)
    (substatement-open . 0)
    (template-args-cont c-lineup-template-args +)
    (topmost-intro . 0)
    (topmost-intro-cont . 0))))

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(add-hook 'prog-mode-hook 'eldoc-mode)

;; Reverting buffers
(global-auto-revert-mode t) ; automatically reload buffers on change

;; Highlight matching parentheses.
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;; Quit Emacs without confirming.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (dolist (process-name '("mu4e-update"
                          "Omni-Server"
                          "terminal<1>"
                          "terminal<2>"
                          "terminal<3>"
                          "nrepl-connection"
                          "nrepl-server"
                          "Python"
                          "*alchemist mix*"
                          "Alchemist-IEx"
                          "compilation"))
    (let ((process (or
                    (get-buffer-process process-name)
                    (get-process process-name))))
      (when process
        (set-process-query-on-exit-flag process nil))))
  ad-do-it)

;; No Autosave by default
(setq auto-save-default nil)

;; Write backup files to own directory.
;; https://www.emacswiki.org/emacs/BackupDirectory
(defvar jojo/backup-directory (expand-file-name (concat user-emacs-directory "backups"))
  "Location of backup directory.")

(setq backup-directory-alist
      `((".*" . ,jojo/backup-directory)))

;; Purge old backups.
(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 5 nil #'jojo/delete-backups)))

(defun jojo/delete-backups ()
  "Delete backups."
  (lambda ()
    (let ((week (* 60 60 24 7))
          (current (float-time (current-time))))
      (dolist (file (directory-files jojo/backup-directory t))
        (when (and (backup-file-name-p file)
                   (> (- current (float-time (nth 5 (file-attributes file))))
                      week))
          (message "Deleted backup %s" file)
          (delete-file file))))))

;; Start daemon automatically.
(add-hook 'after-init-hook (lambda ()
                             (unless (jojo/windows-p)
                               (load "server") ;; server-running-p is not autoloaded.
                               (unless (server-running-p)
                                 (server-start)))))

;; Don't make backups of files in version control.
(setq vc-make-backup-files nil)

;; Same filenames get the directory name inserted also.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; Backspace like Intellij
(dolist (hook '(prog-mode-hook
                alchemist-iex-mode
                org-mode-hook
                css-mode-hook
                html-mode-hook
                yaml-mode-hook))
  (add-hook hook (lambda ()
                   (enable-or-disable-intellij-backspace t))))

(dolist (hook '(python-mode-hook))
  (add-hook hook (lambda ()
                   (enable-or-disable-intellij-backspace nil))))

;; Set this variable to a non-nil value to speed up display of characters
;; using large fonts, at the price of a larger memory footprint of the
;; Emacs session.
(setq inhibit-compacting-font-caches t)

;; Set up file where emacs customizations go.
(setq custom-file "~/.emacs.d/config/jojo-custom.el")
(load custom-file)

(setq source-directory "~/.source/emacs/")

(provide 'jojo-defaults)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-defaults.el ends here
