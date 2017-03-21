;;;; -*- lexical-binding: t; -*-

(autoload 'magit-toplevel "magit.el" nil t)
(autoload 'magit-status "magit.el" nil t)
(autoload 'magit-blame "magit.el" nil t)
(autoload 'magit-log "magit.el" nil t)

(autoload 'hydra-magit/body "jojo-git.el" nil t)

(use-package magit
  :ensure t
  :commands (magit-toplevel magit-status magit-blame magit-log)
  :init
  (defun magit-status-pick-repository ()
    "Calls magit-status with a prefix argument to allow picking the repository."
    (interactive)
    (let ((current-prefix-arg '(4))) ; C-u
      (call-interactively 'magit-status)))
  (defhydra hydra-magit (:color teal :columns 2)
    "
   Magit: %(magit-toplevel)
"
    ("b" magit-blame "Blame")
    ("ll" magit-log "Log")
    ("lc" magit-log-current "Log Current Branch")
    ("lh" magit-log-head "Log Head")
    ("lb" magit-log-buffer-file "Log Buffer")
    ("lB" magit-log-buffer-file-popup "Log Buffer Popup")
    ("p" magit-list-repositories "List Repositories")
    ("r" magit-status-pick-repository "Pick Repository")
    ("s" magit-status "Status")
    ("q" nil "Cancel" :color blue))
  :config
  (eval-after-load 'evil
    (lambda ()
      (evil-define-key 'normal magit-log-mode-map
        (kbd "`") 'magit-process-buffer
        (kbd "~") 'magit-diff-default-context
        (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
        (kbd "$") 'evil-end-of-line)
      (evil-define-key 'normal magit-status-mode-map
        (kbd "q") 'quit-window
        (kbd "`") 'magit-process-buffer
        (kbd "~") 'magit-diff-default-context
        (kbd "0") 'evil-digit-argument-or-evil-beginning-of-line
        (kbd "$") 'evil-end-of-line
        (kbd "Q") 'delete-window)
      (evil-define-key 'normal magit-repolist-mode-map
        (kbd "q") 'quit-window
        (kbd "Q") 'delete-window
        (kbd "RET") 'magit-repolist-status
        (kbd "gr") 'magit-list-repositories)))

  ;; Save buffers automatically instead of asking.
  (setq magit-save-repository-buffers 'dontask)

  (setq magit-repository-directories '("~/Developer"
                                       "~/Code"
                                       "~/.emacs.d"
                                       "~/.vim"
                                       "~/.dotfiles"
                                       "~/.zsh"))
  (setq magit-refresh-status-buffer nil)
  (eval-after-load 'ivy
    (lambda ()
      (setq magit-completing-read-function 'ivy-completing-read)))
  (setq vc-handled-backends (delq 'Git vc-handled-backends))

  (defadvice magit-show-commit (around dont-select-commit-window activate)
    "magit-show-commit selects the window it opens unless magit-display-buffer-noselect is set.
Setting magit-display-buffer-noselect changes the selection logic for other parts of magit though.
Instead, advise magit-show-commit by setting magit-show-commit to t
before calling magit-show-commit and set it back to nil afterwards."
    (setq magit-display-buffer-noselect t)
    (setq ad-return-value ad-do-it)
    (setq magit-display-buffer-noselect nil))

  ;; https://github.com/magit/magit/issues/2541 (tweaked)
  ;; single window or special magit modes -> open in other window
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer
           (cond
            ((eq (count-windows) 1)
             nil)
            ((and (derived-mode-p 'magit-mode)
                  (eq (with-current-buffer buffer major-mode)
                      'magit-status-mode))
             nil)
            ((memq (with-current-buffer buffer major-mode)
                   '(magit-process-mode
                     magit-revision-mode
                     magit-diff-mode
                     magit-stash-mode))
             nil)
            (t
             '(display-buffer-same-window))))))

  ;; Add rebase argument to pull
  ;; https://github.com/magit/magit/issues/2597
  (magit-define-popup-switch 'magit-pull-popup ?R "Rebase" "--rebase")

  ;;; Improve Bisect

  ;; Clear bisect variables on these hooks
  (add-hook 'magit-status-mode-hook #'jojo/magit-bisect-reset-state)
  (add-hook 'magit-log-mode-hook #'jojo/magit-bisect-reset-state)

  (defun jojo/magit-mark-commit-as-bad (revision)
    (interactive (list (magit-read-other-branch-or-commit "Mark bad commit")))
    (if (boundp 'jojo/magit-bad-revision)
        (setq jojo/magit-bad-revision revision)
      (defvar jojo/magit-bad-revision revision "The bad revision to use in git bisect"))
    (jojo/magit-remove-bisect-mark-popup-actions)
    (jojo/magit-add-bisect-mark-popup-actions))

  (defun jojo/magit-mark-commit-as-good (revision)
    (interactive (list (magit-read-other-branch-or-commit "Mark good commit")))
    (if (boundp 'jojo/magit-good-revision)
        (setq jojo/magit-good-revision revision)
      (defvar jojo/magit-good-revision revision "The good revision to use in git bisect"))
    (jojo/magit-remove-bisect-mark-popup-actions)
    (jojo/magit-add-bisect-mark-popup-actions))

  (defun jojo/magit-bisect-reset-state ()
    "Reset bad and good git bisect variables"
    (interactive)
    (jojo/magit-remove-bisect-mark-popup-actions)
    (when (boundp 'jojo/magit-good-revision)
      (makunbound 'jojo/magit-good-revision))
    (when (boundp 'jojo/magit-bad-revision)
      (makunbound 'jojo/magit-bad-revision))
    (jojo/magit-add-bisect-mark-popup-actions))

  (defun jojo/magit-remove-bisect-mark-popup-actions ()
    "Remove popup actions to select bad and good commits"
    (magit-remove-popup-key 'magit-bisect-popup :action ?X)
    (magit-remove-popup-key 'magit-bisect-popup :action ?Z)
    (when (and (bound-and-true-p jojo/magit-good-revision)
               (bound-and-true-p jojo/magit-bad-revision))
      (magit-remove-popup-key 'magit-bisect-popup :action ?C)))

  (defun jojo/magit-add-bisect-mark-popup-actions ()
    "Add popup actions to select bad and good commits"
    (let ((bad-str (if (bound-and-true-p jojo/magit-bad-revision)
                       (concat "Marked bad commit: " jojo/magit-bad-revision)
                     "Mark bad commit"))
          (good-str (if (bound-and-true-p jojo/magit-good-revision)
                        (concat "Marked good commit: " jojo/magit-good-revision)
                      "Mark good commit")))
      (magit-define-popup-action 'magit-bisect-popup ?X bad-str #'jojo/magit-mark-commit-as-bad)
      (magit-define-popup-action 'magit-bisect-popup ?Z good-str #'jojo/magit-mark-commit-as-good)
      (when (and (bound-and-true-p jojo/magit-good-revision)
                 (bound-and-true-p jojo/magit-bad-revision))
        (magit-define-popup-action 'magit-bisect-popup
          ?C (concat "Start: bad commit: " jojo/magit-bad-revision
                     " good commit: " jojo/magit-good-revision)
          #'jojo/magit-bisect-with-good-bad-commits))))

  (defun jojo/magit-bisect-with-good-bad-commits ()
    "Run magit-bisect with `jojo/magit-bad-revision' and `jojo/magit-good-revision'"
    (interactive)
    (magit-bisect-start jojo/magit-bad-revision jojo/magit-good-revision))

  (jojo/magit-add-bisect-mark-popup-actions))

(provide 'jojo-git)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-git.el ends here
