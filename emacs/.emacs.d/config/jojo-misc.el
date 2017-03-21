;;;; -*- lexical-binding: t; -*-
(require 'jojo-funcs)

;; Save on focus lost.
(use-package focus-autosave-mode
  :ensure t
  :defer 2
  :diminish focus-autosave-mode
  :config
  (focus-autosave-mode))

;; Clean up whitespace on focus lost.
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :defer 2
  :config
  (global-whitespace-cleanup-mode))

;; Syntax Checking
(unless (jojo/windows-p)
  (use-package flycheck
    ;; :load-path "~/.emacs.d/fork/flycheck/"
    :ensure t
    :diminish flycheck-mode
    :commands (flycheck-mode)
    :init
    (setq flycheck-idle-change-delay 2)
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (add-hook 'prog-mode-hook #'flycheck-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    :config
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (let ((bitmap 'my-flycheck-fringe-indicator))
      (flycheck-define-error-level 'error
        :severity 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
        :severity 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
        :severity 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-info)))
  (use-package flycheck-pos-tip
    :ensure t
    :after flycheck
    :config
    (flycheck-pos-tip-mode)))

;; Terminal
(use-package multi-term
  :ensure t
  :commands (multi-term multi-term-next multi-term-prev)
  :init
  (defhydra hydra-term (:color teal :hint nil)
    "
     Extra:

     Shell               Files                         Misc
----------------------------------------------------------------
 _c_: Open Shell         _f_: Find File                  _i_: IRC
 _n_: Next Shell         _o_: Find File Other            _m_: Mail
 _p_: Previous Shell     _e_: Finder
 _q_: Eshell             _t_: Terminal
"
    ("c" jojo/open-shell)
    ("q" eshell)
    ("n" multi-term-next)
    ("p" multi-term-prev)
    ("f" find-file)
    ("o" find-file-other-window)
    ("e" jojo/explorer-finder)
    ("t" jojo/open-terminal)
    ("i" jojo/erc)
    ("m" jojo/mu4e))
  :config
  (evil-define-key 'insert term-raw-map (kbd "TAB") 'term-send-raw) ;; rebinding
  (evil-define-key 'normal term-raw-map (kbd "p") 'term-paste)
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (add-to-list 'term-unbind-key-list "C-q") ; C-q binds to raw input by default
  (setq multi-term-program "/bin/zsh"))

;; Movement
(use-package ace-jump-mode
  :ensure t
  :commands
  (ace-jump-mode))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(use-package framemove
  :config
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(use-package shackle
  :ensure t
  :commands (shackle-mode)
  :init
  (add-hook 'after-init-hook #'shackle-mode)
  :config
  (setq shackle-rules
        '(("OmniSharp" :regexp t :align below :size 0.3 :select t)
          ("*Help*" :regexp t :align below :size 0.3 :select t)
          (alchemist-mix-mode :align below :size 0.3 :select nil)
          ("*alchemist help*" :regexp t :align below :size 0.2 :select t)
          (alchemist-iex-mode :align below :size 0.3 :select t)
          (alchemist-test-report-mode :align below :size 0.3 :select nil)
          ("*pdrun*" :regexp t :align below :size 0.3 :select nil)
          ("*Flycheck checkers*" :regexp t :align below :size 0.3 :select nil)
          ("*lua test results*" :regexp t :align below :size 0.3 :select nil)
          (anaconda-mode-view-mode :align below :size 0.3 :select t)
          ("*anaconda-mode*" :regexp t :align below :size 0.3 :select nil)
          (undo-tree-visualizer-mode :align below :size 0.4 :select t)
          (magit-process-mode :align below :size 0.35 :select nil)
          (geiser-doc-mode :align below :size 0.3 :select nil)
          ("*evil-registers*" :regexp t :align below :size 0.3 :select nil)
          ("*Dired log*" :regexp t :align below :size 0.3 :select nil)
          ("*make*" :regexp t :align below :size 0.3 :select nil)
          ("ag search" :regexp t :align below :size 0.3 :select t))))

;; Folding
(use-package fold-dwim-org
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'fold-dwim-org/minor-mode)
  (defun jojo/hs-toggle-node ()
    "Toggle fold with a heuristic to look for close beginning bracket."
    (interactive)
    (let ((current-line (s-trim (thing-at-point 'line t))))
      (cond
       ((not (member major-mode (jojo/bracket-languages)))
        (cond
         ((and (eq major-mode 'lua-mode)
               (string-match-p "function" current-line))
          (save-excursion
            (end-of-line)
            (call-interactively #'hs-toggle-hiding)))
         ((and (eq major-mode 'lua-mode)
               (s-starts-with? "end" current-line))
          (save-excursion
            (end-of-line)
            (search-backward "end")
            (backward-char 1)
            (call-interactively #'hs-toggle-hiding)))
         (t
          (call-interactively #'hs-toggle-hiding))))
       ((s-ends-with? "{" current-line)
        (save-excursion
          (end-of-line)
          (call-interactively #'hs-toggle-hiding)))
       ((save-excursion
          (forward-line 1)
          (when (s-starts-with? "{" (s-trim (thing-at-point 'line t)))
            (end-of-line)
            (call-interactively #'hs-toggle-hiding)
            t))
        t)
       ((and (eq major-mode 'csharp-mode)
             (s-starts-with? "#region" current-line))
        (save-excursion
          (end-of-line)
          (call-interactively #'hs-toggle-hiding)))
       ((and (eq major-mode 'csharp-mode)
             (s-starts-with? "#endregion" current-line))
        (save-excursion
          (end-of-line)
          (search-backward "#endregion")
          (backward-char 1)
          (call-interactively #'hs-toggle-hiding)))
       (t
        (call-interactively #'hs-toggle-hiding)))))

  (defun jojo/c-mode-hide-all ()
    "Toggle Fold All except top two level nodes."
    (interactive)
    (hs-hide-level 2)
    (forward-sexp 2))

  (dolist (mode (jojo/bracket-languages))
    (add-hook (jojo/mode-hook mode)
              (lambda ()
                (set (make-local-variable 'hs-hide-all-non-comment-function)
                     #'jojo/c-mode-hide-all))))

  (eval-after-load 'evil
    (lambda ()
      (define-key evil-normal-state-map (kbd "TAB") 'jojo/hs-toggle-node)
      (define-key evil-visual-state-map (kbd "TAB") 'jojo/hs-toggle-node)
      (define-key evil-motion-state-map (kbd "TAB") 'jojo/hs-toggle-node)
      ;; Set default Tab command.
      (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command)))
  (setq fold-dwim-org-strict nil))

(use-package speed-type
  :ensure t
  :commands (speed-type-region
             speed-type-buffer
             speed-type-text))

(use-package ix
  :ensure t
  :commands (ix ix-browse ix-delete)
  :config
  (setq ix-user "jojojames")
  (setq ix-token "jojojames"))

(use-package eshell
  :ensure nil
  :commands (eshell)
  :config
  ;; https://www.emacswiki.org/emacs/EshellAlias
  (defun eshell/emacs (file)
    "Open file in emacs."
    (find-file file))
  (defun eshell/e (file)
    "Open file in emacs."
    (eshell/emacs file)))

;; colors for various 'color codes' aka hex strings
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :init
  (add-hook 'css-mode-hook
            (lambda ()
              ;; This 2 spaces check could go in a css mode package.
              ;; Adding it here for now out of laziness.
              (when (jojo/use-2-spaces-p)
                (setq css-indent-offset 2))
              (rainbow-mode)))
  :diminish rainbow-mode
  :config
  (rainbow-mode 1))

(provide 'jojo-misc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-misc.el ends here
