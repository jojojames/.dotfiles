;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

(use-package smartparens
  ;; :load-path "~/.emacs.d/fork/smartparens/"
  :ensure t
  :diminish smartparens-mode
  :init
  (defun jojo/set-smartparens-settings ()
    "Setting smartparens settings."
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-autoskip-closing-pair 'always-end)
    (setq sp-autoskip-opening-pair t))
  (add-hook 'prog-mode-hook #'jojo/set-smartparens-settings)

  (defun jojo/evil-strict-compatible-p ()
    "Return whether or not evil-smartparens or evil-cleverparens
is enabled."
    (or (fboundp 'evil-smartparens-mode)
        (fboundp 'evil-cleverparens-mode)
        (fboundp 'lispyville-mode)))

  (dolist (hook (jojo/lisp-hooks))
    (add-hook hook (lambda ()
                     (when (jojo/evil-strict-compatible-p)
                       (smartparens-strict-mode)))))
  :config
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)
  (jojo/set-smartparens-settings)
  (sp-pair "(" ")" :wrap "M-(")
  (sp-pair "(" ")" :wrap "M-)")
  (sp-pair "[" "]" :wrap "M-[")
  (sp-pair "[" "]" :wrap "M-]")
  (sp-pair "{" "}" :wrap "M-{")
  (sp-pair "{" "}" :wrap "M-}")
  (sp-pair "\"" "\"" :wrap "M-\"")

  (sp-pair "(" ")" :wrap "s-(")
  (sp-pair "(" ")" :wrap "s-)")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "[" "]" :wrap "s-]")
  (sp-pair "{" "}" :wrap "s-{")
  (sp-pair "{" "}" :wrap "s-}")
  (sp-pair "\"" "\"" :wrap "s-\"")

  ;; Some custom keymappings
  (eval-after-load 'evil
    (lambda ()
      (evil-define-key 'insert smartparens-mode-map
        (kbd "C-.") 'sp-forward-slurp-sexp
        (kbd "C-,") 'sp-forward-barf-sexp
        (kbd "C->") 'sp-backward-barf-sexp
        (kbd "C-<") 'sp-backward-slurp-sexp)
      (evil-define-key 'normal smartparens-mode-map
        (kbd "M-d") 'sp-kill-sexp
        (kbd "s-d") 'sp-kill-sexp
        (kbd "M-y") 'sp-copy-sexp
        (kbd "s-y") 'sp-copy-sexp
        (kbd "M-<backspace>") 'sp-backward-kill-sexp
        (kbd "s-<backspace>") 'sp-backward-kill-sexp)))

  (defun jojo/toggle-smartparens-strict ()
    "Wrapper around toggling strict smartparens."
    (interactive)
    (call-interactively #'smartparens-strict-mode)
    (when (fboundp 'evil-smartparens-mode)
      (call-interactively #'evil-smartparens-mode))
    (when (fboundp 'evil-cleverparens-mode)
      (call-interactively #'evil-cleverparens-mode))
    (when (fboundp 'lispyville-mode)
      (call-interactively #'lispyville-mode)))

  ;; Disable highlights.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  (sp-local-pair (jojo/bracket-languages) "/*" "*/"
                 :when '(sp-point-in-empty-line-p))
  (sp-local-pair (jojo/bracket-languages) "(" nil
                 :unless '(sp-point-before-word-p))
  (sp-local-pair (jojo/bracket-languages) "[" nil
                 :unless '(sp-point-before-word-p))
  (sp-local-pair (jojo/bracket-languages) "{" "}"
                 :when '(("RET" "<evil-ret>"))
                 :post-handlers '(jojo/reindent-and-position-middle))

  (sp-with-modes '(objc-mode)
    (sp-local-pair "@interface" "@end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p)
                   :post-handlers '(add-pair-and-return)))

  (eval-after-load 'evil
    (lambda ()
      (defun jojo/sp-copy-sexp-to-0-register (&rest _args)
        "After `sp-copy-sexp', put copied text into 0 register like vim."
        (interactive)
        (evil-set-register ?0 (evil-get-register ?1))
        (when evil-this-register
          (evil-set-register evil-this-register (evil-get-register ?0))
          (setq evil-this-register nil)))
      (advice-add 'sp-copy-sexp :after #'jojo/sp-copy-sexp-to-0-register)
      (advice-add 'sp-backward-copy-sexp :after #'jojo/sp-copy-sexp-to-0-register)

      (defun jojo/sp-delete-use-register (&rest _args)
        "After some `smartparens' kill commands, add the register to evil."
        (interactive)
        (when evil-this-register
          (evil-set-register evil-this-register (evil-get-register ?1))
          (setq evil-this-register nil)))

      (advice-add 'sp-kill-sexp :after #'jojo/sp-delete-use-register)
      (advice-add 'sp-backward-kill-sexp :after #'jojo/sp-delete-use-register)))

  (defun add-pair-and-return (&rest _ignored)
    "Adds the pair and then return to position."
    (save-excursion
      (insert "x")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))

  (defun jojo/reindent-and-position-middle (_id action _context)
    "Reindents and positions cursor in the middle."
    (if (eq 'skip-closing-pair action)
        t
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode)))

  (defhydra hydra-smartparens (:color teal :columns 3)
    "Smartparens"
    ("k" sp-kill-sexp "Kill")
    ("K" sp-backward-kill-sexp "Backward Kill")
    ("-" sp-backward-kill-sexp "Backward Kill")
    ("<delete>" sp-unwrap-sexp "Unwrap")
    ("<backspace>" sp-backward-unwrap-sexp "Backward Unwrap")
    ("t" sp-tranpose-sexp "Transpose")
    ("s" sp-splice-sexp "Splice")
    ("w" sp-splice-sexp-killing-around "Splice Killing Around")
    ("a" sp-splice-sexp-killing-backward "Splice Killing Backward")
    ("f" sp-splice-sexp-killing-forward "Splice Killing Forward")
    ("y" sp-copy-sexp "Copy")
    ("Y" sp-backward-copy-sexp "Copy Backwards")
    ("c" sp-convolute-sexp "Convolute")
    ("A" sp-absorb-sexp "Absorb")
    ("e" sp-emit-sexp "Emit")
    ("z" sp-extract-before-sexp "Extract Before")
    ("x" sp-extract-after-sexp "Extract After")
    ("S" sp-split-sexp "Split")
    ("j" sp-join-sexp "Join")
    ("r" sp-rewrap-sexp "Rewrap")
    ("T" sp-swap-enclosing-sexp "Swap Enclosing")
    ("n" sp-add-to-next-sexp "Add to Next")
    ("p" sp-add-to-previous-sexp "Add to Previous")))

(use-package lispyville
  :ensure t
  :commands
  (lispyville-mode)
  :init
  (dolist (hook (jojo/lisp-hooks))
    (add-hook hook (lambda ()
                     (lispyville-mode))))
  :config
  (lispyville-set-key-theme
   '(operators
     s-operators
     additional-movement
     slurp/barf-cp
     additional
     escape)))

(provide 'jojo-pair-editing)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-pair-editing.el ends here
