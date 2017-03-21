;;;; -*- lexical-binding: t; -*-
(use-package lisp-mode
  :ensure nil
  :mode
  "\\.lisp\\'"
  "\\.stumpwmrc\\'"
  "\\.stumpish\\'"
  "\\.sbclrc\\'")

(use-package slime
  :ensure t
  :init
  (cond
   ((jojo/linux-p)
    (setq inferior-lisp-program "/usr/bin/sbcl"))
   (t
    (setq inferior-lisp-program "/usr/local/bin/sbcl")))
  :config
  (use-package slime-company
    :ensure t
    :commands (company-slime)
    :init
    (add-hook 'lisp-mode-hook
              (lambda ()
                (jojo/company-push-backend-local 'company-slime)))
    :config
    (setq slime-company-completion 'fuzzy))
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company))
  ;; Enable fuzzy matching in code buffer and SLIME REPL.
  (setq slime-complete-symbol*-fancy t)

  (slime-setup)

  (eval-after-load 'evil
    (lambda ()
      (evil-define-key 'normal slime-mode-map
        (kbd "g.") 'slime-edit-definition
        (kbd "g/") 'slime-pop-find-definition-stack
        (kbd "gr") 'slime-who-references
        (kbd "K")  'slime-describe-symbol)))

  ;; TODO: Add Debug Hydra
  (defhydra hydra-slime-compile (:color blue :columns 3)
    "Compile"
    ("c" slime-compile-file "Compile")
    ("C" slime-compile-and-load-file "Compile and Load")
    ("l" slime-load-file "Load File")
    ("f" slime-compile-defun "Compile Defun")
    ("r" slime-compile-region "Compile Region")
    ("n" slime-remove-notes "Remove Notes"))

  (defhydra hydra-slime-eval (:color blue :columns 3)
    "Eval"
    ("b" slime-eval-buffer "Buffer")
    ("f" slime-eval-defun "Defun")
    ("F" slime-undefine-function "Undefine Function")
    ("e" slime-eval-last-expression "Last Sexp")
    ("r" slime-eval-region "Region"))

  (defhydra hydra-slime-help (:color blue :columns 3)
    "Help"
    ("a" slime-apropos "Apropros")
    ("A" slime-apropos-all "Apropros All")
    ("d" slime-disassemble-symbol "Disassemble")
    ("h" slime-describe-symbol "Describe Symbol")
    ("H" slime-hyperspec-lookup "Hyperspec Lookup")
    ("p" slime-apropos-package "Apropos Package")
    ("t" slime-toggle-trace-fdefinition "Toggle Trace Fdefinition")
    ("T" slime-untrace-all "Untrace All")
    ("<" slime-who-calls "Who Calls")
    (">" slime-calls-who "Calls Who")
    ;; TODO: Add key bindings for who binds/sets globals?
    ("r" slime-who-references "Who References")
    ("m" slime-who-macroexpands "Who Macroexpands")
    ("s" slime-who-specializes "Who Specializes"))

  (defhydra hydra-slime-navigate (:color blue :columns 3)
    "Navigate"
    ("g" slime-edit-definition "Find Definition")
    ("b" slime-pop-find-definition-stack "Find Definition Pop")
    ("n" slime-next-note "Next Note")
    ("p" slime-previous-note "Previous Note")
    ("r" slime-who-references "Who References")
    ("m" slime-who-macroexpands "Who Macroexpands")
    ("s" slime-who-specializes "Who Specializes"))

  (defhydra hydra-slime-mode (:color blue :columns 3)
    "Slime"
    ("e" hydra-slime-eval/body "Eval")
    ("h" hydra-slime-help/body "Help")
    ("g" hydra-slime-navigate/body "Navigate")
    ("x" slime-scratch "Scratch")
    ("w" jojo/call-stump-hydra "Stumpwm")
    ("ma" slime-macroexpand-all "Macroexpand All")
    ("mo" slime-macroexpand-1 "Macroexpand One")
    ("se" slime-eval-last-expression-in-repl "Eval Last Expression in Repl")
    ("si" slime "Slime")
    ("sq" slime-quit-lisp "Quit Lisp")
    ("tf" slime-toggle-fancy-trace "Toggle Fancy Trace"))
  (jojo/add-hydra '(mode eval compile) 'slime-mode))

(use-package stumpwm-mode
  :ensure t
  :commands (stumpwm-mode)
  :init
  ;; We expect stumpwm to launch a swank server.
  (add-hook 'slime-mode-hook
            (lambda ()
              (when stumpwm-mode
                (slime-connect "127.0.0.1" slime-port))))
  (defun jojo/call-stump-hydra ()
    "Call stump hydra if `stumpwm-mode' is active."
    (interactive)
    (if stumpwm-mode
        (hydra-stumpwm/body)
      (message "`stumpwm-mode' is not active.")))

  ;; These can be used, but the regular `slime-mode' evals should be fine too.
  (defhydra hydra-stumpwm (:color blue)
    "Stumpwm"
    ("e" stumpwm-eval-last-sexp "Eval Last S-exp")
    ("r" stumpwm-eval-region "Eval Region")
    ("d" stumpwm-eval-defun "Eval Defun")
    ("f" stumpwm-eval-defun "Eval Defun"))

  (setq stumpwm-shell-program
        (expand-file-name (concat user-emacs-directory "lang/commonlisp/stumpish")))
  (add-hook 'lisp-mode-hook
            (lambda ()
              (when (or
                     (string-match-p "init.lisp" (buffer-name))
                     (string-match-p ".stumpwmrc" (buffer-name)))
                (stumpwm-mode)))))

;;;###autoload
(defun jojo/commonlisp-bootstrap ()
  "Bootstrap `jojo-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/commonlisp-bootstrap auto-mode-alist))
  (lisp-mode))

(provide 'jojo-commonlisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-commonlisp.el ends here
