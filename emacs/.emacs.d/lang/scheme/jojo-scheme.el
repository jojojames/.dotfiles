;;;; -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :commands (geiser-mode)
  :init
  (add-hook 'scheme-mode-hook #'geiser-mode)
  :config
  (setq geiser-mode-start-repl-p t)
  (setq geiser-repl-query-on-kill-p nil)
  (eval-after-load 'evil
    (lambda ()
      (add-hook 'geiser-doc-mode-hook
                (lambda ()
                  (evil-define-key 'normal geiser-doc-mode-map (kbd "q")
                    #'kill-current-buffer-and-its-windows)))
      (evil-define-key 'insert geiser-repl-mode-map (kbd "<S-return>") #'geiser-repl--newline-and-indent)
      (evil-define-key 'normal geiser-mode-map (kbd "gZ") #'geiser-mode-switch-to-repl-and-enter)
      (evil-define-key 'normal geiser-mode-map (kbd "gz") #'geiser-mode-switch-to-repl)
      (evil-define-key 'normal geiser-mode-map (kbd "g.") #'geiser-edit-symbol-at-point)
      (evil-define-key 'normal geiser-mode-map (kbd "g/") #'geiser-pop-symbol-stack)
      (evil-define-key 'normal geiser-mode-map (kbd "K") #'geiser-doc-symbol-at-point)))

  ;; TODO geiser-debug
  ;; TODO geiser-company
  ;; TODO geiser-repl binds

  (defhydra hydra-geiser-insert (:color blue :columns 3)
    "Insert"
    ("." geiser-completion--complete-module "Complete Module")
    ("i" completion-at-point "Completion at Point")
    ("l" geiser-insert-lambda "Insert Lambda"))

  (defhydra hydra-geiser-compile (:coloe blue :columns 3)
    "Compile"
    ("k" geiser-compile-current-buffer "Compile Buffer"))

  ;; TODO: Write a macro to generate the print versions of these evals by prepending C-u.
  (defhydra hydra-geiser-eval (:color blue :columns 3)
    "Eval"
    ("e" geiser-eval-last-sexp "S-exp")
    ("b" geiser-eval-buffer "Buffer")
    ("d" geiser-eval-definition "Definition")
    ("r" geiser-eval-region "Region")
    ("k" geiser-compile-current-buffer "Compile Buffer")
    ("l" geiser-load-file "Load File")
    ("B" geiser-eval-buffer-and-go "Buffer and Go")
    ("D" geiser-eval-definition-and-go "Definition and Go")
    ("R" geiser-eval-region-and-go "Region and Go")
    ("xe" geiser-expand-last-sexp "Expand S-exp")
    ("xr" geiser-expand-region "Expand Region")
    ("xd" geiser-expand-definition "Expand Definition"))

  (defhydra hydra-geiser-navigate (:color blue :columns 3)
    "Navigate"
    ("g" geiser-edit-symbol-at-point "Find Definition")
    ("b" geiser-pop-symbol-stack "Find Definition Pop")
    ("r" geiser-xref-callers "Callers")
    ("R" geiser-xref-callees "Callees"))

  ;; TODO: Macroexpand bindings.
  (defhydra hydra-geiser-mode (:color blue :columns 3)
    "Geiser"
    ("c" hydra-geiser-compile/body "Compile")
    ("g" hydra-geiser-navigate/body "Navigate")
    ("h" hydra-geiser-help/body "Help")
    ("e" hydra-geiser-eval/body "Eval")
    ("i" hydra-geiser-insert/body "Insert")
    ("m" hydra-geiser-misc/body "Misc")
    ("s" hydra-geiser-scheme/body "Scheme")
    ("k" geiser-restart-repl "Restart Repl")
    ("j" run-geiser "Run Geiser"))

  (defhydra hydra-geiser-help (:color blue :columns 3)
    "Help"
    ("r" geiser-xref-callers "Callers")
    ("R" geiser-xref-callees "Callees")
    ("k" geiser-doc-symbol-at-point "Doc at Point")
    ("l" geiser-doc-look-up-manual "Look up Manual")
    ("m" geiser-doc-module "Doc Module")
    ("s" geiser-autodoc-show "Autodoc Show"))

  (defhydra hydra-geiser-misc (:color blue :columns 3)
    "Misc"
    ("s" geiser-set-scheme "Set Scheme")
    ("l" geiser-add-to-load "Add to Load Path")
    ("e" geiser-edit-module "Edit Module")
    ("q" geiser-squarify "Squarify"))

  (defhydra hydra-geiser-scheme (:color blue :columns 3)
    "Scheme"
    ("d" scheme-send-definition "Send Definition")
    ("c" scheme-compile-definition-and-go "Compile Definition and Go")
    ("k" scheme-compile-file "Compile File")
    ("l" scheme-load-file "Load File")
    ("r" scheme-send-region "Send Region")
    ("t" scheme-trace-procedure "Trace Procedure")
    ("x" scheme-expand-current-form "Expand Current Form")
    ("z" switch-to-scheme "Switch to Scheme")
    ("e" scheme-send-last-sexp "Send Last S-exp")
    ("c" scheme-compile-definition "Compile Definition")
    ("r" scheme-send-region-and-go "Send Region and Go"))

  ;; This is a hack, `geiser-mode' isn't registered
  ;; at the time `jojo/add-hydra' is ran.
  (add-hook 'geiser-mode-hook
            (defun jojo/geiser-setup-hydra ()
              "Set up geiser hydra once."
              (jojo/add-hydra '(mode eval compile help) 'geiser-mode)
              (remove-hook 'geiser-mode-hook #'jojo/geiser-setup-hydra))))

;;;###autoload
(defun jojo/scheme-bootstrap ()
  "Bootstrap `jojo-commonlisp'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/scheme-bootstrap auto-mode-alist))
  (scheme-mode))

(provide 'jojo-scheme)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-scheme.el ends here
