;;;; -*- lexical-binding: t; -*-

(use-package erlang
  :ensure t
  :mode
  ("\\.erl\\'" . erlang-mode)
  ("\\.hrl\\'" . erlang-mode)
  ("\\.xrl\\'" . erlang-mode)
  :init
  (defun jojo/setup-inferior-erlang ()
    "Launch erlang shell in background"
    (when (null (inferior-erlang-running-p))
      (save-selected-window
        (inferior-erlang)
        (quit-window))))
  (add-hook 'erlang-mode-hook #'jojo/setup-inferior-erlang)
  :config
  ;; http://erlang.org/pipermail/erlang-questions/2003-June/009103.html
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist))
  (setq tab-width 4)
  (setq erlang-indent-level 4)
  (erlang-font-lock-level-4))

(use-package distel
  :ensure nil
  :commands (erlang-extended-mode)
  :load-path "~/.emacs.d/fork/distel/elisp"
  :init
  (add-hook 'erlang-mode-hook #'erlang-extended-mode)
  :config
  ;; http://bob.ippoli.to/archives/2007/03/16/distel-and-erlang-mode-for-emacs-on-mac-os-x/
  ;; prevent annoying hang-on-compile
  (defvar inferior-erlang-prompt-timeout t)
  ;; default node name to emacs@localhost
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; tell distel to default to that node
  (setq erl-nodename-cache
        (make-symbol
         (concat
          "emacs@"
          ;; Mac OS X uses "name.local" instead of "name", this should work
          ;; pretty much anywhere without having to muck with NetInfo
          ;; ... but I only tested it on Mac OS X.
          (car (split-string (shell-command-to-string "hostname"))))))

  (evil-define-key 'normal erlang-mode-map
    (kbd "K") 'erl-find-doc-under-point
    (kbd "gz") 'erl-ie-show-session
    (kbd "g.") 'erl-find-source-under-point
    (kbd "g/") 'erl-find-source-unwind
    (kbd "gr") 'erl-who-calls)

  (defhydra hydra-erlang-debug (:color blue)
    "Debug"
    ("i" edb-toggle-interpret "Toggle Interpret")
    ("p" edb-toggle-breakpoint "Toggle Breakpoint")
    ("b" edb-toggle-breakpoint "Toggle Breakpoint")
    ("d" edb-toggle-breakpoint "Toggle Breakpoint")
    ("s" edb-synch-breakpoints "Sync Breakpoints")
    ("S" edb-save-dbg-state" Save Debug State")
    ("R" edb-restore-dbg-state "Restore Debug State")
    ("m" edb-monitor "Monitor"))

  (defhydra hydra-erlang-node (:color blue)
    "Node Communication"
    ("n" erl-choose-nodename "Choose Nodename")
    ("p" erl-ping "Check Connection")
    ("P" erl-process-list "List Processes")
    ("r" erl-reload-module "Reload an Erlang Module")
    ("R" erl-reload-modules "Reload all Erlang Modules")
    ("w" erl-who-calls "Who Calls Function?")
    ("W" erl-rebuild-callgraph "Rebuild Who Calls Graph"))

  (defhydra hydra-erlang-eval (:color blue)
    "Eval"
    ("e" erl-eval-expression "Eval Expression")
    ("z" erl-eval-expression "Create Interactive Session"))

  (defhydra hydra-erlang-help (:color blue)
    "Help"
    ("s" erl-find-sig-under-point "Find Sig Under Point")
    ("S" erl-find-sig "Find Sig")
    ("k" erl-find-doc-under-point "Find Doc Under Point")
    ("K" erl-find-doc "Find Doc")
    ("d" erl-fdoc-describe "Describe")
    ("a" erl-fdoc-apropos "Apropos")
    ("f" fprof "Fprof")
    ("F" fprof-analyse "Fprof Analyze"))

  (defhydra hydra-erlang-mode (:color blue)
    "Erlang"
    ("e" hydra-erlang-eval/body "Eval")
    ("d" hydra-erlang-debug/body "Debug")
    ("h" hydra-erlang-help/body "Help")
    ("c" hydra-erlang-node/body "Node Communication")
    ("r" erl-refactor-subfunction "Refactor Subfunction")
    ("l" erlang-compile-display "Compile Display")
    ("u" erlang-compile "Compile")
    ("z" erlang-shell-display "Shell"))
  (jojo/add-hydra '(eval debug mode) 'erlang-mode))

(use-package company-distel
  :ensure nil
  :commands (company-distel)
  :load-path "~/.emacs.d/fork/company-distel/"
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (jojo/company-push-backend-local 'company-distel))))

;;;###autoload
(defun jojo/erlang-bootstrap ()
  "Bootstrap `jojo-erlang'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/erlang-bootstrap auto-mode-alist))
  (erlang-mode))

(provide 'jojo-erlang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-erlang.el ends here
