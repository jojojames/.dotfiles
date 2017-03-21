;;;; -*- lexical-binding: t; -*-

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :config
  (setq csharp-want-imenu t)
  ;; Folding support for `hs-minor-mode'.
  (defun csharp-hs-forward-sexp (&optional arg)
    (message "csharp-hs-forward-sexp, (arg %d) (point %d)…"
             (if (numberp arg) arg -1)
             (point))
    (let ((nestlevel 0)
          (done nil))
      (if (and arg (< arg 0))
          (message "negative arg (%d) is not supported…" arg)
        ;; else, we have a positive argument, hence move forward.
        ;; simple case is just move forward one brace
        (if (looking-at "{")
            (forward-sexp arg)
          ;; The more complex case is dealing with a "region/endregion" block.
          ;; We have to deal with nested regions!
          (and
           (while (not done)
             (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
                                (point-max) 'move)
             (cond
              ((eobp)) ;; Do nothing if at end of buffer.
              ((and
                (match-beginning 1)
                ;; If the match is longer than 6 chars, we know it is "endregion".
                (if (> (- (match-end 1) (match-beginning 1)) 6)
                    (setq nestlevel (1- nestlevel))
                  (setq nestlevel (1+ nestlevel))))))
             (setq done (not (and (> nestlevel 0) (not (eobp)))))) ; while
           (if (= nest 0)
               (goto-char (match-end 2))))))))

  (unless (assoc 'csharp-mode hs-special-modes-alist)
    (push '(csharp-mode
            ;; regexp for start block
            "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"
            ;; regexp for end block
            "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}"
            ;; regexp for comment start
            "/[*/]"
            ;; hs-forward-sexp-func
            csharp-hs-forward-sexp
            ;; c-like adjust (1 char))
            hs-c-like-adjust-block-beginning)
          hs-special-modes-alist)))

(use-package omnisharp
  :ensure t
  :commands (omnisharp-mode)
  :init
  (defun jojo/company-omnisharp-setup ()
    "Sets up c# completion with company."
    (jojo/company-push-backend 'company-omnisharp))
  (defun jojo/omnisharp-setup ()
    "Bootstrap omnisharp."
    (setq omnisharp-debug t)
    ;; Run $ xbuild in omnisharp-server first.
    (setq omnisharp-server-executable-path
          "~/.emacs.d/fork/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
    (omnisharp-mode))
  (defun jojo/csharp-mode-hook ()
    "csharp mode hook"
    (jojo/omnisharp-setup)
    (jojo/company-omnisharp-setup))
  (add-hook 'csharp-mode-hook #'jojo/csharp-mode-hook)
  :config
  (evil-define-key 'normal csharp-mode-map
    (kbd "gr") 'omnisharp-find-usages
    (kbd "g.") 'omnisharp-go-to-definition
    (kbd "g/") 'pop-tag-mark
    (kbd "K")  'omnisharp-current-type-information)

  ;; Hangs Emacs currently for some types.
  ;; Revisit at some point.
  (setq omnisharp-eldoc-support nil)

  (defhydra hydra-csharp-project (:color blue)
    "Project"
    ("a" omnisharp-add-to-solution-current-file "Add To Solution")
    ("A" omnisharp-add-to-solution-dired-selected-files "Add to Solution Dired")
    ("r" omnisharp-remove-from-project-current-file "Remove From Solution")
    ("R" omnisharp-remove-from-project-dired-selected-files "Remove From Solution Dired")
    ("l" omnisharp-add-reference "Add Reference"))

  (defhydra hydra-csharp-refactor (:color blue)
    "Refactor"
    ("m" omnisharp-rename "Rename")
    ("i" omnisharp-rename-interactively "Rename Interactively")
    ("r" omnisharp-run-code-action-refactoring "Code Action Refactoring"))

  (defhydra hydra-csharp-navigation (:color blue :columns 3)
    "Navigation"
    ("g" omnisharp-go-to-definition "Go To Definition")
    ("G" omnisharp-go-to-definition-other-window "Go To Definition Other")
    ("u" omnisharp-find-usages "Find Usages")
    ("s" omnisharp-helm-find-symbols "Helm Find Symbols")
    ("i" omnisharp-find-implementations "Find Implementations")
    ("r" omnisharp-navigate-to-region "Region")
    ("m" omnisharp-navigate-to-solution-member "Solution Member")
    ("M" omnisharp-navigate-to-solution-member-other-window "Solution Member Other Window")
    ("f" omnisharp-navigate-to-solution-file "Solution File")
    ("F" omnisharp-navigate-to-solution-file-then-file-member "Solution Then File Member")
    ("c" omnisharp-navigate-to-current-file-member "Current File Member"))

  (defhydra hydra-csharp-test (:color blue)
    "Test"
    ("a" omnisharp-unit-test-all "All")
    ("b" omnisharp-unit-test-fixture "Fixture")
    ("t" omnisharp-unit-test-single "Single"))

  (defhydra hydra-csharp-help (:color blue)
    "Help"
    ("t" omnisharp-current-type-information "Current Type Information")
    ("T" omnisharp-current-type-information-to-kill-ring "Current Type To Kill Ring")
    ("s" omnisharp-start-omnisharp-server "Start Server")
    ("S" omnisharp-stop-server "Stop Server")
    ("r" omnisharp-reload-solution "Reload Solution"))

  (defhydra hydra-csharp-mode (:color blue :columns 3)
    "CSharp"
    ("m" omnisharp-build-in-emacs "Build")
    ("p" hydra-csharp-project/body "Manage")
    ("r" hydra-csharp-refactor/body "Refactor")
    ("g" hydra-csharp-navigation/body "Navigate")
    ("t" hydra-csharp-test/body "Test")
    ("h" hydra-csharp-help/body "Help")
    ("u" omnisharp-auto-complete-overrides "Autocomplete Overrides")
    ("i" omnisharp-fix-usings "Fix Usings")
    ("=" omnisharp-code-format "Code Format"))

  (defun jojo/csharp-indent-dwim ()
    "If c style is custom, indent according to that style."
    (interactive)
    (if (string-match-p c-indentation-style "csharp")
        (call-interactively #'indent-region-or-buffer)
      (call-interactively #'omnisharp-code-format)))

  (jojo/add-hydra 'indent 'csharp-mode #'jojo/csharp-indent-dwim)
  (jojo/add-hydra '(test mode) 'csharp-mode))

;;;###autoload
(defun jojo/csharp-bootstrap ()
  "Bootstrap `jojo-csharp'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/csharp-bootstrap auto-mode-alist))
  (csharp-mode))

(provide 'jojo-csharp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-csharp.el ends here
