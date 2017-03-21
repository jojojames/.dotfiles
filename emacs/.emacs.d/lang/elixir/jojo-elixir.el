;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

(use-package elixir-mode
  :ensure t
  :mode
  ("\\.elixir\\'" . elixir-mode)
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode)
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq tab-width 2)
              (eval-after-load 'evil
                (lambda ()
                  (setq evil-shift-width 2)))))
  :config
  (defun sp-elixir-fn-post-handler (id action context)
    "Handler for elixir fn inserts."
    (if (not (equal action 'insert))
        (sp-ruby-def-post-handler id action context)
      (let ((line (thing-at-point 'line t)))
        (cond
         ;; SPC inside () -> (fn| end)
         ((string-match "^.*fn end.*).*$" line)
          (insert " ")
          (backward-char 1))
         ;; SPC -> fn|
         ;;        end
         ((string-match "^.*fn end.*$" line)
          (sp-ruby-def-post-handler id action context))
         ;; RET -> fn
         ;;        |
         ;;        end
         ((string-match "^\s+end.*$" line)
          (sp-ruby-def-post-handler id action context))
         ;; RET inside () -> (fn
         ;;                  |
         ;;                  end)
         ((and (string-match "^\s*end.*).*$" line)
               ;; Not on the same line as another end.
               (not (string-match "^.*end.*end.*$" line)))
          (sp-ruby-def-post-handler id action context))
         ;; Open Paren inside () -> (fn(|) end)
         ((string-match "^.*(fn(.*).*$" line)
          (save-excursion
            (kill-word 1)
            (sp-up-sexp)
            (insert " ")
            (yank)
            (indent-according-to-mode)))
         ;; Open Paren -> fn(|)
         ;;               end
         (t
          (save-excursion
            (kill-word 1)
            (end-of-line)
            (newline)
            (yank)
            (indent-according-to-mode)))))))

  (eval-after-load 'smartparens
    (lambda ()
      (require 'smartparens-ruby)
      (sp-with-modes '(elixir-mode)
        (sp-local-pair "do" "end"
                       :when '(("SPC" "RET"))
                       :post-handlers '(sp-ruby-def-post-handler)
                       :actions '(insert navigate))
        (sp-local-pair "fn" "end"
                       :when '(("(" "SPC" "RET"))
                       :post-handlers '(sp-elixir-fn-post-handler)
                       :actions '(insert navigate))))))

(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  (add-hook 'alchemist-mode-hook
            (lambda ()
              (jojo/company-merge-backends)) t)
  :config
  (setq alchemist-test-ask-about-save nil)
  (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.3.4")
  ;; (setq alchemist-goto-elixir-source-dir "~/.source/elixir/elixir-1.0.0")
  (setq alchemist-goto-erlang-source-dir "~/.source/erlang/otp_src_19.2")
  (jojo/evil-default-state
   'motion '(alchemist-compile-mode
             alchemist-eval-mode
             alchemist-execute-mode
             alchemist-message-mode
             alchemist-help-minor-mode
             alchemist-macroexpand-mode
             alchemist-refcard-mode))

  (jojo/evil-add-default-state 'motion '(alchemist-mix-mode alchemist-test-report-mode))

  ;; Erlang synergy
  (defun jojo/elixir-erlang-pop-back ()
    "Pop back definition function for Erlang mode."
    (interactive)
    (if (ring-empty-p erl-find-history-ring)
        (alchemist-goto-jump-back)
      (erl-find-source-unwind)))

  (defun jojo/alchemist-erlang-mode-hook ()
    (define-key erlang-mode-map (kbd "M-,") 'jojo/elixir-erlang-pop-back)
    (evil-define-key 'normal erlang-mode-map
      (kbd "g/") 'jojo/elixir-erlang-pop-back))

  (add-hook 'erlang-mode-hook 'jojo/alchemist-erlang-mode-hook)

  (defun jojo/alchemist-mix-phoenix-server ()
    "Run mix phoenix.server."
    (interactive)
    (when-let ((process (get-buffer-process "*alchemist mix*")))
      (set-process-query-on-exit-flag process nil))
    (alchemist-mix-execute '("phoenix.server")))

  (evil-define-key 'normal alchemist-mode-map
    (kbd "K") 'alchemist-help-search-at-point
    (kbd "gz") 'alchemist-iex-run
    (kbd "gd") 'alchemist-goto-list-symbol-definitions
    (kbd "g.") 'alchemist-goto-definition-at-point
    (kbd "g/") 'alchemist-goto-jump-back
    (kbd "gt") 'alchemist-project-find-test
    (kbd "go") 'alchemist-project-toggle-file-and-tests
    (kbd "gO") 'alchemist-project-toggle-file-and-tests-other-window)

  (defhydra hydra-alchemist-iex (:color blue :columns 3)
    "IEX"
    ("c" alchemist-iex-compile-this-buffer "Compile Buffer")
    ("b" alchemist-iex-compile-this-buffer "Compile Buffer")
    ("I" alchemist-iex-run "Run")
    ("i" alchemist-iex-project-run "Project Run")
    ("l" alchemist-iex-send-current-line "Send Current Line")
    ("L" alchemist-iex-send-current-line-and-go "Send Current Line and Go")
    ("m" alchemist-iex-reload-module "Reload Module")
    ("r" alchemist-iex-send-region "Send Region")
    ("R" alchemist-iex-send-region-and-go "Send Region and Go"))

  (defhydra hydra-alchemist-eval (:color blue :columns 3)
    "Eval"
    ("i" hydra-alchemist-iex/body "IEX")
    ("b" alchemist-eval-buffer "Buffer")
    ("B" alchemist-eval-print-buffer "Print Buffer")
    ("l" alchemist-eval-current-line "Current Line")
    ("L" alchemist-eval-print-current-line "Print Current Line")
    ("r" alchemist-eval-region "Region")
    ("R" alchemist-eval-print-region "Print Region")
    ("j" alchemist-eval-quoted-current-line "Quoted Current Line")
    ("J" alchemist-eval-print-quoted-current-line "Print Quoted Current Line")
    ("u" alchemist-eval-quoted-region "Quoted Region")
    ("U" alchemist-eval-print-quoted-region "Print Quoted Region")
    ("v" alchemist-eval-quoted-buffer "Quoted Buffer")
    ("V" alchemist-eval-print-quoted-buffer "Print Quoted Buffer"))

  (defhydra hydra-alchemist-test (:color blue :columns 3)
    "Test"
    ("t" alchemist-mix-test "Mix Test")
    ("a" alchemist-mix-test "Mix Test")
    ("S" alchemist-mix-test-stale "Mix Test Stale")
    ("b" alchemist-mix-test-this-buffer "Mix Test Buffer")
    ("p" alchemist-mix-test-at-point "Mix Test at Point")
    ("f" alchemist-mix-test-file "Mix Test File")
    ("." alchemist-test-jump-to-next-test "Next Test")
    ("/" alchemist-test-jump-to-previous-test "Prev Test")
    ("N" alchemist-test-jump-to-next-test "Next Test")
    ("P" alchemist-test-jump-to-previous-test "Prev Test")
    ("r" alchemist-mix-rerun-last-test "Rerun Test"))

  (defhydra hydra-alchemist-mix (:color blue :columns 3)
    "Mix"
    ("m" alchemist-mix "Mix")
    (";" alchemist-mix "Mix")
    (":" alchemist-mix "Mix")
    ("c" alchemist-mix-compile "Compile")
    ("x" alchemist-mix-run "Run")
    ("h" alchemist-mix-help "Help"))

  (defhydra hydra-alchemist-help (:color blue)
    "Help"
    ("m" alchemist-mix-help "Mix")
    (":" alchemist-help "Help")
    ("H" alchemist-help-history "Help History")
    ("h" alchemist-help-search-at-point "Help at Point")
    ("r" alchemist-help-search-marked-region "Help Marked Region"))

  (defhydra hydra-alchemist-phoenix (:color blue :columns 3)
    "Phoenix"
    ("R" alchemist-phoenix-routes "Routes")
    ("c" alchemist-phoenix-find-controllers "Controllers")
    ("l" alchemist-phoenix-find-channels "Channels")
    ("m" alchemist-phoenix-find-models "Models")
    ("r" alchemist-phoenix-router "Router")
    ("s" alchemist-phoenix-find-static "Static")
    ("t" alchemist-phoenix-find-templates "Template")
    ("v" alchemist-phoenix-find-views "View")
    ("w" alchemist-phoenix-find-web "Web"))

  (defhydra hydra-alchemist-mode (:color blue :columns 3)
    "Alchemist"
    ("u" jojo/alchemist-mix-phoenix-server "Run Phoenix Server")
    ("r" jojo/alchemist-mix-phoenix-server "Run Phoenix Server")
    ("p" hydra-alchemist-phoenix/body "Phoenix")
    ("e" hydra-alchemist-eval/body "Eval")
    ("t" hydra-alchemist-test/body "Test")
    ("z" alchemist-iex-run "Run IEX")
    ("i" hydra-alchemist-iex/body "IEX")
    ("m" hydra-alchemist-mix/body "Mix")
    ("h" hydra-alchemist-help/body "Help")
    ("l" alchemist-goto-list-symbol-definitions "Goto Definitions")
    ("xb" alchemist-execute-this-buffer "Execute Buffer")
    ("xf" alchemist-execute-file "Execute File")
    ("xx" alchemist-execute "Execute")
    ("cb" alchemist-compile-this-buffer "Compile Buffer")
    ("cf" alchemist-compile-file "Compile File")
    ("c:" alchemist-compile "Compile"))

  (jojo/add-hydra '(eval test mode) 'alchemist-mode))

(use-package flycheck-mix
  :ensure t
  :commands flycheck-mix-setup
  :init
  (defun jojo/elixir-flycheck-initialize ()
    "Initialize flycheck mix."
    (eval-after-load 'flycheck
      (lambda ()
        (flycheck-mix-setup)))
    (remove-hook 'elixir-mode-hook #'jojo/elixir-flycheck-initialize))
  (add-hook 'elixir-mode-hook #'jojo/elixir-flycheck-initialize))

(use-package elixir-yasnippets
  :ensure t
  :config
  (elixir-snippets-initialize))

;;;###autoload
(defun jojo/elixir-bootstrap ()
  "Bootstrap `jojo-elixir'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/elixir-bootstrap auto-mode-alist))
  (elixir-mode))

(provide 'jojo-elixir)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-elixir.el ends here
