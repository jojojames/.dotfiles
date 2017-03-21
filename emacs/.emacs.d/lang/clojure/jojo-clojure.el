;;;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :ensure t
  :mode
  ("\\.clj\\'" . clojure-mode)
  ("\\.edn\\'" . clojure-mode))

(use-package cider
  :ensure t
  :commands (cider cider-mode cider-jack-in)
  :diminish cider-auto-test-mode
  :init
  (setq nrepl-prompt-to-kill-server-buffer-on-quit nil)
  (defun jojo/cider-mode-hook ()
    (cider-mode)
    (clj-refactor-mode)
    (eldoc-mode)
    (jojo/company-merge-backends))
  (add-hook 'clojure-mode-hook #'jojo/cider-mode-hook)
  (add-hook 'cider-repl-mode-hook #'jojo/cider-mode-hook)
  :config
  (jojo/evil-default-state 'emacs '(cider--debug-mode))
  (jojo/evil-default-state 'motion '(cider-stacktrace-mode))

  ;; https://gist.github.com/ddellacosta/4967694b765b91e9d687
  (defun cider-jack-in-with-profile (profile)
    "Starts up a cider repl using jack-in with the specific lein profile
   selected."
    (interactive "sProfile: ")
    (let* ((profile-str profile)
           (profile-str (replace-regexp-in-string ":\\(.*\\)$" "\\1" profile-str))
           (lein-params (concat "with-profile +" profile-str " repl :headless")))
      (setq cider-lein-parameters lein-params)
      (cider-jack-in)))

  (defun cider-jack-in-with-profile-test ()
    "Wrapper over `cider-jack-in-with-profile' to run test environment."
    (interactive)
    (cider-jack-in-with-profile "test"))

  (defun cider-jack-in-dont-auto-run ()
    "Call cider-jack-in but don't call cider-run aftewards.
This method assumes we've done something like this below.
`(add-hook 'cider-connected-hook #'cider-run)'"
    (interactive)
    ;; Remove the hook before running `cider-jack-in'.
    (remove-hook 'cider-connected-hook #'cider-run)
    ;; Add it back after we're connected.
    (add-hook 'cider-connected-hook
              (lambda ()
                (add-hook 'cider-connected-hook #'cider-run)))
    (cider-jack-in))

  (defun cider-connect-dwim ()
    "Connect with cider-jack-in if not connected or restart if it is."
    (interactive)
    (if (cider-connected-p)
        (cider-restart)
      (cider-jack-in)))

  (use-package leiningen-wrapper
    :ensure nil
    :load-path "~/.emacs.d/me/leiningen-wrapper")

  ;; Call cider-run immediately after cider-jack-in connects.
  (add-hook 'cider-connected-hook #'cider-run)

  (evil-define-multiple
   (clojure-mode-map cider-mode-map cider-repl-mode-map)
   (normal visual)
   (kbd "gz") 'cider-switch-to-repl-buffer
   (kbd "g.") 'cider-find-dwim
   (kbd "g/") 'cider-pop-back
   (kbd "gf") 'cider-find-file
   (kbd "gr") 'jojo/find-references
   (kbd "K")  'cider-doc)

  (defhydra hydra-cider-lein (:color blue :columns 3)
    "Lein"
    ("lmm" lein-migratus-migrate "Migrate")
    ("lmr" lein-migratus-rollback "Rollback")
    ("lmd" lein-migratus-down "Down")
    ("lmu" lein-migratus-up "Up")
    ("lmR" lein-migratus-reset "Reset")
    ("lmc" lein-migratus-create "Create"))

  (jojo/defhydra
   (hydra-cider-eval hydra-cider-repl-eval) (:color blue :columns 3)
   "Eval"
   ("q" cider-eval-defun-at-point "Defun at Point")
   ("z" cider-switch-to-repl-buffer "Repl")
   ("r" cider-eval-region "Region")
   ("e" cider-eval-last-sexp "Sexp")
   ("x" cider-eval-last-sexp-and-replace "Sexp and Replace")
   ("w" cider-eval-last-sexp-to-repl "Sexp to Repl")
   ("i" cider-insert-last-sexp-in-repl "Insert Sexp in Repl")
   ("b" cider-eval-buffer "Buffer"))

  (jojo/defhydra
   (hydra-cider-test hydra-cider-repl-test) (:color blue :columns 3)
   "Test"
   ("r" cider-test-rerun-tests "Rerun")
   ("t" cider-test-run-test "Test")
   ("n" cider-test-run-ns-tests "NS Tests")
   ("l" cider-test-run-loaded-tests "Loaded Tests")
   ("p" cider-test-run-project-tests "Project Tests")
   ("b" cider-test-show-report "Show Report"))

  (jojo/defhydra
   (hydra-cider-debug hydra-cider-repl-eval)
   (:color blue)
   "Debug"
   ("f" cider-debug-defun-at-point "Defun at Point")
   ("d" cider-debug-defun-at-point "Defun at Point")
   ("L" cider-browse-instrumented-defs "Browse Instrumented Defs")
   ("l" cider-debug-toggle-locals "Toggle Locals"))

  (jojo/defhydra
   (hydra-cider-mode hydra-cider-repl-mode)
   (:color blue :columns 3)
   "Cider"
   ("l" hydra-cider-lein/body "Lein")
   ("e" hydra-cider-eval/body "Eval")
   ("t" hydra-cider-test/body "Test")
   ("d" hydra-cider-debug/body "Debug")
   ("c" cider-scratch "Scratch")
   ("jj" cider-jack-in "Jack In")
   ("jJ" cider-jack-in-dont-auto-run "Jack In (No Autorun)")
   ("jp" cider-jack-in-with-profile "Jack In With Profile")
   ("jt" cider-jack-in-with-profile-test "Jack In With Test Profile")
   ("LL" cider-load-buffer "Load Buffer")
   ("Lf" cider-load-file "Load File")
   ("Lp" cider-load-all-project-ns "Load Project Namespaces")
   ("R" cider-refresh "Refresh")
   ("u" cider-connect-dwim "Connect DWIM")
   ("r" cider-run "Run")
   ("q" cider-quit "Quit"))

  (jojo/add-hydra '(mode eval test debug)
                  '(cider-mode cider-repl-mode))

  (evil-define-key 'normal cider-test-report-mode-map (kbd "q") 'quit-window)

  ;; todo
  ;; https://github.com/clojure-emacs/cider/issues/784
  ;; if ns has the string 'test' in it, return the ns
  ;; otherwise do some magic here
  ;; probably need to account for luminus and how they set up the test namespaces
  ;; or maybe just chance luminus namespace
  (defun jojo/cider-determine-test-ns (ns)
    "Figuring out which namespace to test given the current namespace."
    (if (locate-dominating-file default-directory ".luminus")
        ns
      (cider-test-default-test-ns-fn ns)))
  (setq cider-test-infer-test-ns #'jojo/cider-determine-test-ns)

  (cider-auto-test-mode)
  (setq cider-test-show-report-on-success nil)
  ;; attempt to automatically look up symbol first
  (setq cider-prompt-for-symbol nil)
  ;; use shift return to get a new line in repl
  (define-key cider-repl-mode-map (kbd "C-j") nil)
  (define-key cider-repl-mode-map [(shift return)] 'cider-repl-newline-and-indent)

  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

;; (use-package flycheck-clojure
;;   :commands (flycheck-clojure-setup)
;;   :init
;;   (eval-after-load 'cider (lambda ()
;;                             (flycheck-clojure-setup))))

(use-package cider-eval-sexp-fu
  :ensure t
  :after eval-sexp-fu)

(use-package clj-refactor
  :ensure t
  :defer t
  :diminish clj-refactor-mode)

;;;###autoload
(defun jojo/clojure-bootstrap ()
  "Bootstrap `jojo-clojure'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/clojure-bootstrap auto-mode-alist))
  (clojure-mode))

(provide 'jojo-clojure)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-clojure.el ends here
