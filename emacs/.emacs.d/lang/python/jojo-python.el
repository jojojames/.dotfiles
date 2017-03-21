;;;; -*- lexical-binding: t; -*-

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (eval-after-load 'python
    (lambda ()
      ;; https://github.com/jorgenschaefer/elpy/issues/887#issuecomment-281521965
      (defun python-shell-completion-native-try ()
        "Return non-nil if can trigger native completion."
        (let ((python-shell-completion-native-enable t)
              (python-shell-completion-native-output-timeout
               python-shell-completion-native-try-output-timeout))
          (python-shell-completion-native-get-completions
           (get-buffer-process (current-buffer))
           nil "_")))

      (defun jojo/setup-inferior-python ()
        "Launch python shell in background"
        (if-let ((python-shell-buffer (python-shell-get-buffer)))
            (save-selected-window
              (switch-to-buffer-other-window
               python-shell-buffer)
              (quit-window))
          (run-python (python-shell-calculate-command) nil nil)))

      (add-hook 'python-mode-hook #'jojo/setup-inferior-python)))
  :config
  (defhydra hydra-python-skelaton (:color blue :columns 3)
    "Skeleton"
    ("c" python-skeleton-class "Class")
    ("d" python-skeleton-def "Def")
    ("f" python-skeleton-for "For")
    ("i" python-skeleton-if "If")
    ("I" python-skeleton-import "Import")
    ("t" python-skeleton-try "Try")
    ("w" python-skeleton-while "While"))

  (defhydra hydra-python-eval (:color blue)
    "Python Eval"
    ("r" python-shell-send-region "Send Region")
    ("b" python-shell-send-buffer "Send Buffer")
    ("s" python-shell-send-string "Send String")
    ("l" python-shell-send-file "Send File")
    ("e" python-shell-send-defun "Send Function"))

  (defhydra hydra-python-mode (:color blue)
    "Python"
    ("e" hydra-python-eval/body "Eval")
    ("z" python-shell-switch-to-shell "Shell")
    ("u" run-python "Run")
    ("s" hydra-python-skelaton/body "Skeleton")
    ("p" pdb "Pdb"))

  (jojo/add-hydra '(mode eval) 'python-mode)

  (eval-after-load 'evil
    (lambda ()
      (jojo/set-evil-shift-width python-indent-offset)))

  ;; pdb setup
  (when (jojo/osx-p)
    (setq pdb-path '/usr/lib/python2.7/pdb.py)
    (setq gud-pdb-command-name (symbol-name pdb-path))

    (defadvice pdb (before gud-query-cmdline activate)
      "Provide a better default command line when called interactively."
      (interactive
       (list (gud-query-cmdline pdb-path
                                (file-name-nondirectory buffer-file-name))))))

  ;; https://github.com/emacsmirror/python-mode - see troubleshooting
  ;; https://bugs.launchpad.net/python-mode/+bug/963253
  ;; http://pswinkels.blogspot.com/2010/04/debugging-python-code-from-within-emacs.html
  (when (jojo/windows-p)
    (setq windows-python-pdb-path "c:/python27/python -i c:/python27/Lib/pdb.py")
    (setq pdb-path 'C:/Python27/Lib/pdb.py)
    (setq gud-pdb-command-name (symbol-name pdb-path))
    (setq gud-pdb-command-name windows-python-pdb-path)

    (defun jojo/set-pdb-command-path ()
      (setq gud-pdb-command-name
            (concat windows-python-pdb-path " " buffer-file-name)))

    ;; everytime we enter a new python buffer, set the command path to include the buffer filename
    (add-hook 'python-mode-hook 'jojo/set-pdb-command-path)))

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook #'python-mode-hook (lambda ()
                                 (anaconda-mode)
                                 (anaconda-eldoc-mode)))
  :config
  (eval-after-load 'evil (lambda ()
                           (evil-define-key 'normal python-mode-map
                             (kbd "g.") 'anaconda-mode-find-definitions
                             (kbd "gv") 'anaconda-mode-find-assignments
                             (kbd "gr") 'anaconda-mode-find-references
                             (kbd "gf") 'anaconda-mode-find-file
                             (kbd "g/") 'anaconda-mode-go-back
                             (kbd "K") 'anaconda-mode-show-doc))))

(use-package company-anaconda
  :ensure t
  :commands (company-anaconda)
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (jojo/company-push-backend-local
               '(company-anaconda :with company-capf)))))

;;;###autoload
(defun jojo/python-bootstrap ()
  "Bootstrap `jojo-python'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/python-bootstrap auto-mode-alist))
  (python-mode))

(provide 'jojo-python)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-python.el ends here
