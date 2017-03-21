;;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (setq lua-indent-level 2)
  (eval-after-load 'evil
    (lambda ()
      (jojo/set-evil-shift-width 2)
      (evil-define-key 'normal lua-mode-map
        (kbd "gr") 'jojo/find-references
        (kbd "g.") 'dumb-jump-go
        (kbd "g/") 'dumb-jump-back
        (kbd "K") 'lua-search-documentation)))

  (defhydra hydra-lua-test (:color blue :columns 3)
    "Lua Test"
    ("p" jojo/lua-run-test-at-point "Test at Point")
    ("f" jojo/lua-run-test-file "Test File")
    ("t" jojo/lua-run-test-suite "Test Suite"))

  (defhydra hydra-lua-eval (:color blue :columns 3)
    "Lua Eval"
    ("b" lua-send-buffer "Send Buffer")
    ("l" lua-send-current-line "Send Current Line")
    ("e" lua-send-defun "Send Defun")
    ("r" lua-send-region "Send Region")
    ("s" lua-show-process-buffer "Show Process Buffer")
    ("h" lua-hide-process-buffer "Hide Process Buffer")
    ("z" lua-start-process "Start Repl")
    ("x" lua-kill-process "Kill Repl")
    ("R" lua-restart-with-whole-file "Restart Repl with File"))

  (defhydra hydra-lua-mode (:color blue :columns 3)
    "Lua"
    ("l" hydra-love-mode/body "Love")
    ("e" hydra-lua-eval/body "Eval")
    ("t" hydra-lua-test/body "Test")
    ("z" lua-show-process-buffer "Show Repl")
    ("h" lua-search-documentation "Search Documentation")
    ("r" pd/love-run "Make")
    ("u" pd/love-run "Make"))

  (defun pd/love-run ()
    "Run pdrun script in root of project."
    (interactive)
    (let ((default-directory
            (locate-dominating-file default-directory "makefile"))
          (run-command "make"))
      (compilation-start run-command 'compilation-mode
                         (lambda (_mode-name)
                           "*pdrun make*")
                         t)))

  (defun jojo/lua-run-test-suite ()
    "Run test_suite.lua."
    (interactive)
    (let ((default-directory (locate-dominating-file
                              (file-name-directory buffer-file-name)
                              "main.lua")))
      (compilation-start
       (format "lua tests/test_suite.lua -v")
       'compilation-mode
       (lambda (_mode-name)
         "*lua test results*")
       t)))

  (defun jojo/lua-run-test-file ()
    "Run test file using buffer as file."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((default-directory (locate-dominating-file
                                  (file-name-directory buffer-file-name)
                                  "main.lua")))
          (compilation-start (format "lua %s -v" buffer-file)
                             'compilation-mode
                             (lambda (_mode-name)
                               "*lua test results*")
                             t))
      (message "`buffer-file-name' is nil.")))

  (defun jojo/lua-run-test-at-point ()
    "Run test at point."
    (interactive)
    (if-let ((buffer-file (buffer-file-name)))
        (let ((function-name
               (let ((current-line (thing-at-point 'line t)))
                 (progn
                   (unless (string-match-p "function" current-line)
                     (search-backward "function"))
                   (let ((new-current-line (s-trim (thing-at-point 'line t))))
                     (s-trim
                      (s-chop-suffix
                       "()"
                       (s-chop-prefix "function" new-current-line))))))))
          (if function-name
              (let ((default-directory (locate-dominating-file
                                        (file-name-directory buffer-file-name)
                                        "main.lua")))
                (compilation-start
                 (format "lua %s %s -v" buffer-file (s-replace ":" "." function-name))
                 'compilation-mode
                 (lambda (_mode-name)
                   "*lua test results*")
                 t))
            (message "Couldn't find `function-name'.")))
      (message "`buffer-file-name' is nil.")))

  (jojo/add-hydra '(mode eval test) 'lua-mode))

(use-package company-lua
  :ensure t
  :commands (company-lua)
  :init
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq company-lua-interpreter 'love)
              (jojo/company-push-backend-local 'company-lua))))

(use-package love-minor-mode
  :ensure t
  :commands (love/possibly-enable-mode)
  :init
  (defcustom love/documentation-url
    "https://love2d.org/wiki/"
    "URL pointing to the Love wiki."
    :type 'string
    :group 'lua)

  (defun love/search-documentation ()
    "Search Love documentation for the word at the point."
    (interactive)
    (let ((url (concat love/documentation-url (lua-funcname-at-point))))
      (funcall lua-documentation-function url)))

  (defhydra hydra-love-mode ()
    "Love"
    ("k" love/search-documentation "Search Documentation")
    ("p" love/create-project-configuration "Create Project")
    ("f" love/search-forums "Search Forums")
    ("h" love/browse-documentation "Browse Documentation"))
  (add-hook 'lua-mode-hook
            (lambda ()
              (love/possibly-enable-mode))))

;;;###autoload
(defun jojo/lua-bootstrap ()
  "Bootstrap `jojo-lua'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/lua-bootstrap auto-mode-alist))
  (lua-mode))

(provide 'jojo-lua)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-lua.el ends here
