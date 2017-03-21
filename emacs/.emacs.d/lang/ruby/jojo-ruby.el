;;;; -*- lexical-binding: t; -*-

(use-package ruby-mode
  :ensure t
  :mode
  "\\.rb$\\'"
  "\\Rakefile$\\'"
  "\\.gemspec$\\'"
  "\\.ru$\\'"
  "\\Gemfile$\\'"
  "\\.rake$\\'"
  :interpreter "ruby"
  :init
  (defun jojo/check-if-cocoapods ()
    "Check if the ruby file is cocoapods.
   - Disable flycheck for CocoaPods"
    (when (string-match-p "^.*Podfile$" (buffer-file-name))
      (when (bound-and-true-p flycheck-mode)
        (flycheck-mode 0))))
  (add-hook 'ruby-mode-hook #'jojo/check-if-cocoapods)
  :config
  (jojo/set-evil-shift-width ruby-indent-level)
  (defhydra hydra-bundler-mode (:color blue :columns 3)
    "Bundler"
    ("o" bundle-open "Open")
    ("c" bundle-console "Console")
    ("C" bundle-check "Check")
    ("i" bundle-install "Install")
    ("u" bundle-update "Update")
    ("e" bundle-exec "Exec")
    ("O" bundle-outdated "Outdated")
    ("s" bundle-show "Show")
    ("v" bundle-version "Version")
    ("b" bundle-command "Command"))

  (defhydra hydra-ruby-mode (:color blue)
    "Ruby"
    ("B" hydra-bundler-mode/body "Bundler"))
  (jojo/add-hydra 'mode 'ruby-mode))

(use-package robe
  :ensure t
  :commands (robe-mode)
  :init
  (add-hook 'ruby-mode-hook #'jojo/robe-mode-hook)
  (defun jojo/robe-mode-hook ()
    "robe-mode hook."
    (if (derived-mode-p 'motion-mode)
        (robe-mode 0)
      (progn
        (robe-mode 1)
        (jojo/company-robe-setup))))
  (defun jojo/company-robe-setup ()
    "Sets up ruby completion with company."
    (jojo/company-push-backend-local 'company-robe))
  :config
  (evil-define-key 'normal robe-mode-map
    (kbd "g.") 'robe-jump
    (kbd "g/") 'pop-tag-mark
    (kbd "gf") 'robe-find-file
    (kbd "K") 'robe-doc)
  (defhydra hydra-robe-mode (:color blue)
    "Robe"
    ("K" robe-rails-refresh "Rails Refresh")
    ("B" hydra-bundler-mode/body "Bundler"))
  (jojo/add-hydra 'mode 'robe-mode))

(use-package projectile-rails
  :ensure t
  :commands (projectile-rails-on)
  :init
  (add-hook 'web-mode-hook #'jojo/init-projectile-rails)
  (add-hook 'ruby-mode-hook #'jojo/init-projectile-rails)
  (defun jojo/init-projectile-rails ()
    "Do some startup initialization."
    (projectile-rails-on)
    (when projectile-rails-mode
      ;; Handle Hydra in `hydra-rails-mode' instead.
      ;; There should be some handling here for the case where
      ;; we open a rails project and then open another robe project.
      (jojo/remove-hydra 'mode 'robe-mode)))
  :config
  (evil-define-key 'normal projectile-rails-mode-map
    (kbd "gf") 'projectile-rails-goto-file-at-point)
  (defhydra hydra-rails-mode (:color blue :columns 4)
    "Rails"
    ("a" projectile-rails-find-locale "Locale")
    ("A" projectile-rails-find-job "Job")
    ("B" hydra-robe-mode/body)
    ("c" projectile-rails-find-controller "Controller")
    ("C" projectile-rails-find-current-controller "Current Controller")
    ("d" projectile-rails-dbconsole "DB Console")
    ("D" projectile-rails-console "Console")
    ("e" projectile-rails-find-environment "Environment")
    ("E" projectile-rails-generate "Generate")
    ("f" projectile-rails-find-feature "Feature")
    ("F" projectile-rails-find-validator "Validator")
    ("gg" projectile-rails-goto-gemfile "Gemfile")
    ("gr" projectile-rails-goto-routes "Routes")
    ("gd" projectile-rails-goto-schema "Schema")
    ("gs" projectile-rails-goto-seeds "Seeds")
    ("gh" projectile-rails-goto-spec-helper "Spec Helper")
    ("h" projectile-rails-find-helper "Helper")
    ("H" projectile-rails-find-current-helper "Current Helper")
    ("i" projectile-rails-find-initializer "Initializer")
    ("j" projectile-rails-find-javascript "Javascript")
    ("J" projectile-rails-find-stylesheet "Stylesheet")
    ("K" robe-rails-refresh "Rails Refresh")
    ("l" projectile-rails-find-lib "Lib")
    ("L" projectile-rails-find-layout "Layout")
    ("m" projectile-rails-find-model "Model")
    ("M" projectile-rails-find-current-model "Current Model")
    ("n" projectile-rails-find-migration "Migration")
    ("N" projectile-rails-find-current-migration "Current Migration")
    ("o" projectile-rails-find-log "Log")
    ("p" projectile-rails-find-spec "Spec")
    ("P" projectile-rails-find-current-spec "Current Spec")
    ("r" projectile-rails-rake "Rake")
    ("R" projectile-rails-find-rake-task "Rake Task")
    ("t" projectile-rails-find-test "Test")
    ("T" projectile-rails-find-current-test "Current")
    ("u" projectile-rails-server "Server")
    ("v" projectile-rails-find-view "View")
    ("V" projectile-rails-find-current-view "Current View")
    ("x" projectile-rails-extract-region "Extract Region")
    ("y" projectile-rails-find-fixture "Fixture")
    ("Y" projectile-rails-find-current-fixture "Current Fixture")
    ("@" projectile-rails-find-mailer "Mailer")
    ("<RET>" projectile-rails-goto-file-at-point "File At Point"))
  (jojo/add-hydra 'mode 'projectile-rails-mode))

(use-package motion-mode
  :ensure t
  :commands (motion-recognize-project)
  :init
  (defun jojo/motion-mode-hook ()
    "Motion mode hook."
    (setq motion-flymake nil) ;; disable flymake
    (motion-recognize-project)
    (jojo/company-push-backend-local 'company-dict)
    (jojo/remove-hydra 'mode 'robe-mode)
    (setq flycheck-checker 'ruby-rubocop))
  (add-hook 'ruby-mode-hook #'jojo/motion-mode-hook)
  :config
  (eval-after-load 'flycheck (lambda ()
                               (flycheck-add-mode 'ruby-rubocop 'motion-mode)))
  (defun jojo/rake-to-device ()
    "Executes rake device."
    (interactive)
    (motion-execute-rake-command "device"))
  (defun jojo/rake-pod-install ()
    "Executes rake install."
    (interactive)
    (motion-execute-rake-command "pod:install"))
  (defun jojo/rake-pod-update ()
    "Executes rake update."
    (interactive)
    (motion-execute-rake-command "pod:update"))
  (defun jojo/rake-clean ()
    "Executes rake update."
    (interactive)
    (motion-execute-rake-command "clean"))
  (defun jojo/rake-spec ()
    "Executes rake spec."
    (interactive)
    (motion-execute-rake-command "spec"))
  (defun jojo/rake-run-sim ()
    "Tries to reload motion app.
If failure, run rake instead."
    (interactive)
    (unless (ignore-errors (motion-reload-app))
      (motion-execute-rake)))
  (defun jojo/rake-execute (command)
    "Enter in rake command to execute."
    (interactive "sEnter Rake command:")
    (motion-execute-rake-command command))
  (evil-define-key 'normal motion-mode-map (kbd "K") 'motion-dash-at-point)
  (defhydra hydra-motion-mode (:color blue)
    "Motion"
    ("pi" jojo/rake-pod-install "Pod Install")
    ("pu" jojo/rake-pod-update "Pod Update")
    ("c" jojo/rake-clean "Rake Clean")
    ("d" jojo/rake-to-device "Rake to Device")
    ("R" jojo/rake-execute "Rake Execute")
    ("u" jojo/rake-run-sim "Rake Run Sim")
    ("t" jojo/rake-spec "Rake Spec")
    ("r" motion-execute-rake "Execute Rake"))
  (jojo/add-hydra 'mode 'motion-mode))

;;;###autoload
(defun jojo/ruby-bootstrap ()
  "Bootstrap `jojo-ruby'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/ruby-bootstrap auto-mode-alist))
  (ruby-mode))

(provide 'jojo-ruby)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-ruby.el ends here
