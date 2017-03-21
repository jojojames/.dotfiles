;;;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure nil
  :mode ("\\.java\\'" . java-mode)
  :config
  (use-package javadoc-lookup
    :ensure t
    :commands (javadoc-lookup)
    :init
    (evil-define-key 'normal java-mode-map (kbd "K") 'javadoc-lookup))

  ;; https://stackoverflow.com/questions/19953924/how-do-you-run-java-codes-in-emacs
  (defun java-eval-nofocus ()
    "Run current program (that requires no input)."
    (interactive)
    (let* ((source (file-name-nondirectory buffer-file-name))
           (out (file-name-sans-extension source))
           (class (concat out ".class")))
      (save-buffer)
      (shell-command (format "rm -f %s && javac %s" class source))
      (if (file-exists-p class)
          (compile (format "java %s" out))
        (progn
          (set (make-local-variable 'compile-command)
               (format "javac %s" source))
          (command-execute 'compile)))))

  ;; https://emacs.stackexchange.com/questions/7466/how-to-have-emacs-format-indent-javadoc-comments-correctly
  (add-hook 'java-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") (key-binding (kbd "M-j")))))

  (defhydra hydra-java-mode (:color blue)
    ("u" java-eval-nofocus "Eval"))
  (jojo/add-hydra 'mode 'java-mode))

(use-package android-mode
  :ensure t
  :commands (android-root android-mode)
  :init
  (dolist (hook '(java-mode-hook groovy-mode-hook))
    (add-hook hook (lambda ()
                     (when (android-root)
                       (android-mode)
                       (set
                        (make-local-variable 'compilation-scroll-output) t)))))
  :config
  (defhydra hydra-android-mode (:color blue)
    "Android"
    ("a" android-start-app "Start App")
    ("d" android-start-ddms "DDMS")
    ("e" android-start-emulator "Start Emulator")
    ("l" android-logcat "Logcat")
    ("C" android-build-clean "Clean")
    ("t" android-build-test "Test")
    ("c" android-build-debug "Debug")
    ("u" android-build-install "Install")
    ("r" android-build-reinstall "Reinstall")
    ("i" android-build-uninstall "Uninstall"))
  (jojo/add-hydra 'mode 'android-mode)
  (setq android-mode-sdk-dir
        "/Applications/adt-bundle-mac-x86_64-20140702/sdk")
  (setq android-mode-builder 'gradle))

;;;###autoload
(defun jojo/java-bootstrap ()
  "Bootstrap `jojo-java'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/java-bootstrap auto-mode-alist))
  (java-mode))

(provide 'jojo-java)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-java.el ends here
