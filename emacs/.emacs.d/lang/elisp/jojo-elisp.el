;;;; -*- lexical-binding: t; -*-

(use-package elisp-mode
  :ensure nil
  :config
  (jojo/evil-default-state 'emacs '(edebug-mode))

  ;; https://github.com/jwiegley/use-package/issues/152
  ;; Edebug a defun or defmacro
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp (concat "(\\s-*"
                                    "\\(defun\\|defmacro\\)\\s-+"
                                    "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-mark-and-excursion
       (search-backward-regexp modi/fns-regexp)
       (setq fn (match-string 1))
       (mark-sexp)
       (narrow-to-region (point) (mark))
       (if (member fn modi/fns-in-edebug)
           ;; If the function is already being edebugged, uninstrument it
           (progn
             (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
             (eval-region (point) (mark))
             (setq-default eval-expression-print-length 12)
             (setq-default eval-expression-print-level  4)
             (message "Edebug disabled: %s" fn))
         ;; If the function is not being edebugged, instrument it
         (progn
           (add-to-list 'modi/fns-in-edebug fn)
           (setq-default eval-expression-print-length nil)
           (setq-default eval-expression-print-level  nil)
           (edebug-defun)
           (message "Edebug: %s" fn)))
       (widen))))

  (defun jojo/goto-to-scratch-buffer ()
    "Move to *scratch* buffer."
    (interactive)
    (pop-to-buffer "*scratch*"))

  (jojo/defhydra
   (hydra-emacs-lisp-debug hydra-lisp-interaction-debug)
   (:color blue)
   "Elisp Debug"
   ("q" cancel-debug-on-entry "Cancel")
   ("f" debug-on-entry "Debug On Entry")
   ("d" modi/toggle-edebug-defun "Toggle Modi"))

  (jojo/defhydra
   (hydra-emacs-lisp-eval hydra-lisp-interaction-eval)
   (:color blue :columns 3)
   "Elisp Eval"
   ("r" eval-region "Region")
   ("e" eval-last-sexp "Sexp")
   ("d" eval-defun "Defun")
   ("p" eval-print-last-sexp "Sexp Print")
   ("x" eval-last-sexp-and-replace "Sexp And Replace")
   ("b" eval-buffer "Buffer"))

  (jojo/defhydra
   (hydra-emacs-lisp-mode hydra-lisp-interaction-mode)
   (:color blue :columns 3)
   "Elisp"
   ("c" jojo/goto-to-scratch-buffer "Scratch")
   ("u" emacs-lisp-byte-compile "Byte Compile")
   ("l" emacs-lisp-byte-compile-and-load "Byte Compile And Load")
   ("rd" byte-recompile-directory "Recompile Directory")
   ("rD" disassemble "Disassemble")
   ("z" ielm "IELM")
   ("e" hydra-emacs-lisp-eval/body "Eval")
   ("d" hydra-emacs-lisp-debug/body "Debug"))

  (jojo/add-hydra '(mode eval debug)
                  '(emacs-lisp-mode lisp-interaction-mode))

  (jojo/set-evil-shift-width lisp-body-indent))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :commands (turn-on-elisp-slime-nav-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :config
  (defvar jojo/elisp-navigation-stack '() "Stack used to navigate tags.")
  (defun jojo/elisp-gtd-dwim ()
    "DWIM Goto Definition."
    (interactive)
    (condition-case nil
        (progn
          (call-interactively 'elisp-slime-nav-find-elisp-thing-at-point)
          (push 'elisp-slime-nav-mode jojo/elisp-navigation-stack))
      (error
       (progn
         (call-interactively 'dumb-jump-go)
         (push 'dumb-jump-mode jojo/elisp-navigation-stack)))))

  (defun jojo/elisp-gtd-back ()
    "DWIM Pop Goto Definition."
    (interactive)
    (let ((method (pop jojo/elisp-navigation-stack)))
      (cond
       ((eq method 'elisp-slime-nav-mode) (pop-tag-mark))
       ((eq method 'dumb-jump-mode) (dumb-jump-back))
       (t
        (message "No tag found to pop.")))))

  (eval-after-load 'evil
    (lambda ()
      (evil-define-key 'normal elisp-slime-nav-mode-map
        (kbd "g.") 'jojo/elisp-gtd-dwim
        (kbd "g/") 'jojo/elisp-gtd-back
        (kbd "gr") 'jojo/find-references
        (kbd "K")  'elisp-slime-nav-describe-elisp-thing-at-point)))

  (turn-on-elisp-slime-nav-mode)
  (defadvice elisp-slime-nav-describe-elisp-thing-at-point (after slime-move-to-doc activate)
    "Move point to the other window after opening up documentation window."
    (pop-to-buffer "*Help*")))

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode)
  :init
  (add-hook 'cider-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'clojure-mode-hook 'eval-sexp-fu-flash-mode)
  (add-hook 'emacs-lisp-mode-hook 'eval-sexp-fu-flash-mode)
  :config
  (eval-sexp-fu-flash-mode 1))

(provide 'jojo-elisp)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-elisp.el ends here
