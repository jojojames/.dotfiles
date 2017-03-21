;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

(when (jojo/windows-p)
  ;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
  (require 'server)
  (and (>= emacs-major-version 23)
       (defun server-ensure-safe-dir (dir) "Noop" t))

  ;; Add git path for windows.
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/")
  (add-to-list 'exec-path "C:/Program Files/Git/bin/")

  (set-face-attribute 'default nil :font "Consolas-10")
  (defun explorer ()
    (interactive)
    (cond
     ;; in buffers with file name
     ((buffer-file-name)
      (shell-command
       (concat "start explorer /e,/select,\""
               (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
     ;; in dired mode
     ((eq major-mode 'dired-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
     ;; in eshell mode
     ((eq major-mode 'eshell-mode)
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
     ;; use default-directory as last resource
     (t
      (shell-command
       (concat "start explorer /e,\""
               (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))
  (global-set-key (kbd "s-j") 'explorer))

(when (jojo/osx-p)
  ;; Delete moving to OSX Trash Bin.
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; https://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Set the shell environment properly.
  (use-package exec-path-from-shell
    :ensure t
    :if (jojo/osx-p)
    :config
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize))

  (defun find-and-set-font (&rest candidates)
    "Set the first font found in CANDIDATES."
    (let ((font (cl-find-if (lambda (f) (find-font (font-spec :name f)))
                            candidates)))
      (when font
        (set-face-attribute 'default nil :font font))
      font))

  (defun jojo/setup-font ()
    "Set font"
    (interactive)
    (if (< (jojo/monitor-width) 1500)
        (find-and-set-font
         "Source Code Pro-12"
         "Mononoki-12"
         "SF Mono-11"
         "Menlo-11"
         "Monaco-11")
      (find-and-set-font
       "Source Code Pro-12"
       "Menlo-12"
       "DejaVu Sans Mono-11"
       "Consolas-12"
       "SF Mono-12"
       "mononoki-12")))

  (jojo/setup-font)

  ;; When window changes size, update font used.
  (setq window-size-change-functions
        '((lambda (change) (jojo/setup-font))))

  ;; Don't let osx swallow Meta key.
  (setq mac-pass-command-to-system nil)

  ;; Command -> Meta
  ;; Option -> Super
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

  ;; Set Command+q to quit.
  (global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

  ;; Use the osx emoji font for emoticons.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      (font-spec :name "Apple Color Emoji") nil 'prepend))

  ;; Mac uses menubar.
  (when window-system
    (menu-bar-mode 1))

  ;; Reveal in finder.
  (use-package reveal-in-osx-finder
    :ensure t
    :commands (reveal-in-osx-finder)
    :config
    (global-set-key (kbd "s-r") 'reveal-in-osx-finder)))

(when (jojo/linux-p)
  ;; Disable ui fluff.
  ;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  ;; (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  (defun jojo/update-path-var ()
    "Update $PATH with list of directories."
    (interactive)
    (setenv "PATH"
            (concat (mapconcat
                     (lambda (path)
                       (expand-file-name path))
                     '("~/Code/love/src") ":")
                    ":" (getenv "PATH"))))

  (jojo/update-path-var)

  (use-package xclip
    :ensure t
    :config
    (xclip-mode 1))
  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (set-face-attribute 'default nil :height 110)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; Work with stumpwm if it exists.
  ;; This probably needs testing on Linux where stumpwm is on the path
  ;; but not being used.
  ;; http://code.ryuslash.org/dot/tom/emacs/tree/.emacs.d/init.org
  (defvar oni:stumpish-program
    (expand-file-name (concat user-emacs-directory "lang/commonlisp/stumpish"))
    "The location of the stumpish executable.")

  (defmacro oni:stumpwm (&rest body)
    "Execute BODY in stumpwm."
    (declare (indent 0))
    `(call-process oni:stumpish-program nil nil nil
                   ,(format "eval '%S'" `(progn ,@body))))

  (defun oni:stumpwm-command (cmd)
    "Execute CMD in stumpwm."
    (call-process oni:stumpish-program nil nil nil cmd))

  (defun oni:stumpwm-echo (message)
    (call-process oni:stumpish-program nil nil nil (format "echo %s" message)))

  (defun jojo/windmove-do-window-select (orig-fun &rest args)
    "Integrate with Stumpwm."
    (interactive)
    (condition-case err
        (apply orig-fun args)
      (error
       (when (executable-find "stumpwm")
         (oni:stumpwm-command
          (format "move-focus %s" (nth 0 args)))))))
  (advice-add 'windmove-do-window-select :around #'jojo/windmove-do-window-select))

(provide 'jojo-platform)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-platform.el ends here
