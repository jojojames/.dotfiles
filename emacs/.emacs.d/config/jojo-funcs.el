;;;; -*- lexical-binding: t; -*-
(use-package subr-x :ensure nil)

(defmacro jojo/cached-boolean (symbol func)
  "Return `symbol' using `func' as its definition."
  `(symbol-value
    (if (boundp ',symbol)
        ,symbol
      (defvar ,symbol ,func))))

(defun jojo/windows-p ()
  "Are we on windows?"
  (jojo/cached-boolean on-windows (eq system-type 'windows-nt)))

(defun jojo/osx-p ()
  "Are we on osx?"
  (jojo/cached-boolean on-osx (eq system-type 'darwin)))

(defun jojo/linux-p ()
  "Are we on linux?"
  (jojo/cached-boolean on-linux (or
                                 (eq system-type 'gnu/linux)
                                 (eq system-type 'linux))))

(defun jojo/monitor-width ()
  "Return monitor size with focus on Macbook monitor."
  (car (cdr (cdr (cdr (assoc 'geometry (frame-monitor-attributes)))))))

;; use display pixels to determine device
(defvar display-width (jojo/monitor-width))

(defun jojo/macbook-retina-p ()
  "Are we on macbook?"
  (jojo/cached-boolean
   on-macbook-retina
   (let ((width
          (if-let (w
                   (let ((width-result))
                     (dolist (display-alist
                              (display-monitor-attributes-list) width-result)
                       ;; (name . "Color LCD")
                       (when-let (name-of-display
                                  (cdr (assoc 'name display-alist)))
                         (when (string-match-p "Color LCD"
                                               (cdr (assoc 'name display-alist)))
                           ;; (workarea 0 23 1280 709)
                           (setq
                            width-result
                            (car
                             (cdr (cdr (cdr
                                        (assoc 'workarea display-alist)))))))))))
              w
            ;; Give up and return display-pixel-width.
            (jojo/monitor-width))))
     (or (eq width 1440)
         (eq width 1280)))))

(defun jojo/imac-p ()
  "Are we on imac?"
  (jojo/cached-boolean on-imac (> display-width 4400)))

(defun jojo/check-call-success (f)
  "Check if `f' was called successfully.
https://stackoverflow.com/questions/11578723/whats-the-best-way-in-elisp-to-trap-an-error-case"
  (ignore-errors (or (funcall f) t)))

(defun jojo/buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (unless indent-tabs-mode
    (untabify (point-min) (point-max))))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defun whitespace-region-or-buffer-cleanup ()
  "Clean up whitespace in region or buffer"
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (whitespace-cleanup-region (region-beginning) (region-end))
          (message "Cleaned up white space in selected region."))
      (progn
        (whitespace-cleanup)
        (message "Cleaned up white space.")))))

(defun toggle-window-split ()
  "Toggles window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows-helper (x d)
  "Rotates windows."
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))

(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun jojo/explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (cond
   ((and (jojo/osx-p)
         (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and (jojo/windows-p)
         (fboundp #'explorer))
    (explorer))
   (t
    (message "Implement `explorer-finder' for this OS!"))))

(defun jojo/open-shell ()
  "Opens up a specific terminal depending on operating system."
  (interactive)
  (cond
   ((jojo/osx-p) (multi-term))
   ((jojo/windows-p) (eshell))
   (t
    (message "Implement `jojo/open-shell' for this OS!"))))

(defun jojo/open-terminal ()
  "Open system terminal."
  (interactive)
  (cond
   ((jojo/osx-p)
    (shell-command
     ;; open -a Terminal doesn't allow us to open a particular directory unless
     ;; We use --args AND -n, but -n opens an entirely new Terminal application
     ;; instance on every call, not just a new window. Using the
     ;; bundle here always opens the given directory in a new window.
     (concat "open -b com.apple.terminal " default-directory) nil nil))
   (t
    (message "Implement `jojo/open-terminal' for this OS!"))))

(defun jojo/find-references ()
  "When we don't have find-usages/find-references,
do a search for the string from projet root to mimic that functionality."
  (interactive)
  (cond
   ((jojo/osx-p)
    (ag-project (ag/dwim-at-point)))
   ((jojo/linux-p)
    (ag-project (ag/dwim-at-point)))
   ((jojo/windows-p)
    (call-interactively #'rgrep))
   (t
    (message "Implement `jojo/find-references' for this OS!"))))

;; https://stackoverflow.com/questions/32977277/emacs-backspace-at-beginning-of-tabbed-line-similar-to-intellij
(defun intellij-backspace ()
  "Provides an intellij-like backspace."
  (interactive)
  (let* ((end (save-excursion
                (end-of-line)
                (point)))
         (beginning (save-excursion
                      (beginning-of-line)
                      (point))))
    (if (string-match "^[ \t]+$" (buffer-substring beginning end))
        (progn
          (beginning-of-line)
          (kill-line)
          (forward-line -1)
          (indent-for-tab-command)
          (end-of-line))
      (progn
        (if (and
             (bound-and-true-p smartparens-mode)
             (bound-and-true-p smartparens-strict-mode))
            (sp-backward-delete-char)
          (backward-delete-char-untabify 1))))))

(defun enable-or-disable-intellij-backspace (enable)
  "If enable, enable intellij-like backspace, otherwise use default backspace."
  (interactive)
  (eval-after-load 'evil
    (lambda ()
      (define-key evil-insert-state-local-map (kbd "DEL")
        (if enable 'intellij-backspace nil)))))

(defun jojo/resize-window ()
  "Resize window to fit contents."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer))))

(defun jojo/switch-to-buffer-replace-old-window-with-current-buffer ()
  "Switch window's buffer to new buffer,
and replace new buffer's old window with `current-buffer.'"
  (interactive)
  (let ((cb (current-buffer)))
    (let ((tb (call-interactively #'switch-to-buffer)))
      (dolist (w (window-list))
        (when (and (eq (window-buffer w) tb)
                   (not (eq (get-buffer-window) w)))
          (set-window-buffer w cb))))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun jojo/bracket-languages ()
  "Return languages that use braces."
  '(c-mode
    c++-mode
    objc-mode
    csharp-mode
    java-mode
    js-mode
    js2-mode
    php-mode
    json-mode
    ruby-mode
    motion-mode
    swift-mode
    css-mode))

(defun jojo/lisp-modes ()
  "Return modes that are lispy."
  '(lisp-mode
    lisp-interaction-mode
    emacs-lisp-mode
    common-lisp-mode
    slime-mode
    clojure-mode
    cider-mode
    cider-repl-mode
    scheme-mode
    geiser-mode
    geiser-repl-mode))

(defun jojo/lisp-hooks ()
  "Return hooks that are lispy."
  (mapcar #'jojo/mode-hook (jojo/lisp-modes)))

(defun jojo/mode-hook (mode)
  "Return hook given `mode' symbol."
  (intern (concat (symbol-name mode) "-hook")))

(defun jojo/use-2-spaces-p ()
  "Determine through various heuristics if
project should be on two space indent."
  (cond
   ((eq major-mode 'web-mode)
    ;; Elixir/Phoenix projects should always use 2.
    (when-let ((bf-name (buffer-file-name)))
      (string-match-p "eex" bf-name)))
   ((eq major-mode 'js2-mode)
    (when-let ((project-root (projectile-project-root)))
      ;; phoenix/rumbl project is Elixir.
      (string-match-p "rumbl" project-root)))
   ((eq major-mode 'css-mode)
    (when-let ((project-root (projectile-project-root)))
      ;; phoenix/rumbl project is Elixir.
      (string-match-p "rumbl" project-root)))
   (t
    ;; Default to 4 spaces.
    nil)))

(defun jojo/project-c-style ()
  "Determine c style of project."
  (let ((proot (projectile-project-root)))
    (cond
     ((string-match-p "gpicker" proot) "linux-tabs-only")
     (t
      "k&r"))))

(defun kill-current-buffer-and-its-windows ()
  "Wraps `kill-buffer-and-its-windows' using with current buffer.
This saves 1 extra keypress."
  (interactive)
  (kill-buffer-and-its-windows (current-buffer)))

;; https://www.emacswiki.org/emacs/misc-cmds.el
(defun kill-buffer-and-its-windows (buffer)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when (interactive-p)
      (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

(defun jojo/mode-map-from-hook (hook)
  "Return mode-map from hook."
  (let ((hook-str (symbol-name hook)))
    (symbol-value (intern
                   (concat
                    (substring hook-str 0
                               (cl-search "-" hook-str :from-end t))
                    "-map")))))

(defun jojo/evil-default-state (state modes)
  "Set default evil state using mode hooks."
  (eval-after-load 'evil
    (lambda ()
      (cl-loop
       for mode in modes
       do (let ((hook (concat (symbol-name mode) "-hook")))
            (add-hook (intern hook)
                      `(lambda ()
                         (if (not ,mode)
                             (evil-normal-state)
                           (cond
                            ((eq 'motion ',state) (evil-motion-state))
                            ((eq 'insert ',state) (evil-insert-state))
                            ((eq 'emacs ',state) (evil-emacs-state))
                            ((eq 'visual ',state) (evil-visual-state))
                            (t (evil-normal-state)))))))))))

(defun jojo/evil-add-default-state (state modes)
  "Set default evil state using evil-state-modes."
  (eval-after-load 'evil
    (lambda ()
      (cl-loop
       for mode in modes
       do (let ((state-modes (cond
                              ((eq 'motion state) 'evil-motion-state-modes)
                              ((eq 'insert state) 'evil-insert-state-modes)
                              ((eq 'emacs state) 'evil-emacs-state-modes)
                              ((eq 'visual state) 'evil-visual-state-modes)
                              (t 'evil-normal-state-modes))))
            (add-to-list state-modes mode))))))

(defun jojo/set-evil-shift-width (width)
  "Changing local indent width based on width passed."
  (make-local-variable 'evil-shift-width)
  (setq evil-shift-width width))

(defun jojo/find-file-dwim ()
  "Find file DWIM."
  (interactive)
  (unless (jojo/check-call-success #'projectile-find-file)
    (if (fboundp 'grizzl-recentf)
        (call-interactively 'find-file)
      (unless (jojo/check-call-success #'counsel-git)
        (unless (jojo/check-call-success #'counsel-find-file)
          (call-interactively 'find-file))))))

(defun jojo/recentf-dwim ()
  "Find recent files DWIM."
  (interactive)
  (unless (jojo/check-call-success #'counsel-recentf)
    (unless (jojo/check-call-success #'grizzl-recentf)
      (ido-recentf-open))))

(defun jojo/buffers-dwim ()
  "List buffers DWIM."
  (interactive)
  (unless (jojo/check-call-success #'ivy-switch-buffer)
    (unless (jojo/check-call-success #'grizzl-buffers)
      (ido-switch-buffer))))

;; macro to define keys for multiple modes
;; modified a little to not have to repeat the 'mode' var
;; https://www.reddit.com/r/emacs/comments/2u5uzq/i_wrote_a_somewhat_useful_elisp_macro/
(defmacro evil-define-multiple (keymaps modes &rest bindings)
  "Macro to allow keymaps to be bound."
  `(progn
     ,@(cl-loop
        for keymap in keymaps
        appending
        (cl-loop
         for mode in modes
         appending
         (cl-loop
          for i from 0 to (- (cl-list-length bindings) 1) when (cl-evenp i)
          collect
          (let ((key (nth i bindings))
                (cmd (nth (+ 1 i) bindings)))
            `(evil-define-key ',mode ,keymap ,key ,cmd)))))))

(defmacro jojo/define-make-function (make-command &optional mode)
  "Create a `make' target command.

`make-command' is expected to be a string and should be the target in a makefile.
`make-command' can also be a list of strings.

    ;; Note the lack of quote for the list.
    (jojo/define-make-function (\"all\" \"test\") 'makefile-mode)

If `mode' is non-nil, `mode' will be preprended to to function's name.

`mode' can be a 'symbol (cons), a symbol or a variable that points to a symbol.

    (jojo/define-make-function \"all\" 'c-mode)
    (jojo/define-make-function \"all\" c-mode)
    (jojo/define-make-function \"all\" mode) ;; `mode' points to 'c-mode.

These are all valid and results in a function named jojo/c-mode-make-all.

If `mode' is nil, something like:

    (jojo/define-make-function \"all\")

will result in a function named jojo/make-all. "
  (let ((commands
         (if (consp make-command)
             make-command
           (list make-command))))
    `(progn
       ,@(cl-loop
          for command in commands
          collect
          (let ((funsymbol
                 (intern (concat "jojo/"
                                 (if mode
                                     (concat
                                      (cond
                                       ((eq (type-of mode) 'cons)
                                        (symbol-name (nth 1 mode)))
                                       ((eq (type-of mode) 'string)
                                        mode)
                                       ((eq (type-of mode) 'symbol)
                                        (if (boundp mode)
                                            (symbol-name (symbol-value mode))
                                          (symbol-name mode)))
                                       (t
                                        ;; throw error
                                        nil))
                                      "-")
                                   nil)
                                 "make-" command))))
            `(defun ,funsymbol ()
               ,(concat "Run make " command ".")
               (interactive)
               (compilation-start
                ,(concat "make " command)
                'compilation-mode (lambda (_mode-name) "*make*") t)))))))

(provide 'jojo-funcs)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-funcs.el ends here
