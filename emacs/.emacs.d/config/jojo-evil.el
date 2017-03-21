;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

;; Vim Style Undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  ;; Double undo limit
  (setq undo-limit 160000)
  (setq undo-strong-limit 240000)
  (setq undo-outer-limit 24000000)
  (global-undo-tree-mode))

(use-package evil
  :load-path "~/.emacs.d/fork/evil"
  :ensure t
  :init
  (setq evil-want-C-u-scroll t) ;; Regain scroll up with C-u.
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)

  (setq evil-search-module #'evil-search)
  (setq evil-ex-search-persistent-highlight nil)

  ;; Set underscore to be a word.
  ;; https://bitbucket.org/lyro/evil/wiki/Home#!underscore-_-is-not-a-word-character
  (dolist (hook
           (append (jojo/lisp-hooks)
                   '(erlang-mode-hook
                     c-mode-common-hook
                     csharp-mode-hook
                     elixir-mode-hook
                     python-mode-hook
                     ruby-mode-hook
                     coffee-mode-hook
                     text-mode-hook)))
    (add-hook hook (lambda ()
                     (modify-syntax-entry
                      (string-to-char "_")
                      "w"))))
  :config
  ;; http://spacemacs.org/doc/FAQ
  ;; https://github.com/syl20bnr/spacemacs/issues/2032
  ;; https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard/15054#15054
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-flash-delay 8) ;; control the highlight time of searches
  (defhydra hydra-window (:color blue :columns 4)
    "Window Commands"
    ("." winner-redo "Redo Layout")
    ("/" winner-undo "Undo Layout")
    ("-" split-window-below "Split Below")
    ("|" split-window-right "Split Right")
    ("\\" split-window-right "Split Right")
    ("=" balance-windows "Balance Windows")
    ("h" split-window-below "Split Below")
    ("q" nil "Cancel")
    ("r" jojo/resize-window "Resize Window")
    ("s" toggle-window-split "Rotate Split")
    ("t" rotate-windows "Swap Windows")
    ("v" split-window-right "Split Right")
    ("w" toggle-frame-maximized "Maximize")
    ("c" jojo/switch-to-buffer-replace-old-window-with-current-buffer
     "Switch Buffer & Replace"))

  (defhydra hydra-system-process (:color blue :columns 4)
    "System Processes and Profiler"
    ("P" profiler-start "Start Profiler")
    ("R" profiler-report "Report Profiler")
    ("S" profiler-stop "Stop Profiler")
    ("L" list-processes "List Processes")
    ("q" nil "Cancel"))

  (defhydra hydra-swiper (:color blue :columns 3)
    "Swiper Commands"
    ("s" swiper "Swiper")
    ("c" counsel-ag "Counsel AG")
    ("f" counsel-find-file "Counsel Find File")
    ("o" occur "Occur")
    ("m" multi-occur "Multi Occur")
    ("a" ag "AG")
    ("p" ag-project "AG Project"))

  ;; Set default actions for various leader keys.
  (jojo/add-hydra 'indent 'fundamental-mode #'indent-region-or-buffer)

  (dolist (symbol '(mode test debug eval compile))
    (jojo/add-hydra symbol
                    'fundamental-mode
                    (lambda ()
                      (interactive)
                      (message (concat "No "
                                       (s-capitalize (symbol-name symbol))
                                       " Bindings")))))

  (defhydra hydra-space (:color blue :hint nil :idle .2)
    "
     EVIL:

     Find        Window              Navigate          Misc            Options
---------------------------------------------------------------------------------------
 _f_: File       _w_: Window            _h_: Left        _s_: Swiper         _m_: Mode
 _b_: Buffer     _-_: Split Below       _l_: Right       _v_: Config         _d_: Debug
 _r_: Recent     _|_: Split Right       _k_: Up          _P_: Processes      _e_: Eval
 _n_: Tree       _<backspace>_: Delete  _j_: Down        _=_: Indent         _q_: OS
 _p_: Project    _F_: Frame             _/_: Next        _+_: Whitespace     _t_: Test
 _g_: Git                             _._: Previous    _SPC_: Jump         _0_: Smartparens
"
    ("0" hydra-smartparens/body)
    ("9" hydra-smartparens/body)
    ("f" jojo/find-file-dwim)
    ("F" make-frame)
    ("5" make-frame)
    ("b" jojo/buffers-dwim)
    ("r" jojo/recentf-dwim)
    ("w" hydra-window/body)
    ("-" split-window-below)
    ("|" split-window-right)
    ("\\" split-window-right)
    ("h" windmove-left)
    ("l" windmove-right)
    ("k" windmove-up)
    ("j" windmove-down)
    ("." jojo/evil-jump-forward-dwim)
    ("/" jojo/evil-jump-backward-dwim)
    ("<backspace>" delete-window)
    ("s" hydra-swiper/body)
    ("v" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    ("P" hydra-system-process/body)
    ("+" whitespace-region-or-buffer-cleanup)
    ("u" undo-tree-visualize)
    ("x" kill-buffer)
    ("q" hydra-term/body)
    ("n" neotree-dwim)
    ("p" hydra-projectile/body)
    ("g" hydra-magit/body)
    ("SPC" ace-jump-mode)
    ("=" (lambda () (interactive) (jojo/run-hydra 'indent)))
    ("H" (lambda () (interactive) (jojo/run-hydra 'help)))
    ("c" (lambda () (interactive) (jojo/run-hydra 'compile)))
    ("d" (lambda () (interactive) (jojo/run-hydra 'debug)))
    ("m" (lambda () (interactive) (jojo/run-hydra 'mode)))
    ("e" (lambda () (interactive) (jojo/run-hydra 'eval)))
    ("t" (lambda () (interactive) (jojo/run-hydra 'test))))

  (defun jojo/evil-jump-backward-dwim ()
    (interactive)
    (let ((original-buffer (current-buffer)))
      (while (and
              (> (length
                  (ring-elements (evil--jumps-get-window-jump-list)))
                 (+ 1
                    (evil-jumps-struct-idx (evil--jumps-get-current))))
              (eq original-buffer (current-buffer)))
        (evil--jump-backward 1))

      ;; We weren't able to jump backward through marks,
      ;; so fall back and use the previous buffer.
      (when (eq original-buffer (current-buffer))
        (message "Jumping backwards using `evil-prev-buffer'.")
        (evil-prev-buffer))))

  (defun jojo/evil-jump-forward-dwim ()
    (interactive)
    (let ((original-buffer (current-buffer)))
      (while (and
              (< 0
                 (evil-jumps-struct-idx (evil--jumps-get-current)))
              (eq original-buffer (current-buffer)))
        (evil--jump-forward 1))

      ;; We weren't able to jump backward through marks,
      ;; so fall back and use the previous buffer.
      (when (eq original-buffer (current-buffer))
        (message "Jumping forwards using `evil-next-buffer'.")
        (evil-next-buffer))))

  (eval-after-load 'evil
    (lambda ()
      (evil-define-key 'normal global-map (kbd "<SPC>") #'hydra-space/body)
      (evil-define-key 'motion global-map (kbd "<SPC>") #'hydra-space/body)
      (evil-define-key 'visual global-map (kbd "<SPC>") #'hydra-space/body)))

  ;; C-j jumps foward in jumplist, C-o goes the other way
  (setq evil-want-C-i-jump nil)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-jump-forward)

  ;; swap the ` and ' keys
  (define-key evil-motion-state-map "'" 'evil-goto-mark)
  (define-key evil-motion-state-map "`" 'evil-goto-mark-line)

  (setq evil-normal-state-tag   (propertize " NORMAL ")
        evil-emacs-state-tag    (propertize " EMACS ")
        evil-insert-state-tag   (propertize " INSERT ")
        evil-motion-state-tag   (propertize " MOTION ")
        evil-visual-state-tag   (propertize " VISUAL ")
        evil-operator-state-tag (propertize " OPERATOR "))

  (jojo/evil-add-default-state
   'motion '(package-menu-mode
             etags-select-mode
             occur-mode
             debugger-mode
             messages-buffer-mode
             flycheck-error-list-mode))

  ;; Start ERC in Normal Mode
  (delete 'erc-mode evil-insert-state-modes)
  (evil-mode 1)

  ;; https://github.com/syl20bnr/spacemacs/issues/646
  ;; https://bitbucket.org/lyro/evil/pull-requests/13/add-new-configuration-variable-evil-move/diff
  ;; https://github.com/luxbock/evil-cleverparens
  (setq evil-move-beyond-eol t)

  ;; Keybinds
  ;; reselect text after identing
  ;; https://superuser.com/questions/469327/combining-two-operators-in-evil-mode-emacs
  (define-key evil-visual-state-map "g>" 'evil-shift-right)
  (define-key evil-visual-state-map "g<" 'evil-shift-left)
  (define-key evil-visual-state-map ">" (kbd "g>gv"))
  (define-key evil-visual-state-map "<" (kbd "g<gv"))

  ;; Let Emacs use M-. for goto definition
  (define-key evil-normal-state-map (kbd "M-.") nil)

  ;; compilation mode rebinds
  (add-hook
   'compilation-mode-hook
   '(lambda ()
      (local-unset-key "g")
      (local-unset-key "h")
      (local-unset-key "0")
      (evil-define-key
        'motion compilation-mode-map "0"
        'evil-digit-argument-or-evil-beginning-of-line)
      (evil-define-key
        'motion compilation-mode-map "r" 'recompile)
      (evil-define-key
        'motion compilation-mode-map "h" 'evil-backward-char)))

  ;; No idea why `special-mode' behaves badly when evil-magit is deferred.
  (eval-after-load 'special-mode
    (lambda ()
      (eval-after-load 'evil
        (lambda ()
          (evil-define-key 'normal
            special-mode-map (kbd "q")
            'kill-current-buffer-and-its-windows)))))

  (add-hook 'evil-list-view-mode-hook
            (lambda ()
              (evil-define-key 'motion evil-list-view-mode-map
                (kbd "q") 'kill-current-buffer-and-its-windows)))

  ;; search with ag
  (define-key evil-normal-state-map "g?" #'ag-project)
  (define-key evil-visual-state-map "g?" #'ag-project)

  ;; comint-mode
  (add-hook 'comint-mode-hook
            '(lambda ()
               (evil-define-key 'insert comint-mode-map (kbd "<up>")
                 'comint-previous-input)
               (evil-define-key 'insert comint-mode-map (kbd "<down>")
                 'comint-next-input)))

  ;; jumping around
  (define-key evil-normal-state-map "gp" #'ace-jump-mode)
  (define-key evil-normal-state-map "gP" #'ace-jump-mode-pop-mark)

  ;; mode help
  (add-hook 'help-mode-hook
            '(lambda ()
               (evil-define-key 'motion help-mode-map (kbd "g.") 'push-button)))

  ;; occur mode
  (evil-set-initial-state 'occur-mode 'motion)
  (add-hook 'occur-mode-hook
            '(lambda ()
               (evil-define-key 'motion occur-mode-map
                 (kbd "RET") 'occur-mode-goto-occurrence
                 (kbd "q") 'kill-current-buffer-and-its-windows)))

  (add-hook 'profiler-report-mode-hook
            '(lambda ()
               ;; Using define-key instead of evil-define-key
               ;; because TAB doesn't play nice with Magit(etc?).
               (define-key profiler-report-mode-map (kbd "TAB") 'profiler-report-toggle-entry)
               (define-key profiler-report-mode-map [tab] 'profiler-report-toggle-entry)
               (define-key profiler-report-mode-map (kbd "g.") 'profiler-report-find-entry)))

  ;; package mode bindings
  (add-hook 'package-menu-mode-hook
            '(lambda ()
               (evil-add-hjkl-bindings package-menu-mode-map 'emacs
                 (kbd "/") 'evil-search-forward
                 (kbd "n") 'evil-search-next
                 (kbd "N") 'evil-search-previous)))

  (setq evil-default-cursor t))  ; fix black cursor

(use-package dired
  :ensure nil
  :commands (dired)
  :init
  (defun jojo/dired-find-file ()
    "In Dired, visit the file or directory named on this line.
If a file, open in other window."
    (interactive)
    (let ((find-file-run-dired t)
          (dired-file-name (dired-get-file-for-visit)))
      (if (file-directory-p dired-file-name)
          ;; Copied from `dired-find-file'.
          (find-file dired-file-name)
        ;; Copied from `dired-display-file'.
        (display-buffer (find-file-noselect dired-file-name) t))))

  (add-hook 'dired-mode-hook
            (lambda ()
              (eval-after-load 'evil
                (lambda ()
                  ;; Wipe the "g" so `evil' can use "gg".
                  (define-key dired-mode-map (kbd "g") nil)
                  (evil-define-key 'normal dired-mode-map "G" 'evil-goto-line)
                  (evil-define-key 'normal dired-mode-map "w" 'evil-forward-word-begin)
                  (evil-define-key 'normal dired-mode-map "Y" 'dired-copy-filename-as-kill)
                  (evil-define-key 'normal dired-mode-map "v" 'evil-visual-char)
                  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
                  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
                  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
                  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
                  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
                  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
                  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
                  (evil-define-key 'normal dired-mode-map (kbd "RET") 'jojo/dired-find-file)))))
  :config
  (defhydra hydra-dired-mode (:color teal :columns 3)
    "Dired"
    ("m" hydra-dired-mark/body "Marks")
    ("o" hydra-dired-operate/body "Operate"))

  (defhydra hydra-dired-operate-encrypt (:color teal :columns 3)
    "Encrypt"
    ("d" epa-dired-do-decrypt "Decrypt")
    ("v" epa-dired-do-verify "Verify")
    ("s" epa-dired-do-sign "Sign")
    ("e" epa-dired-do-encrypt "Encrypt"))

  (defhydra hydra-dired-operate-image (:color teal :columns 3)
    "Images"
    ("d" image-dired-delete-tag "Delete Tag")
    ("t" image-dired-tag-files "Tag Files")
    ("c" image-dired-dired-comment-files "Comment Files")
    ("s" image-dired-display-thumbs "Display Thumbs"))

  (defhydra hydra-dired-operate-change (:color teal :columns 3)
    "Change"
    ("c" dired-do-chown "Chown")
    ("g" dired-do-chgrp "Change Group")
    ("h" dired-do-chmod "Chmod")
    ("t" dired-do-touch "Touch"))

  (defhydra hydra-dired-operate-elisp (:color teal :columns 3)
    "Elisp"
    ("l" dired-do-load "Load Elisp File")
    ("b" dired-do-byte-compile "Byte Compile"))

  (defhydra hydra-dired-operate (:color teal :columns 3)
    "Operate"
    ("I" hydra-dired-operate-image/body "Images")
    ("E" hydra-dired-operate-encrypt/body "Encrypt")
    ("C" hydra-dired-operate-change/body "Change")
    ("L" hydra-dired-operate-elisp/body "Elisp")
    ("q" dired-do-query-replace-regexp "Query Replace Regexp")
    ("f" dired-do-search "Search")
    ("S" dired-do-isearch-regexp "Isearch Regexp")
    ("s" dired-do-isearch "Isearch")
    ("c" dired-do-compress "Compress")
    ("p" dired-do-print "Print")
    ("L" dired-do-hardlink "Hardlink")
    ("l" dired-do-symlink "Symlink")
    ("!" dired-do-async-shell-command "Async Shell Shell")
    ("@" dired-do-shell-command "Shell Command")
    ("d" dired-do-delete "Delete")
    ("r" dired-do-rename "Rename")
    ("c" dired-do-copy "Copy")
    ("y" dired-do-copy "Copy"))

  (defhydra hydra-dired-mark (:color teal :columns 3)
    "Mark"
    ("P" dired-prev-marked-file "Move to Previous Mark")
    ("N" dired-next-marked-file "Move to Next Mark")
    ("C" dired-change-marks "Change Marks")
    ("U" dired-unmark-all-marks "Unmark all Marks")
    ("S" dired-mark-symlinks "Mark Symlinks")
    ("D" dired-mark-directories "Mark Directories")
    ("c" dired-clean-directory "Clean Directory")
    ("x" dired-mark-executables "Mark Executables")
    ("g" dired-flag-garbage-files "Flag Garbage Files")
    ("b" dired-flag-backup-files "Flag Backup Files")
    ("s" dired-flag-auto-save-files "Flag Autosave Files")
    ("d" dired-flag-file-deletion "Flag File Deletion")
    ("u" dired-unmark "Unmark")
    ("m" dired-mark "Mark")
    ("T" dired-toggle-marks "Toggle Marks"))

  (jojo/add-hydra 'mode 'dired-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :ensure t
  :init
  ;; Advise star/pound search to be case insensitive.
  ;; This advice could also go into the evil package.
  ;; This probably causes the search highlight to flicker
  ;; while going to the next search candidate.
  (defun jojo/evil-visual-star-ignorecase (orig-fun &rest args)
    (interactive)
    (let ((evil-ex-search-case 'insensitive))
      (apply orig-fun args)))

  (advice-add 'evil-ex-search-word-backward :around #'jojo/evil-visual-star-ignorecase)
  (advice-add 'evil-ex-search-word-forward :around #'jojo/evil-visual-star-ignorecase)
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent t))

;;; magit integration
(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement t))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
(global-set-key [escape] 'minibuffer-keyboard-quit)

;;; evil surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;; tree like vim
(use-package neotree
  :ensure t
  :load-path "~/.emacs.d/fork/emacs-neotree/"
  :commands (neo-global--window-exists-p
             neotree-dir
             neotree-toggle
             neotree-enter
             neotree-hide)
  :init
  (setq neo-persist-show nil)
  (setq neo-window-fixed-size nil)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-mode-line-type 'neotree)
  (setq neo-show-hidden-files t)
  (setq neo-mode-line-type 'none)
  (defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (defun neotree-reopen (&rest _args)
    "Close and reopen neotree returning to the current window after.
`neotree-resize-window' doesn't work on file open (neotree window isn't live anymore).
Hiding and showing again will resize the neotree window properly."
    (interactive)
    (save-selected-window
      (neotree-hide)
      (neotree-show)))

  (defun neotree-resize-window-with-timer (&rest args)
    "Resize neotree window after waiting for redisplay."
    (when (neo-global--window-exists-p)
      (redisplay)
      (run-with-timer .1 nil #'neotree-resize-window args)))

  ;; When opening a file from neotree.
  (advice-add 'neo-open-file :after #'neotree-reopen)
  ;; When going up the directory stack.
  (advice-add 'neotree-change-root :after #'neotree-resize-window)
  ;; When opening a directory node.
  (advice-add 'neo-open-dir :after #'neotree-resize-window)
  ;; When showing neotree with `neotree-show'.
  (advice-add 'neotree-show :after #'neotree-resize-window)
  ;; Inside neotree-dwim
  (advice-add 'neotree-dir :after #'neotree-resize-window)
  ;; Inside neotree-dwim
  (advice-add 'neotree-find :after #'neotree-resize-window)

  (defun jojo/neotree-around-reopen (orig-fun &rest args)
    "Close neotree before calling advised function.
Reopen neotree after advised function runs."
    (interactive)
    (if (not (neo-global--window-exists-p))
        (apply orig-fun args)
      (save-selected-window
        (neotree-hide)
        (let ((res (apply orig-fun args)))
          (neotree-show)
          res))))

  (defun neotree-dwim ()
    "Open neotree with projectile as root and open node for current file.
If projectile unavailable or not in a project, open node at file path.
If file path is not available, open $HOME."
    (interactive)
    (if (neo-global--window-exists-p)
        (call-interactively 'neotree-hide)
      (cond
       ((derived-mode-p 'magit-mode)
        (let ((magit-relative-file (magit-file-at-point)))
          (neotree-dir (magit-toplevel))
          (when magit-relative-file
            (neotree-find
             (magit-expand-git-file-name magit-relative-file)))))
       (t
        (let ((file-name (buffer-file-name)))
          (if (and (not file-name)
                   (let ((buffer-name (buffer-name)))
                     (cond
                      ((equal buffer-name "*cider-repl server*") nil)
                      (t t))))
              (neotree-dir "~/")
            (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                     (projectile-project-p))
                                (projectile-project-root)
                              (file-name-directory file-name))))
              (neotree-dir dir-name)
              (neotree-find file-name))))))))

  (defhydra hydra-neotree-mode (:color teal :columns 3)
    "Neotree"
    ("A" neotree-stretch-toggle "Toggle Stretch Window")
    ("c" neotree-create-node "Create")
    ("d" neotree-delete-node "Delete")
    ("g" neotree-refresh "Refresh")
    ("h" neotree-hidden-file-toggle "Toggle Hidden Files")
    ("n" neotree-create-node "Create")
    ("r" neotree-rename-node "Rename")
    ("R" neotree-resize-window "Resize Window")
    ("q" nil "Cancel")
    ("y" neotree-copy-node "Copy"))

  (jojo/add-hydra 'mode 'neotree-mode)

  (add-hook 'neotree-mode-hook
            (lambda ()
              (eval-after-load 'evil
                (lambda ()
                  (define-key evil-normal-state-local-map
                    (kbd "r") 'neotree-resize-window)
                  (define-key evil-normal-state-local-map
                    (kbd "R") 'neotree-resize-window)
                  (define-key evil-normal-state-local-map
                    (kbd "TAB") 'neotree-enter)
                  (define-key evil-normal-state-local-map
                    (kbd "q") 'neotree-hide)
                  (define-key evil-normal-state-local-map
                    (kbd "RET") 'neotree-enter)
                  (define-key evil-normal-state-local-map
                    (kbd "<S-return>") 'neotree-quick-look)))))
  :config
  ;; These advices go after :config because we don't want to use them
  ;; until after neotree has loaded and is active.

  ;; Upon resizing window
  (advice-add #'toggle-frame-maximized
              :after #'neotree-resize-window-with-timer)

  ;; :align with shackle doesn't work when neotree is open.
  ;; Close it while shackle is running and then reopen.
  (eval-after-load 'shackle
    (lambda ()
      (advice-add 'shackle--display-buffer-aligned-window :around
                  #'jojo/neotree-around-reopen)))

  ;; Try to use icons from `all-the-icons',
  ;; if font is not installed, we use fallback.
  (if (null (find-font (font-spec :name "github-octicons")))
      (setq neo-theme 'nerd)
    (use-package all-the-icons :ensure t)
    (setq neo-theme 'icons))

  ;; Sort neotree hidden filepaths to the bottom.
  (setq neo-filepath-sort-function 'neo-sort-hidden-last))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)
              ("gy" . evil-commentary-yank)
              ("s-/" . evil-commentary-line))
  :init
  :config
  (evil-commentary-mode))

(provide 'jojo-evil)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-evil.el ends here
