;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

(use-package company
  :ensure t
  ;; :load-path "~/.emacs.d/fork/company-mode"
  ;; :diminish company-mode
  :init
  (defun jojo/company-visible-and-explicit-action-p ()
    "Determine if tooltip is visible and user explicit action took place."
    (and (company-tooltip-visible-p)
         (company-explicit-action-p)))

  (defun company-ac-setup ()
    "Sets up company to behave similarly to auto-complete mode."
    (setq company-require-match nil)
    (setq company-tooltip-idle-delay .25)
    (setq company-auto-complete #'jojo/company-visible-and-explicit-action-p)
    (setq company-frontends
          '(company-echo-metadata-frontend
            company-pseudo-tooltip-unless-just-one-frontend-with-delay
            company-preview-frontend))
    (define-key company-active-map [tab]
      'company-select-next-if-tooltip-visible-or-complete-selection)
    (define-key company-active-map (kbd "TAB")
      'company-select-next-if-tooltip-visible-or-complete-selection))

  (defun jojo/company-set-prefix-length (len)
    "Changing prefix length locally."
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length len))

  (defun jojo/company-set-delay (delay)
    "Changing delay length locally."
    (make-local-variable 'company-idle-delay)
    (setq company-idle-delay delay))

  (defun jojo/company-set-clang-args (clang-args)
    "Set up clang arguments locally."
    (make-local-variable 'company-clang-arguments)
    (setq company-clang-arguments clang-args))

  (defun jojo/company-backend-in-backends (b)
    "Check if backend b is already in company-backends.
We need to do this check because each backend has additional symbols attached.
Ex. company-clang :with company-yasnippet."
    (let ((in-backend nil))
      (dolist (backend company-backends)
        (when (member b backend)
          (setq in-backend t)))
      in-backend))

  (defun jojo/company-push-backend (b &optional no-merge)
    "Adds backend b to company mode if it's not already in the list of backends.
If `no-merge' is non-nil, don't merge additional backends."
    (unless (jojo/company-backend-in-backends b)
      (add-to-list 'company-backends b))
    (unless no-merge
      (jojo/company-merge-backends)))

  (defun jojo/company-push-backend-local (b &optional no-merge)
    "Push backend into local backends.
If `no-merge' is non-nil, don't merge additional backends."
    (make-local-variable 'company-backends)
    (jojo/company-push-backend b no-merge))

  (defun jojo/company-set-local-backends (backends &optional no-merge)
    "Set backends locally.
If `no-merge' is non-nill, don't merge additional backends."
    (make-local-variable 'company-backends)
    (setq company-backends backends)
    (unless no-merge
      (jojo/company-merge-backends)))
  :config
  (setq company-echo-delay 1)
  (setq company-minimum-prefix-length 1)
  ;; Add additional backend support for all company backends.
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  ;; https://stackoverflow.com/questions/134887/when-to-use-quote-in-lisp
  (defun merge-backend-with-company-backends (backend-to-merge)
    "Merges a backend with every backend in company-backends.
The backend will only be merged if it's not already being used in the current backend.
We do this because so that the backend that we're merging
will always be part of the completion candidates.
For example, merging company-yasnippet to company-capf
will yield (company-capf :with company-yasnippet)."

    ;; create a list of backend-to-merge with a count equal to company-backends
    ;; this is so mapcar* can iterate over both lists equally
    ;; ex. if we have (company-capf company-xcode),
    ;; then the list is (company-yasnippet company-yasnippet)
    (setq blist (make-list (cl-list-length company-backends) backend-to-merge))
    ;; b will be backend-to-merge
    ;; backend will be a backend from company-backends
    (setq company-backends (cl-mapcar (lambda (backend b)
                                        (if (and (listp backend) (member b backend))
                                            backend
                                          (append (if (consp backend)
                                                      backend
                                                    (list backend))
                                                  (if (and (listp backend)
                                                           (member :with backend))
                                                      `(,b)
                                                    `(:with ,b)))))
                                      company-backends blist)))

  (defun jojo/company-merge-backends ()
    "Merge common backends."
    (merge-backend-with-company-backends 'company-yasnippet)
    (merge-backend-with-company-backends 'company-dabbrev-code))
  (jojo/company-merge-backends)

  ;; if the completion is JoJo, typing jojo will get to it
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t) ; default is keep-prefix

  ;; Don't search other buffers.
  (setq company-dabbrev-other-buffers nil)
  (setq company-dabbrev-code-time-limit .000005)

  ;; use tab to cycle selection
  ;; https://github.com/company-mode/company-mode/issues/216
  ;; https://github.com/company-mode/company-mode/issues/246#issuecomment-68538735
  (setq company-auto-complete nil)
  (define-key company-active-map [backtab] 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map [S-tab] 'company-select-previous)
  (define-key company-active-map [S-iso-lefttab] 'company-select-previous)
  (define-key company-active-map [(shift tab)] 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "RET")
    'jojo/company-complete-selection-or-abort-if-same-unless-yas)
  (define-key company-active-map [return]
    'jojo/company-complete-selection-or-abort-if-same-unless-yas)
  ;; These get overwritten if `company-ac-setup' is called.
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

  (defun jojo/company-complete-selection-or-abort-if-same-unless-yas ()
    "Complete selection or abort if prefix matches selection.
If backend is yasnippet, complete normally."
    (interactive)
    (if (and
         ;; Completion is not from Yasnippet.
         (not (eq 'company-yasnippet
                  (get-text-property 0 'company-backend
                                     (nth company-selection company-candidates))))
         ;; Completion result is the same as the prefix.
         (string-equal company-prefix
                       (nth company-selection company-candidates)))
        (jojo/company-abort-and-newline)
      (company-complete-selection)))

  (defun jojo/company-abort-and-newline ()
    "Cancel the company selection and then go to next line."
    (interactive)
    (company-abort)
    (cond
     ((eq major-mode 'eshell-mode)
      (call-interactively 'eshell-send-input))
     (t
      (newline-and-indent))))

  (define-key company-active-map (kbd "<S-return>") 'jojo/company-abort-and-newline)

  ;; loop completion selections
  (setq company-selection-wrap-around t)
  (setq company-idle-delay .1)
  (company-ac-setup)

  (defun jojo/remove-dabbrev-on-semantic-c-completion (candidates)
    "Removes dabbrev on semantic c completion."
    (if (or
         (not (eq (length company-prefix) 0))
         (not (derived-mode-p 'c-mode 'csharp-mode)))
        candidates
      (let ((res '()))
        (dolist (candidate candidates)
          (let ((backend (get-text-property 0 'company-backend candidate)))
            (unless (or
                     (eq 'company-dabbrev backend)
                     (eq 'company-dabbrev-code backend))
              (push candidate res))))
        (reverse res))))

  (defun jojo/remove-dups-from-dabbrev (candidates)
    "Remove duplicate candidates with this company-transformer."
    (let ((hash (make-hash-table :test 'equal))
          (suffixes-to-trim (cond
                             ((eq 'objc-mode major-mode)
                              ;; "enabled" is the same as "enabled:".
                              '(":"))
                             ((eq 'csharp-mode major-mode)
                              ;; "Function()" is the same as "Function".
                              '("()"))
                             ((derived-mode-p 'c-mode)
                              ;; "Function()" is the same as "Function".
                              '("()"))
                             (t
                              ;; Return no suffixes to trim by default.
                              nil))))
      (dolist (candidate candidates)
        (let ((candidate-str
               (s-chop-suffixes suffixes-to-trim
                                (substring-no-properties candidate))))
          (if (not (gethash candidate-str hash))
              ;; Candidate is not yet a duplicate
              ;; since it's the first time we've seen it.
              (puthash candidate-str candidate hash)
            ;; Candidate is a duplicate.
            ;; Handle depending on which backend the current candidate is.
            (lexical-let* ((j-backend
                            (get-text-property
                             0 'company-backend candidate))
                           (j-hash-backend
                            (get-text-property
                             0 'company-backend
                             (gethash candidate-str hash))))
              (cond
               ((or (eq 'company-dabbrev j-backend)
                    (eq 'company-dabbrev-code j-backend))
                ;; dabbrev never replaces candidates.
                t)
               ((eq 'company-yasnippet j-backend)
                (when (or (eq 'company-dabbrev j-hash-backend)
                          (eq 'company-dabbrev-code j-hash-backend))
                  ;; yasnippet only replaces dabbrev candidates
                  (puthash candidate-str candidate hash)))
               (t
                ;; Candidates not from yasnippet or dabbrev have higher priority.
                (puthash candidate-str candidate hash)))))))
      ;; Hash table is now unsorted, so return candidates sorted.
      (sort (hash-table-values hash) 'string<)))

  (defun jojo/setup-company-transformers (&optional reset)
    "Push list of transformers to `company-transformers'.
If `reset', set `company-transformers' to nil."
    (if reset
        (setq company-transformers nil)
      (push #'company-sort-prefer-same-case-prefix company-transformers)
      (push #'company-sort-by-backend-importance company-transformers)
      (push #'jojo/remove-dups-from-dabbrev company-transformers)
      (push #'jojo/remove-dabbrev-on-semantic-c-completion company-transformers)))
  (jojo/setup-company-transformers)
  (global-company-mode))

;; documentation popup for company
(use-package company-quickhelp
  :ensure t
  :commands (company-quickhelp-mode)
  :init
  (defun jojo/company-quickhelp-hook ()
    "Setting up company-quickhelp."
    (company-quickhelp-mode 1))
  (add-hook 'company-mode-hook #'jojo/company-quickhelp-hook)
  :config
  (setq company-quickhelp-delay 2.3))

;; Replicate ac-dict from auto-complete.
(use-package company-dict
  :ensure t
  :commands company-dict
  :config
  (setq company-dict-dir
        (concat user-emacs-directory "tools/acdict/")))

;; Completion for use with terminal modes.
(use-package company-shell
  :ensure t
  :commands company-shell
  :init
  (dolist (hook '(eshell-mode-hook term-mode-hook))
    (add-hook hook (lambda ()
                     (jojo/company-push-backend 'company-shell)))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after company
  :init
  ;; https://github.com/joaotavora/yasnippet/issues/289
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1)))
  :config
  ;; When yasnippet gets an empty prefix, (can happen in a grouped backend)
  ;; don't trigger it if the prefix comes back with an empty string.
  (advice-add 'company-yasnippet :around #'jojo/company-yasnippet)

  (defun jojo/company-yasnippet (orig-fun &rest args)
    "`company-mode' backend for `yasnippet'."
    (interactive (list 'interactive))
    (cl-case (nth 0 args)
      (prefix
       (and (bound-and-true-p yas-minor-mode)
            (let ((company-symbol (company-grab-symbol)))
              (if (string-equal company-symbol "")
                  nil
                company-symbol))))
      (t
       (apply orig-fun args))))

  (add-to-list 'yas-snippet-dirs "~/.emacs.d/fork/yasnippet-snippets")

  ;; Yas messages stretches the status buffer when it starts up.
  (setq yas-verbosity 0)

  (defun jojo/yas-company-expand ()
    "If `company-mode' is active, try to use complete using `company-mode',
otherwise expand with `yasnippet'.

This generally works when `yasnippet' expansion is active and `company-mode' is active.
Sometimes `company-mode's keymap? overrides `yasnippet''s causing the next TAB to
not expand `yasnippet' anymore."
    (interactive)
    (if (or
         (company-tooltip-visible-p)
         company-prefix)
        (company-select-next-if-tooltip-visible-or-complete-selection)
      (yas-next-field-or-maybe-expand)))

  (define-key yas-keymap [(tab)] 'jojo/yas-company-expand)
  (define-key yas-keymap (kbd "TAB") 'jojo/yas-company-expand)

  (yas-global-mode 1))

(provide 'jojo-autocomplete)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-autocomplete.el ends here
