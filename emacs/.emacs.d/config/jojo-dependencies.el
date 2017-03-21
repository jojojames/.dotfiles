;;;; -*- lexical-binding: t; -*-

(use-package dash :ensure t)
(use-package s :ensure t)

(use-package alert :ensure t
  :commands (alert)
  :config
  ;; https://github.com/jwiegley/alert/issues/38
  (defun alert-osx-notifier-notify (info)
    (apply #'call-process "osascript" nil nil nil "-e"
           (list (format "display notification %S with title %S"
                         (alert-encode-string (plist-get info :message))
                         (alert-encode-string (plist-get info :title)))))
    (alert-message-notify info))
  (when (jojo/osx-p)
    (setq alert-default-style 'osx-notifier))
  (when (jojo/windows-p)
    (let ((notifications
           `((alert
              :display "IRC Mention"
              :enabled t))))
      (gntp-register notifications gntp-server))
    (setq alert-default-style 'gntp)))

(use-package dumb-jump
  :ensure t
  :commands (dumb-jump-go dumb-jump-back)
  :config
  (eval-after-load 'ivy
    (lambda ()
      (setq dumb-jump-selector 'ivy))))

(use-package hydra
  :ensure t
  :config
  (setq lv-use-separator t)
  (defmacro jojo/defhydra (names body &optional docstring &rest heads)
    (cons 'progn
          (mapcar (lambda (name)
                    `(defhydra ,name ,body ,docstring ,@heads))
                  names)))

  (setq type-minor-map (make-hash-table :test 'equal))
  (setq type-major-map (make-hash-table :test 'equal))

  (defun jojo/inner-add-hydra (type mode function)
    "`type' can be
`mode', `eval', `test', `debug', `compile', `help' or `indent'
`mode' can be major or minor mode,
`function' is function to call."
    (interactive)
    (let* ((type-map (if (member mode minor-mode-list) 'type-minor-map 'type-major-map)))
      (let ((hash (or (gethash type (symbol-value type-map))
                      (puthash type (make-hash-table :test 'equal) (symbol-value type-map)))))
        (puthash mode function hash))))

  (defun jojo/add-hydra (jtypes jmodes &optional func)
    "Add leaders through a uniform naming scheme.
`jmodes' is a mode `slime-mode' or part of a mode list.
`types' is a type `mode', `eval', `compile' or part of a type list.
`base' is used to determine hydra name that will be bound to.
If `func' is non nil, base will be ignored."
    (interactive)
    (let ((types-list (if (listp jtypes) jtypes (list jtypes)))
          (modes-list (if (listp jmodes) jmodes (list jmodes))))
      (dolist (mode modes-list)
        (let ((base
               ;; "slime-mode" -> "slime"
               ;; "lisp-interaction-mode" -> "lisp-interaction"
               (mapconcat (lambda (str) str)
                          (butlast (s-split "-" (symbol-name mode)))
                          "-")))
          (dolist (type types-list)
            (jojo/inner-add-hydra
             type
             mode
             ;; "slime" -> "hydra-slime-mode/body"
             (if func
                 func
               (intern (concat "hydra"
                               "-"
                               base
                               "-"
                               (symbol-name type)
                               "/body")))))))))

  (defun jojo/remove-hydra (type mode)
    "Symmetrical to jojo/add-hydra but drops the key-value pair."
    (interactive)
    (letf* ((type-map (if (member mode minor-mode-list)
                          'type-minor-map
                        'type-major-map)))
      (when-let (hash (gethash type (symbol-value type-map)))
        (remhash mode hash))))

  (defun jojo/run-hydra (type)
    "Run specific leader function based on `type'.
type can be
`mode', `eval', `test', `debug', `compile', `help' or `indent'.
Run minor mode function if available."
    (interactive)
    (let ((func))
      (when (and (> (hash-table-count type-minor-map) 0)
                 (gethash type type-minor-map))
        (let ((hashmap (gethash type type-minor-map)))
          (maphash (lambda (key value)
                     (when (and (boundp key) (symbol-value key))
                       (setq func value))) hashmap)))

      (unless func
        (when-let (hashmap (gethash type type-major-map))
          (setq func (or (gethash major-mode hashmap)
                         (gethash (get major-mode 'derived-mode-parent) hashmap)
                         (gethash 'fundamental-mode hashmap)))))
      (call-interactively func))))

(provide 'jojo-dependencies)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-dependencies.el ends here
