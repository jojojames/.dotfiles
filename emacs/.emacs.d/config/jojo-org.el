;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (evil-define-key 'normal org-mode-map
    (kbd "g.") 'org-open-at-point
    (kbd "TAB") 'org-cycle
    ;; to be backward compatible with older org version
    (kbd "]") (if (fboundp 'org-forward-same-level)
                  'org-forward-same-level
                'org-forward-heading-same-level)
    (kbd "[") (if (fboundp 'org-backward-same-level)
                  'org-backward-same-level
                'org-backward-heading-same-level))

  (evil-define-key 'emacs org-agenda-mode-map
    (kbd "b") 'evil-backward-word-begin
    (kbd "w") 'evil-forward-word-begin
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char)
  (jojo/set-evil-shift-width 4)

  (defun jojo/customize-org-ui ()
    "Customize various Org Mode UI Elements"
    (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
    (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.3 :weight 'bold)
    (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.2 :weight 'bold)
    (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.1)
    (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.0))
  (jojo/customize-org-ui)

  (when (jojo/osx-p)
    (setq org-directory "~/Dropbox/Notes")
    (setq org-agenda-files '("~/Dropbox/Notes")))
  (when (jojo/windows-p)
    (setq org-directory "C:/Users/james/Dropbox/Notes")
    (setq org-agenda-files '("C:/Users/james/Dropbox/Notes")))

  (setq org-capture-templates
        '(;; standard todo
          ("t" "Todo" entry
           (file+headline (concat org-directory "/mine.org") "Tasks")
           "* TODO %u %a %?\n")
          ;; handle this message in the next two days
          ("H" "High Priority" entry
           (file+headline (concat org-directory "/mine.org") "High Priority")
           "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
          ;; wait for an e-mail reply
          ("W" "Wait for Reply"
           entry (file+headline (concat org-directory "/mine.org") "Waiting")
           "* WAIT %u %a %?\n")))

  (defhydra hydra-org-hyperlink (:color blue :columns 3)
    "Links"
    ("l" org-store-link "Store Link")
    ("s" org-store-link "Store Link")
    ("r" org-occur-link-in-agenda-files "Occur Links")
    ("i" org-insert-link "Insert Link")
    ("g" org-open-at-point "Follow Link")
    ("n" org-next-link "Next Link")
    ("p" org-previous-link "Previous Link")
    ("t" org-toggle-link-display "Toggle Link Display")
    ("d" org-toggle-link-display "Toggle Link Display"))

  (defhydra hydra-org-time (:color blue :columns 3)
    "Time"
    ("t" org-time-stamp "Timestamp")
    ("T" org-time-stamp-inactive "Inactive Timestamp")
    ("D" hydra-org-change-date/body "Change Date")
    ("y" org-evaluate-time-range "Evaluate Time Range")
    ("s" org-schedule "Schedule Item")
    ("d" org-deadline "Deadline")
    ("Z" org-toggle-time-stamp-overlays "Custom Time Format")
    ("c" org-goto-calendar "Goto Calendar")
    ("C" org-date-from-calendar "Date from Calendar")
    ("0" org-timer-start "Start Timer")
    ("9" org-timer-pause-or-continue "Pause/Continue Timer")
    ("8" org-timer-pause-or-continue "Pause/Continue Timer")
    ("7" org-timer "Insert Timer String")
    ("6" org-timer-item "Insert Timer Item"))

  (defhydra hydra-org-change-date (:color blue :columns 3)
    "Change Date"
    ("l" org-shiftright "1 Day Later")
    ("h" org-shiftleft "1 Day Before")
    ("k" org-shiftup "1 ... Later")
    ("j" org-shiftdown "1 ... Before"))

  (defhydra hydra-org-mode (:color blue :columns 3)
    "Org"
    ("t" hydra-org-time/body "Time and Scheduling")
    ("h" hydra-org-hyperlink/body "Hyperlinks"))
  (jojo/add-hydra '(mode) 'org-mode)

  (setq org-src-fontify-natively t)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-goto-interface 'outline-path-completion
        org-goto-max-level 10))

;;;###autoload
(defun jojo/org-bootstrap ()
  "Bootstrap `jojo-org'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/org-bootstrap auto-mode-alist))
  (org-mode))

(provide 'jojo-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-org.el ends here
