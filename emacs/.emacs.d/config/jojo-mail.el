;;;; -*- lexical-binding: t; -*-

(require 'jojo-funcs)

;; Sending Mail

;; Use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")

;; Choose the SMTP server according to the from field in the outgoing email.
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;; Add the source shipped with mu to load-path.

(when (jojo/osx-p)
  (add-to-list 'load-path
               (concat
                (replace-regexp-in-string
                 "\n" ""
                 (shell-command-to-string "echo $(brew --prefix mu)"))
                "/share/emacs/site-lisp/mu/mu4e")))

;; Receiving Mail

(use-package mu4e
  :ensure nil
  :diminish overwrite-mode
  :commands (mu4e mu4e-view-message-with-msgid)
  :config
  (add-to-list 'auth-sources "~/.emacs.d/mail/.email.gpg")
  ;; synergy with org mode capture
  (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
  (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

  ;; additional actions -> a key
  (add-to-list 'mu4e-view-actions
               '("browser" . mu4e-action-view-in-browser) t)

  ;; (add-to-list 'mu4e-view-actions
  ;;              '("xwidget" . mu4e-action-view-with-xwidget) t)

  ;; adding cc header
  (add-hook 'mu4e-compose-mode-hook
            (defun jojo-add-cc ()
              "Add a cc: header."
              (save-excursion (message-add-header "Cc: \n"))))

  (setq mu4e-cache-maildir-list t)
  (setq mu4e-completing-read-function 'ivy-completing-read)
  (setq mu4e-maildir (expand-file-name "~/Mail"))
  (setq mu4e-get-mail-command "mbsync fastmail")
  (setq mu4e-headers-show-threads nil)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-update-interval 30)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mu4e-view-show-images t)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-index-update-error-warning nil)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-index-cleanup nil)
  (setq mu4e-index-lazy-check t)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-format-flowed t)

  ;; Taken from mu4e page to define bookmarks.
  (add-to-list 'mu4e-bookmarks '("flag:attach" "Messages with attachment" ?a) t)
  (add-to-list 'mu4e-bookmarks '("size:5M..500M" "Big messages" ?b) t)
  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged messages" ?f) t)

  ;; mu4e requires to specify drafts, sent, and trash dirs
  (setq mu4e-drafts-folder "/mu4e/drafts")
  (setq mu4e-sent-folder "/mu4e/sent")
  (setq mu4e-trash-folder "/mu4e/trash")

  ;; Use imagemagick, if available.
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; setting up contexts between personal and work
  (setq mu4e-contexts
        `(
          ;; Fastmail
          ,(make-mu4e-context
            :name "Fastmail"
            :enter-func (lambda ()
                          (mu4e-message "Switched to the Fastmail context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "james@jojojames.com")))
            :vars '((user-mail-address . "james@jojojames.com")
                    (user-full-name . "James Nguyen")
                    (mu4e-compose-signature .
                                            (concat
                                             "James Nguyen\n"))))

          ;; Gmail
          ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda ()
                          (mu4e-message "Switched to the Gmail context"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "ja.nguyen@gmail.com")))
            :vars '((user-mail-address . "ja.nguyen@gmail.com")
                    (user-full-name . "James Nguyen")
                    (mu4e-compose-signature .
                                            (concat
                                             "James Nguyen\n"))))

          ;; WhosHere
          ,(make-mu4e-context
            :name "WhosHere"
            :enter-func (lambda ()
                          (mu4e-message "Switched to the WhosHere context"))
            ;; leave-fun not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "james@whoshere.net")))
            :vars '((user-mail-address . "james@whoshere.net")
                    (user-full-name . "James Nguyen")
                    (mu4e-compose-signature .
                                            (concat
                                             "James Nguyen\n"))))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask-if-none))

(use-package org-mu4e
  :after mu4e
  :ensure nil)

(use-package evil-mu4e
  :ensure t
  :after mu4e
  :config
  (evil-define-key 'motion mu4e-view-mode-map
    (kbd "q") 'mu4e~view-quit-buffer
    (kbd "g.") 'mu4e~view-browse-url-from-binding
    (kbd "C-n") 'mu4e-view-headers-next
    (kbd "C-p") 'mu4e-view-headers-prev)

  (evil-define-key 'motion mu4e-headers-mode-map
    (kbd "q") 'mu4e~headers-quit-buffer
    (kbd "+") 'mu4e-headers-mark-for-flag)

  (defun jojo/mu4e-update-all-mail ()
    "Download mail from all accounts."
    (interactive)
    (let ((mu4e-get-mail-command "mbsync -a"))
      (mu4e-update-mail-and-index t)))

  (evil-define-key 'motion mu4e-main-mode-map
    (kbd "gR") 'jojo/mu4e-update-all-mail
    (kbd "gM") 'mu4e~headers-jump-to-maildir
    (kbd "C-j") nil
    (kbd "C-k") nil
    (kbd "C-n") 'mu4e-headers-next
    (kbd "C-p") 'mu4e-headers-prev))

(use-package mu4e-maildirs-extension
  :ensure t
  :after mu4e
  :config
  ;; Don't show mu4e maildir in the list of maildirs.
  (setq mu4e-maildirs-extension-ignored-regex "mu4e")

  (setq mu4e-maildirs-extension-parallel-processes 8)
  (setq mu4e-maildirs-extension-toggle-maildir-key (kbd "TAB"))

  (defun jojo/mu4e-maildirs-extension-always-update ()
    "Update index when returning into main mode."
    (mu4e-maildirs-extension-force-update '(16)))
  (add-hook 'mu4e-main-mode-hook 'jojo/mu4e-maildirs-extension-always-update)

  (mu4e-maildirs-extension))

;;;###autoload
(defun jojo/mu4e ()
  "Wrap `mu4e'."
  (interactive)
  (mu4e))

(provide 'jojo-mail)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-mail.el ends here
