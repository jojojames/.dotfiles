;;;; -*- lexical-binding: t; -*-
(require 'jojo-funcs)

(defun gpg-agent-restart ()
  "This kills and restarts the gpg-agent.

To kill gpg-agent, we use killall. If you know that the agent is
OK, you should just reload the environment file using
`gpg-agent-reload-info'."
  (interactive)
  (shell-command "killall gpg-agent")
  (shell-command "gpg-agent --daemon --enable-ssh-support --write-env-file")
  ;; read the environment file instead of parsing the output
  (gpg-agent-reload-info))

(defun gpg-agent-reload-info ()
  "Reload the ~/.gpg-agent-info file."
  (interactive)
  (let ((gpg-file-name (expand-file-name "~/.gpg-agent-info")))
    (if (not (file-exists-p gpg-file-name))
        (message (concat gpg-file-name " was not found."))
      (with-temp-buffer
        (insert-file-contents gpg-file-name)
        (goto-char (point-min))
        (while (re-search-forward "\\([A-Z_]+\\)=\\(.*\\)" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(defun gpg-agent-startup ()
  "Initialize the gpg-agent if necessary.

Note that sometimes the gpg-agent can be up and running and still
be useless, in which case you should restart it using
`gpg-agent-restart'."
  (gpg-agent-reload-info)
  (when-let (ssh-agent-pid (getenv "SSH_AGENT_PID"))
    (unless (member (string-to-number ssh-agent-pid)
                    (list-system-processes))
      (gpg-agent-restart))))

(defun jojo/gpg-init ()
  "Initialize gpg-agent in Emacs."
  (if-let (gpg-agent-info (getenv "GPG_AGENT_INFO"))
      (if (string-match-p gpg-agent-info "com.apple.launchd")
          ;; This is the case where Macports updates gpg
          ;; in /Library/LaunchAgents/ and/or ~/Library/LaunchAgents/.
          ;; This branch of code can be avoided if we remove that startup file.
          (message "GPG started from a LaungeAgent, restarting.")
        (gpg-agent-restart)
        ;; Try and get the latest gpg environment variables.
        ;; This might not be necessary.
        (gpg-agent-reload-info))
    ;; No info, start gpg-agent manually.
    (gpg-agent-startup)))

(when (jojo/osx-p)
  (jojo/gpg-init))

(provide 'jojo-security)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-security.el ends here
