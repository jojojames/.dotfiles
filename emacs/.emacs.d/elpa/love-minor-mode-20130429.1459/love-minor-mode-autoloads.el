;;; love-minor-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "love-minor-mode" "love-minor-mode.el" (22697
;;;;;;  11668 0 0))
;;; Generated autoloads from love-minor-mode.el

(autoload 'love-minor-mode "love-minor-mode" "\
Toggles LÖVE minor mode.

\\{love-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'love/possibly-enable-mode "love-minor-mode" "\
This function determines whether or not to automatically
enable `love-minor-mode'.  If the current buffer contains any
LÖVE-specific functions then we enable the minor mode.

\(fn)" nil nil)

(add-hook 'lua-mode-hook 'love/possibly-enable-mode)

(autoload 'love/create-project-configuration "love-minor-mode" "\
This function creates a `conf.lua' file in a given directory.
It automatically fills the file with the love.conf() function and
sets the name and identity of the game.

\(fn DIRECTORY NAME IDENTITY)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; love-minor-mode-autoloads.el ends here
