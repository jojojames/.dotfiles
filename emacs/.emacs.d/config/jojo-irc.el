;;;; -*- lexical-binding: t; -*-
(require 'jojo-funcs)

;;;###autoload
(defun jojo/erc ()
  "Start erc."
  (interactive)
  (load-library "~/.emacs.d/config/jojo-erc.el.gpg"))

(provide 'jojo-irc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-irc.el ends here
