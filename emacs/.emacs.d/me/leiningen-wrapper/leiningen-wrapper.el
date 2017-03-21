;;; leiningen-wrapper.el --- Set of wrappers over leiningen.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 James Nguyen <ja.nguyen@gmail.com>

;; Author: James Nguyen <ja.nguyen@gmail.com>
;; Keywords: convenience, files, frames, mouse
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interact with leiningen from Emacs.

;;; Code:

(defun lein-project-dir ()
  "Get project directory."
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (or (locate-dominating-file default-directory ".lein-env")
        default-directory)))

(defmacro lein-in-root (body)
  "Execute BODY form with `project-directory' as
``default-directory''."
  `(let ((default-directory (lein-project-dir)))
     ,body))

(defun lein-run-command (command)
  "Run lein command in project root."
  (interactive)
  (lein-in-root
   (compile (format "lein %s" command))))

;;;###autoload
(defun lein-run ()
  "$ lein run"
  (interactive)
  (lein-run-command "run"))

;;;###autoload
(defun lein-migratus-migrate ()
  "$ lein migratus migrate"
  (interactive)
  (lein-run-command "migratus migrate"))

;;;###autoload
(defun lein-migratus-rollback ()
  "$ lein migratus rollback"
  (interactive)
  (lein-run-command "migratus rollback"))

;;;###autoload
(defun lein-migratus-down ()
  "$ lein migratus down"
  (interactive)
  (lein-run-command "migratus down"))

;;;###autoload
(defun lein-migratus-up ()
  "$ lein migratus down"
  (interactive)
  (lein-run-command "migratus up"))

;;;###autoload
(defun lein-migratus-reset ()
  "$ lein migratus reset"
  (interactive)
  (lein-run-command "migratus reset"))

;;;###autoload
(defun lein-migratus-create (script-name)
  "$ lein migratus create table-name"
  (interactive "sEnter new script name: ")
  (lein-run-command (format "migratus create %s" script-name)))

(provide 'leiningen-wrapper)
;;; leiningen-wrapper.el ends here
