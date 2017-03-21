;;;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :mode
  ("\\.m\\'" . objc-mode)
  ("\\.mm\\'" . objc-mode)
  ("\\.xctool.args\\'" . objc-mode))

(use-package xcode-mode
  ;; https://github.com/phonegap/ios-sim
  ;; https://github.com/facebook/xctool
  :commands (xcode-mode)
  :load-path "~/.emacs.d/fork/xcode-mode/"
  :ensure nil
  :init
  (add-hook 'objc-mode-hook #'xcode-mode)
  (add-hook 'swift-mode-hook #'xcode-mode)
  :config
  (setq xcode-completing-read-function 'grizzl-read)
  (defhydra hydra-xcode-mode (:color teal :hint nil)
    "
   Xcode: %(xcode-project-directory)

   Build                     Run                     Open
------------------------------------------------------------------------------------
  _ba_: Archive             _pi_: Run Pod Install   _op_: Open Project
  _bb_: Build               _rr_: Run               _os_: Open Storyboard
  _br_: Build and Run       _rt_: Run Tests         _ow_: Open Workspace
  _bt_: Builds Tests        _tt_: Run Test
  _bT_: Builds Tests Only   _R_: RTags
  _cc_: Clean

  "
    ("ba" xcode-xctool-archive)
    ("bb" xcode-xctool-build)
    ("br" xcode-xctool-build-and-run)
    ("bt" xcode-xctool-build-tests)
    ("bT" xcode-xctool-build-tests-only)
    ("cc" xcode-xctool-clean)
    ("dd" xcode-delete-derived-data "Delete Derived Data")
    ("op" xcode-open-storyboard)
    ("ow" xcode-open-workspace)
    ("os" xcode-open-project)
    ("pi" xcode-pod-install)
    ("rr" xcode-xctool-run)
    ("rt" xcode-xctool-run-tests)
    ("tt" xcode-xctool-test)
    ("R" hydra-rtags-mode/body)
    ("=" clang-format-region-or-buffer) ;; TODO Make more generic
    ("q" nil "Cancel"))

  (jojo/add-hydra 'mode 'xcode-mode))

;;;###autoload
(defun jojo/objc-bootstrap ()
  "Bootstrap `jojo-objc'."
  (setq auto-mode-alist (rassq-delete-all #'jojo/objc-bootstrap auto-mode-alist))
  (objc-mode))

(provide 'jojo-objc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; jojo-objc.el ends here
