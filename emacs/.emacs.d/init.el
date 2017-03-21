;;;; -*- lexical-binding: t; -*-

;; (package-initialize)

(setq gc-cons-threshold 100000000) ; 100 mb
(add-hook 'focus-out-hook 'garbage-collect)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; M-x list-packages U x to upgrade packages.
(setq package-list '(diminish))

;; Disable in favor of `use-package'.
(setq package-enable-at-startup nil)

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Activate all packages (in particular autoloads).
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; for :diminish
(require 'bind-key) ; for :bind

;; Install package if not existing.
(setq use-package-always-ensure nil)

;; Check loading times with `use-package'.
(setq use-package-verbose t)

;; Fetch the list of packages when unavailable.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install any missing packages.
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(use-package jojo-funcs :ensure nil)
(use-package jojo-dependencies :ensure nil)
(use-package jojo-theme :ensure nil)
(use-package jojo-defaults :ensure nil)
(use-package jojo-platform :ensure nil)

(use-package jojo-git :ensure nil
  :commands (magit-toplevel magit-status magit-blame magit-log hydra-magit/body))

(use-package jojo-misc :ensure nil)
(use-package jojo-pair-editing :ensure nil)
(use-package jojo-project :ensure nil)
(use-package jojo-evil :ensure nil)
(use-package jojo-autocomplete :ensure nil)
(use-package jojo-tags :ensure nil)

(use-package jojo-java :ensure nil
  :mode ("\\.java\\'" . jojo/java-bootstrap))

(use-package jojo-csharp :ensure nil
  :mode ("\\.cs\\'". jojo/csharp-bootstrap))

;; (use-package jojo-xamarin :ensure nil)

(use-package jojo-rust :ensure nil
  :mode ("\\.rs\\'" . jojo/rust-bootstrap))

(use-package jojo-swift :ensure nil
  :mode ("\\.swift\\'" . jojo/swift-bootstrap))

(use-package jojo-ruby :ensure nil
  :mode
  ("\\.rb$\\'" . jojo/ruby-bootstrap)
  ("\\Rakefile$\\'" . jojo/ruby-bootstrap)
  ("\\.gemspec$\\'" . jojo/ruby-bootstrap)
  ("\\.ru$\\'" . jojo/ruby-bootstrap)
  ("\\Gemfile$\\'" . jojo/ruby-bootstrap)
  ("\\.rake$\\'" . jojo/ruby-bootstrap)
  :interpreter ("ruby" . jojo/ruby-bootstrap))

(use-package jojo-python :ensure nil
  :mode ("\\.py\\'" . jojo/python-bootstrap)
  :interpreter ("python" . jojo/python-bootstrap))

(use-package jojo-lua :ensure nil
  :mode ("\\.lua\\'" . jojo/lua-bootstrap)
  :interpreter ("lua" . jojo/lua-bootstrap))

(use-package jojo-commonlisp :ensure nil
  :mode
  ("\\.lisp\\'" . jojo/commonlisp-bootstrap)
  ("\\.stumpwmrc\\'" . jojo/commonlisp-bootstrap)
  ("\\.stumpish\\'" . jojo/commonlisp-bootstrap)
  ("\\.sbclrc\\'" . jojo/commonlisp-bootstrap))

(use-package jojo-scheme :ensure nil
  :init
  (remove-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)
  :mode
  ("\\.oak\\'" . jojo/scheme-bootstrap)
  ("\\.scm\\.[0-9]*\\'" . jojo/scheme-bootstrap)
  ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . jojo/scheme-bootstrap)
  ("\\.rkt\\'" . jojo/scheme-bootstrap)
  ("\\.scm\\'" . jojo/scheme-bootstrap)
  :interpreter
  ("guile" . jojo/scheme-bootstrap)
  ("scm" . jojo/scheme-bootstrap))

(use-package jojo-elisp :ensure nil)

(use-package jojo-clojure :ensure nil
  :mode
  ("\\.clj\\'" . jojo/clojure-bootstrap)
  ("\\.edn\\'" . jojo/clojure-bootstrap))

(use-package jojo-erlang :ensure nil
  :mode
  ("\\.erl\\'" . jojo/erlang-bootstrap)
  ("\\.hrl\\'" . jojo/erlang-bootstrap)
  ("\\.xrl\\'" . jojo/erlang-bootstrap))

(use-package jojo-elixir :ensure nil
  :mode
  ("\\.elixir\\'" . jojo/elixir-bootstrap)
  ("\\.ex\\'" . jojo/elixir-bootstrap)
  ("\\.exs\\'" . jojo/elixir-bootstrap))

(use-package jojo-haskell :ensure nil
  :mode ("\\.hs\\'" . jojo/haskell-bootstrap))

(use-package jojo-groovy :ensure nil
  :mode
  ("\\.gradle\\'" . jojo/groovy-bootstrap)
  ("\\.groovy\\'" . jojo/groovy-bootstrap))

(use-package jojo-web :ensure nil
  :mode
  ("\\.phtml\\'" . jojo/web-bootstrap)
  ("\\.tpl\\.php\\'" . jojo/web-bootstrap)
  ("\\.blade\\.php\\'" . jojo/web-bootstrap)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . jojo/web-bootstrap)
  ("\\.[agj]sp\\'" . jojo/web-bootstrap)
  ("\\.as[cp]x\\'" . jojo/web-bootstrap)
  ("\\.erb\\'" . jojo/web-bootstrap)
  ("\\.mustache\\'" . jojo/web-bootstrap)
  ("\\.djhtml\\'" . jojo/web-bootstrap)
  ("\\.jsp\\'" . jojo/web-bootstrap)
  ("\\.eex\\'" . jojo/web-bootstrap))

(use-package jojo-javascript :ensure nil
  :mode ("\\.js\\'" . jojo/javascript-bootstrap))

(use-package jojo-php :ensure nil
  :mode ("\\.php\\'" . jojo/php-bootstrap))

(use-package jojo-extra-lang :ensure nil)
(use-package jojo-c :ensure nil)

(use-package jojo-objc :ensure nil
  :mode
  ("\\.m\\'" . jojo/objc-bootstrap)
  ("\\.mm\\'" . jojo/objc-bootstrap)
  ("\\.xctool.args\\'" . jojo/objc-bootstrap))

(use-package jojo-org :ensure nil
  :mode ("\\.org\\'" . jojo/org-bootstrap))

(use-package jojo-security :ensure nil :defer 3)
(use-package jojo-mail :ensure nil :commands (jojo/mu4e))
(use-package jojo-irc :ensure nil :commands (jojo/erc))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
