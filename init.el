;; User Information

(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

;; Load Common Lisp

(require 'cl)

;; Turn on debugging

;(setq debug-on-error t)
;(setq debug-on-quit t)

;; Load Use-Package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Add Homebrew packages to path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; === Appearance ===
(load-file "~/.emacs.d/appearance.el")

;; === System ===
(load-file "~/.emacs.d/system.el")

;; === Utilities ===
(load-file "~/.emacs.d/utilities.el")

;; === Markdown and Pandoc ====
(load-file "~/.emacs.d/markdown.el")

;; === Jekyll-Blog ===
(load-file "~/.emacs.d/blog.el")
