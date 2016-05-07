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

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

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

;; === Helm ===
(load-file "~/.emacs.d/helm.el")


;; === Utilities ===
(load-file "~/.emacs.d/utilities.el")

;; === Functions ===
(load-file "~/.emacs.d/functions.el")
		   
;; === Markdown and Pandoc ====
(load-file "~/.emacs.d/markdown.el")


;; === Org-Mode ===
(load-file "~/.emacs.d/org.el")

;; === LaTeX ===
(load-file "~/.emacs.d/latex.el")

;; === Jekyll-Blog ===
(load-file "~/.emacs.d/blog.el")

;; === Eww ===
(load-file "~/.emacs.d/browser.el")

;; === Keybindings ===
(load-file "~/.emacs.d/keybindings.el")

;; === Evil ===
(load-file "~/.emacs.d/evil.el")


(setq default-directory "~/")
