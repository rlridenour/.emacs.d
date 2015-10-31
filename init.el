;; User Information

(setq user-full-name "Randy Ridenour"
      user-mail-address "rlridenour@gmail.com")

;; Load Common Lisp

(require 'cl)

;; Turn on debugging

(setq debug-on-error t)
(setq debug-on-quit t)

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





















;; Turn off debugging and set default directory

(setq debug-on-error nil)
(setq debug-on-quit nil)
(setq default-directory "~/")
