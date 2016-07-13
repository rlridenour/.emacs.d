;; Turn off splash screen and set major mode to org
(setq inhibit-splash-screen t
      initial-scratch-message nil
      ;; initial-major-mode 'org-mode
	  )

;; Theme
(use-package hc-zenburn-theme
  :ensure t
  :config (load-theme 'hc-zenburn t)
  )

;; Set the default font. The "frame-alist" is necessary to set the font when using emacs daemon and client.
;; (if (eq system-type 'darwin)
;; (set-default-font "Monaco-12") ;;; set default font
(set-default-font "DejaVu Sans Mono-12")
(setq default-frame-alist '((font . "DejaVu Sans Mono-12"))) ;;; set default font for emacs --daemon / emacsclient


;; Change background color for selected text.
(set-face-attribute 'region nil :background "#666")

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)


;; Highlight the current line.
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray17")

;; Match parentheses — especially useful when editing Lisp
(show-paren-mode 1)

;; Turn off the menu, tool bar, and scroll bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)

;; Show line and column numbers in mode line:
(line-number-mode 1)
(column-number-mode 1)
(winner-mode 1)

;; Hide mouse when typing:
(setq make-pointer-invisible t)

;; Set tab to four spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8

;; Show file path in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(use-package spaceline
  :ensure t
  :config
  (progn
	(require 'spaceline-config)
	(spaceline-spacemacs-theme)
	(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)))
