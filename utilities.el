;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;


;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init (progn
		  ;; (bind-key "SPC" 'yas-expand yas-minor-mode-map)
          (yas-global-mode 1)
          (yas-reload-all)))
;; (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)

;; (use-package swiper
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind ("C-s" . swiper)
;;   ("C-c C-s" . isearch-forward)
;;   ("s-r" . ivy-recentf)
;;   ("C-c C-r" . ivy-resume)
;;   :config
;;   (setq ivy-display-style 'fancy
;;         ivy-use-virtual-buffers t
;;         ivy-re-builders-alist)
;;   (use-package smex
;;    :ensure t
;;    :config (smex-initialize))
;;   (use-package hydra
;; 	:ensure t)
;;   (ivy-mode))

;; (use-package counsel
;;   :ensure t
;;   :bind (("C-x C-f" . counsel-find-file)
;; 		 ;; ("M-x" . counsel-M-x)
;; 		 ("s-o" . counsel-find-file))
;;   :config
;;   (setq counsel-find-file-at-point t))

;; avy
(use-package avy
  :ensure t
  :bind (("M-g l" . avy-goto-line)
		 ;; ("s-l" . avy-goto-line)
		 ("M-g w" . avy-goto-word-1)
		 ("M-g M-g" . avy-goto-char-2)
		 ("s-/" . avy-goto-char-timer)))

(use-package  ace-window
  :ensure
  :bind ("s-w" . ace-window)
  :config
  ;; (setq aw-leading-char-style 'path)
  (setq aw-background nil)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("s-P" . smex))
  :config
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package expand-region
  :ensure t
  :commands (er/mark-symbol)
  :bind* ("C-=" . er/expand-region))

;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package evil-nerd-commenter
  :ensure t
  :config (evilnc-default-hotkeys))

(use-package shrink-whitespace
  :ensure t
  :bind ("M-=" . shrink-whitespace))

(use-package easy-kill
  :ensure t
  :bind ([remap kill-ring-save] . easy-kill))

(use-package zop-to-char
  :ensure t
  :bind ([remap zap-to-char] . zop-to-char))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))

(use-package ag
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (setq company-tooltip-limit 20)
    (global-company-mode 1)))

;; (use-package empos
;;   :ensure t
;;   :config
;;   (progn
;; 	(setq empos-available-engines '("arxiv" "crossref")
;; 		  empos-bib-file "/Users/rlridenour/Dropbox/bibtex/empos.bib")))

(use-package reveal-in-osx-finder
  :ensure t
  :bind ("C-c z" . reveal-in-osx-finder))

(use-package smartparens
  :ensure t)
(require 'smartparens-config)
(smartparens-global-mode t)

(use-package olivetti
  :ensure t)

;; (use-package neotree
;;   :ensure t
;;   :bind ("C-c n" . neotree-toggle)
;;   :config (setq neo-smart-open t))

(use-package aggressive-indent
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'markdown-mode))

;; From http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; (use-package dired-ranger
;;   :ensure t
;;   :bind (:map dired-mode-map
;;               ("W" . dired-ranger-copy)
;;               ("X" . dired-ranger-move)
;;               ("Y" . dired-ranger-paste)))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode 1)
  (unless window-system
	(setq nlinum-format "%d ")))

(use-package biblio
  :ensure t)

;; ;; volatile highlights - temporarily highlight changes from pasting etc
;; (use-package volatile-highlights
;;   :ensure t
;;   :config
;;   (volatile-highlights-mode t))


;; Use flyspell-correct-ivy
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("s-;" . flyspell-correct-word-generic)
			  ("C-;" . flyspell-correct-previous-word-generic)))

;; Word count with wc-mode

(use-package wc-mode
  :ensure t)

;; Cdlatex makes inserting LaTeX easier.

(use-package cdlatex
  :ensure t)

;; Move-text - move current line or active region up or down with m-arrow key

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

										;flashes cursor when scrolling
;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1)
;;   ;; (setq beacon-color "#666600")
;;   )

										; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; Web-mode (from http://cestlaz.github.io)
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
		'(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
		'(("css" . (ac-source-css-property))
		  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)) ; this fixes the quote problem I mentioned

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
