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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Turn off splash screen and set major mode to org
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
)

;; Add Homebrew packages to path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Theme
(use-package hc-zenburn-theme
  :ensure t
  :config (load-theme 'hc-zenburn t)
  )

;; Set the default font. The "frame-alist" is necessary to set the font when using emacs daemon and client.
(set-default-font "Monaco-12") ;;; set default font
(setq default-frame-alist '((font . "Monaco-12"))) ;;; set default font for emacs --daemon / emacsclient

;; Change background color for selected text.
(set-face-attribute 'region nil :background "#666")

;; Turn on syntax highlighting for all buffers
(global-font-lock-mode t)


;; Highlight the current line.
(global-hl-line-mode 1)

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

;; Keep custom-set variables separate from configuration file.
(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set modifier keys in OS X
   
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper) 
   



;; Always prefer UTF-8


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; Save When Losing Focus

(defun save-all ()
  (interactive)
(save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)


;; Use "y" and "n":
(defalias 'yes-or-no-p 'y-or-n-p)



;; Confirm killing emacs on graphical sessions:
(when (window-system)
(setq confirm-kill-emacs 'yes-or-no-p))


;; Edit by Visual Lines
(global-visual-line-mode t)



;; Navigate visual lines:
(setq line-move-visual t)



;; Single space ends sentence:
(setq sentence-end-double-space nil)



;; Don't make backup files
(setq make-backup-files nil)


;; Spelling
(setq flyspell-issue-welcome-flag nil)


;; From [[https://joelkuiper.eu/spellcheck_emacs][Joel Kuiper]]

;; Enable flyspell mode for highlighting spelling errors.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

   
;; Check comments and strings when coding.
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))


;; Use F7 to check current word, M-F7 for next word.
(global-set-key (kbd "<f7>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f7>") 'flyspell-check-next-highlighted-word)


;; Spell-check with right mouse button.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; Use hunspell with US English dictionary.
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))


;; Store personal dictionary in Dropbox to sync between machines.
(setq ispell-personal-dictionary "/Users/rlridenour/Dropbox/emacs/ridenour-ispell-dictionary ")

  

;; Hide various file types, most LaTeX auxiliary files, in Dired.
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-omit-extensions '("fdb_latexmk" "aux" "bbl" "blg" "fls" "glo" "idx" "ilg" "ind" "ist" "log" "out" "gz" "DS_Store"))
(setq dired-dwim-target t)

;; Load Abbreviations
(load "~/Dropbox/emacs/my-emacs-abbrev")


;; Bookmarks
(require 'bookmark)
(bookmark-bmenu-list)


;; Recent Files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Don't ask for confirmation to kill processes when exiting Emacs. Credit to [[http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html][Timothy Pratley]].
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))
     

;;Shell
(setq multi-term-program "/usr/local/bin/zsh")

;; Kill contents of scratch buffer, not the buffer itself. From [[http://emacswiki.org/emacs/RecreateScratchBuffer][TN]].


(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)



;; Mark date and time that files were saved.
(add-hook 'before-save-hook 'time-stamp)

(use-package swiper
  :ensure t
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper)
   ("C-c C-r" . ivy-resume)))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("s-P" . smex))
  :config
  (smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package use-package-chords
  :ensure t)

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package expand-region
  :ensure t
  :commands (er/mark-symbol)
  :bind* ("C-=" . er/expand-region))

;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  )

;; == LaTex / AucTeX ==
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  
  ;; Don't use Helm for the reftex-citation lookup
;;  (eval-after-load 'helm-mode
;;    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
;;    )
  
  )















;; Turn off debugging and set default directory

(setq debug-on-error nil)
(setq debug-on-quit nil)
(setq default-directory "~/")
