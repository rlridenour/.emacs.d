;;;;;;;;;;;;
;; System ;;
;;;;;;;;;;;;

;; (use-package validate                   ; Validate options
;; :ensure t)

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

;; Use TeX to input special characters
(setq default-input-method 'TeX)
;; Start TeX input when switching buffers
;; (defadvice switch-to-buffer (after activate-input-method activate)
;; (activate-input-method "TeX"))

;; Save When Losing Focus

;; (defun save-all ()
;;   (interactive)
;;   (save-some-buffers t))
;; (add-hook 'focus-out-hook 'save-all)

;; auto save often
;; save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)

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

;; Use tab for indentation and completion. From http://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq-default tab-always-indent 'complete)

;; Backup files
;; Don't make backup files
;; (setq make-backup-files nil)
;; Save backups to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Spelling
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

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

;; find aspell and hunspell automatically
(cond
 ;; try hunspell at first
 ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          )))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

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
  (cl-flet ((process-list ())) ad-do-it))

;; ibuffer

;; Don't ask for unnecessary confirmations
(setq ibuffer-expert t)

;; Auto-update buffer list
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

;; Isearch
;; From Xah Lee, http://ergoemacs.org/emacs/emacs_isearch_by_arrow_keys.html
(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
  )

;;Shell
(setq multi-term-program "/usr/local/bin/fish")
(setq explicit-shell-file-name "/usr/local/bin/fish")

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

;; From http://oremacs.com/2015/01/01/three-ansi-term-tips/
(add-hook 'term-exec-hook 'oleh-term-exec-hook)
;; Make completion case-insensitive in eshell
(setq eshell-cmpl-ignore-case t)
(setq pcomplete-ignore-case t)

;; Kill contents of scratch buffer, not the buffer itself. From [[http://emacswiki.org/emacs/RecreateScratchBuffer][TN]].
(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)


(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))


;; Mark date and time that files were saved.
(add-hook 'before-save-hook 'time-stamp)

(setq delete-by-moving-to-trash t
	  trash-directory "~/.Trash/emacs")

;; Disable warning bell
;; (setq visible-bell t)
;; (setq visible-bell nil) ;; The default
;; (setq ring-bell-function 'ignore)

;; This flashes the mode-line (from http://www.stefanom.org/prettify-my-emacs-symbols/)
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
 
 (setq visible-bell nil
       ring-bell-function 'my-terminal-visible-bell)

;; Start eshell
(global-set-key (kbd "C-x m") (lambda () (interactive) (eshell t)))
;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell
(global-set-key (kbd "C-x M-m") 'shell)

;; CUA mode for rectangle editing
;; Sometimes very useful (but we don't use the core cua keys.)

  (setq cua-enable-cua-keys nil)
  (cua-mode)

;; To start a rectangle, use [C-return] and extend it using the normal
;; movement keys (up, down, left, right, home, end, C-home,
;; C-end). Once the rectangle has the desired size, you can cut or
;; copy it using C-w and M-w, and you can
;; subsequently insert it - as a rectangle - using C-y.  So
;; the only new command you need to know to work with cua-mode
;; rectangles is C-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;;
;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [C-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].
  




;; Turn off debugging and set default directory

;;(setq debug-on-error nil)
;;(setq debug-on-quit nil)

;; Convert tabs to spaces
;; (setq tab-width 4)
;; (setq-default indent-tabs-mode nil)

;; Garbage collection from http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; overwrite selected text
(delete-selection-mode t)

;; Do not use external GPG password entry
(setenv "GPG_AGENT_INFO" nil)

;; Start server
(unless (daemonp) (server-mode 1))
