;; I used Vim before Textmate, before Sublime Text, before Emacs... I find the Vim commands much easier to remember, but for various reasons, my mind doesn't work well with modal editing. Anyway, I find editing with Vim very useful under certain circumstances, and Evil-mode is a great way to do that. 

(use-package evil
  :ensure t
  :init
  (progn
	(use-package evil-leader
	  :ensure t
	  :defer t
	  :init (global-evil-leader-mode)
	  :config
	  (progn
		(evil-leader/set-leader "<SPC>")
		(evil-leader/set-key
		  "f" 'swiper-helm
		  "k" 'kill-this-buffer
		  "o" 'counsel-find-file
		  "p" 'hydra-markdown/body
		  "r" 'helm-mini
		  "t" 'hydra-toggle/body
		  "w" 'save-buffer
		  "x" 'helm-M-x
		  )))
	;; boot evil by default
	(evil-mode 1))
  :config
  (progn
	;; This is to make the escape key work (almost) like it does in Vim.

	;; esc quits
	(defun minibuffer-keyboard-quit ()
	  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
	  (interactive)
	  (if (and delete-selection-mode transient-mark-mode mark-active)
		  (setq deactivate-mark  t)
		(when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
		(abort-recursive-edit)))
	(define-key evil-normal-state-map [escape] 'keyboard-quit)
	(define-key evil-visual-state-map [escape] 'keyboard-quit)
	(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
	(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
	(global-set-key [escape] 'evil-exit-emacs-state)

	;; This ensures that the Vim navigation keys navigate by visual lines.

	(define-key evil-motion-state-map "j" #'evil-next-visual-line)
	(define-key evil-motion-state-map "k" #'evil-previous-visual-line)
	(define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
	(define-key evil-motion-state-map "^" #'evil-first-non-blank-of-visual-line)
	(define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)

	;; Evil binds =RET=, which is often used in other modes. This unbinds it, but there's a downside. =RET= now splits a line in normal mode, which causes me some aggravation sometimes. I'm not sure how best to fix it.

	(define-key evil-motion-state-map (kbd "RET") nil)
	(define-key evil-motion-state-map (kbd " ") nil)

	;; This makes most Emacs commands work in insert mode.

	(setcdr evil-insert-state-map nil)
	(define-key evil-insert-state-map [escape] 'evil-normal-state)



	;; This makes isearch backward work in Evil.

	(define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)


	;; Use jk instead of ESC
	(key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
	(key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
	(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
	(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
	(key-chord-define evil-normal-state-map "kj" 'evil-force-normal-state)
	(key-chord-define evil-visual-state-map "kj" 'evil-change-to-previous-state)
	(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
	(key-chord-define evil-replace-state-map "kj" 'evil-normal-state)

	;; h/l wrap around to next lines
	(setq-default evil-cross-lines t)

	))

;; (evil-mode 0)

;; Having a leader key makes it possible to have some convenient keyboard shortcuts that wouldn't be possible otherwise. I use space as the leader key.



;; Set initial state for some modes.

(loop for (mode . state) in '(
							  ;; (inferior-emacs-lisp-mode . emacs)
                              ;; (pylookup-mode . emacs)
                              ;; (comint-mode . emacs)
                              (shell-mode . emacs)
                              (term-mode . emacs)
                              ;; (bc-menu-mode . emacs)
                              ;; (magit-branch-manager-mode-map . emacs)
                              ;; (rdictcc-buffer-mode . emacs)
                              (ebib-entry-mode . emacs)
                              (ebib-index-mode . emacs)
                              (ebib-log-mode . emacs))
      do (evil-set-initial-state mode state))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
