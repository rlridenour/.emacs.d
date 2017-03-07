;;;;;;;;;;;;;;
;; Ivy-Mode ;;
;;;;;;;;;;;;;;


(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind
  (("s-r" . counsel-recentf)
   ;; ("C-s" . swiper)
   ("s-f" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("s-o" . counsel-find-file)
   ("C-c b" . counsel-bookmark)
   ("C-c i" . counsel-imenu)
   ("s-." . ivy-switch-buffer)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c a" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ("C-c C-r" . ivy-resume)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call))
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  ;; version of ivy-yank-word to yank from start of word
  ;; from http://pragmaticemacs.com/emacs/search-or-swipe-for-the-current-word/
  (defun bjm/ivy-yank-whole-word ()
	"Pull next word from buffer into search string."
	(interactive)
	(let (amend)
	  (with-ivy-window
		;;move to last word boundary
		(re-search-backward "\\b")
		(let ((pt (point))
			  (le (line-end-position)))
		  (forward-word 1)
		  (if (> (point) le)
			  (goto-char pt)
			(setq amend (buffer-substring-no-properties pt (point))))))
	  (when amend
		(insert (replace-regexp-in-string "  +" " " amend)))))

  ;; bind it to M-j
  (define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)
  )

