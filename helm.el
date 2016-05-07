;; Helm

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
	(require 'helm-config)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
		  helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
		  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
		  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
		  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
		  helm-ff-file-name-history-use-recentf t
		  helm-quick-update t
		  helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
		  helm-autoresize-mode t)
	
 	
    (helm-mode))
  :bind (("C-c h" . helm-mini)
		 ("s-r" . helm-mini)
		 ("C-x C-f" . helm-find-files)
		 ("s-o" . helm-find-files)
		 ("C-h a" . helm-apropos)
		 ("C-x C-b" . helm-buffers-list)
		 ("C-x b" . helm-buffers-list)
		 ("M-y" . helm-show-kill-ring)
		 ("M-x" . helm-M-x)
		 ("C-x c o" . helm-occur)
		 ("C-x c s" . helm-swoop)
		 ("C-x c y" . helm-yas-complete)
		 ("C-x c Y" . helm-yas-create-snippet-on-region)
		 ("C-x c b" . my/helm-do-grep-book-notes)
		 ("C-x c SPC" . helm-all-mark-rings)))



(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
