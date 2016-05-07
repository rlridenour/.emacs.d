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
		  helm-quick-update t
		  helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
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



