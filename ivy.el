;;;;;;;;;;;;;;
;; Ivy-Mode ;;
;;;;;;;;;;;;;;


(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind
  ("s-r" . ivy-recentf)
  ("C-s" . swiper)
  ("s-f" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate)
  ("C-S-o" . counsel-rhythmbox)
  ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) "))



