;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

;; Flycheck

(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;;;;;;;;;;;;
;; Racket ;;
;;;;;;;;;;;;

(use-package racket-mode
  :ensure t
  :defer t
  :mode ("\\.rkt[dl]?\\'" . racket-mode))

;; (use-package geiser
;; :ensure t)

;;;;;;;;;;;;;;;;;
;; Common Lisp ;;
;;;;;;;;;;;;;;;;;

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;;;;;;;;;;
;; Swift ;;
;;;;;;;;;;;

(use-package swift-mode
  :ensure t
  :defer t)
