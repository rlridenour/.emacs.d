;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :config
  (elpy-enable))


;;;;;;;;;;;;
;; Racket ;;
;;;;;;;;;;;;

(use-package racket-mode
  :ensure t
  :defer t
  :mode ("\\.rkt[dl]?\\'" . racket-mode))

;; (use-package geiser
;; :ensure t)
