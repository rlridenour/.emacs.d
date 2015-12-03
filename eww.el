;; Eww browser for Emacs

;; From http://oremacs.com/2014/12/30/ace-link-eww/
(use-package ace-link
  :ensure t
  :init
    (ace-link-setup-default))

(defun oleh-eww-hook ()
  (define-key eww-mode-map "j" 'oww-down)
  (define-key eww-mode-map "k" 'oww-up)
  (define-key eww-mode-map "l" 'forward-char)
  (define-key eww-mode-map "L" 'eww-back-url)
  (define-key eww-mode-map "h" 'backward-char)
  (define-key eww-mode-map "v" 'recenter-top-bottom)
  (define-key eww-mode-map "V" 'eww-view-source)
  (define-key eww-mode-map "m" 'eww-follow-link)
  (define-key eww-mode-map "a" 'move-beginning-of-line)
  (define-key eww-mode-map "e" 'move-end-of-line)
  (define-key eww-mode-map "o" 'ace-link-eww)
  (define-key eww-mode-map "f" 'ace-link-eww)
  (define-key eww-mode-map "y" 'eww))
(add-hook 'eww-mode-hook 'oleh-eww-hook)

(defun oww-down (arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

(defun oww-up (arg)
(interactive "p")
    (if (bolp)
      (progn
        (forward-line -1)
        (backward-paragraph arg)
        (forward-line 1))
    (line-move (- arg))))
