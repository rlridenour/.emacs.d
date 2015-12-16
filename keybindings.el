;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))

(use-package key-chord
  :ensure t
  :defer t
  :config
  (key-chord-mode 1))

(bind-chords
 ("jk" . prelude-switch-to-previous-buffer)
 ("kj" . prelude-switch-to-previous-buffer))

(bind-keys
("s-0" . delete-window)
("s-1" . delete-other-windows)
("s-2" . swap-windows)
("s-3" . split-window-right)
("s-4" . desktop-clear)
("s-5" . delete-frame)
("S-C-<left>" . shrink-window-horizontally)
("S-C-<right>" . enlarge-window-horizontally)
("S-C-<down>" . shrink-window)
("S-C-<up>" . enlarge-window)
("C-x c" . save-buffers-kill-emacs)
("s-." . ivy-switch-buffer)
("C-x C-b" . ibuffer))



;; (global-set-key (kbd "s-0") 'delete-window) 
;; (global-set-key (kbd "s-1") 'delete-other-windows) 
;; (global-set-key (kbd "s-3") 'split-window-right)
;; (global-set-key (kbd "s-5") 'delete-frame)
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; (global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
;; (global-set-key (kbd "s-p") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)

;; Other Miscellaneous Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
;; (global-set-key (kbd "C-x g") 'magit-status)
;; (global-set-key (kbd "C-r") 'isearch-backward)
;; (global-set-key (kbd "s-g") 'god-mode)
;; (global-set-key (kbd "s-o") 'helm-find-files)

(global-set-key (kbd "<s-backspace>") 'kill-whole-line)




;; Hydras

;; Hydra-toggle
(defhydra hydra-toggle (:color blue) 
  "toggle"
  ("a" abbrev-mode "abbrev")
  ("c" column-number-mode "column")
  ("d" toggle-debug-on-error "debug")
  ;; ("e" evil-mode "evil")
  ("f" auto-fill-mode "fill")
  ;; ("g" god-mode "god")
  ("l" linum-mode "linum")
  ("o" olivetti-mode "olivetti")
  ("r" read-only-mode "read-only") 
  ("t" toggle-truncate-lines "truncate")
  ("w" whitespace-mode "whitespace")
  ("q" nil "global"))
(global-set-key (kbd "s-t") 'hydra-toggle/body)

;; Hydra-Blog

     ;; I didn't realize that I could jump to a directory with Hydra. I got the tip from [[http://thewanderingcoder.com/2015/02/shortcuts-to-default-directories/][Sean Miller]].

(defhydra hydra-blog (:color blue)
"buffer"
    ("n" jekyll-draft-post "new post")
    ("p" jekyll-publish-post "publish")
    ("t" jekyll-timestamp "timestamp")
    ("P" (find-file "~/Sites/rlridenour.github.io/_posts/") "post directory")
    ("d" (find-file "~/Sites/rlridenour.github.io/_drafts") "draft directory")
    ("q" nil))
(global-set-key (kbd "C-c b") 'hydra-blog/body)

(defhydra hydra-markdown (:color blue)
  ("a" pandoc-article "article")
  ("h" pandoc-handout "handout")
  ("o" pandoc-obuletter "obu letter")
  ("d" pandoc-docx "docx")
  ("w" pandoc-html "html")
  ("p" pandoc-pdf "pdf")
  ("t" pandoc-clean "trash non-md files")
  ("c" tex-clean "clean aux files")
  ("C" tex-clean-all "clean all")
  ("1" markdown-insert-header-atx-1 "header 1")
  ("2" markdown-insert-header-atx-2 "header 2")
  ("3" markdown-insert-header-atx-3 "header 3")
  ("4" markdown-insert-header-atx-4 "header 4")
  ("q" nil))
(global-set-key (kbd "s-p") 'hydra-markdown/body)

