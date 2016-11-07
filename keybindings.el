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

;; Hydras

;; Hydra-toggle
(defhydra hydra-toggle (:color blue) 
  "toggle"
  ("a" abbrev-mode "abbrev")
  ("c" column-number-mode "column")
  ("C" cdlatex-mode "cdlatex")
  ("d" toggle-debug-on-error "debug")
  ("e" evil-mode "evil")
  ("f" auto-fill-mode "fill")
  ;; ("g" god-mode "god")
  ("l" nlinum-mode "linum")
  ("o" olivetti-mode "olivetti")
  ("r" read-only-mode "read-only") 
  ("t" toggle-truncate-lines "truncate")
  ("w" wc-mode "word-count")
  ("W" whitespace-mode "whitespace")
  ("q" nil "global"))
;; (global-set-key (kbd "s-t") 'hydra-toggle/body)

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
;; (global-set-key (kbd "C-c b") 'hydra-blog/body)

(defhydra hydra-markdown (:color blue)
  ("a" pandoc-article "article")
  ("b" pandoc-beamer "beamer")
  ("s" pandoc-slides "slides")
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
;; (global-set-key (kbd "s-p") 'hydra-markdown/body)

(defhydra hydra-locate (:color blue)
  ("l" avy-goto-line "avy-line")
  ("L" goto-line "goto-line")
  ("w" avy-goto-word-1 "goto-word")
  ("b" ivy-bookmark-goto "bookmarks")
  ("m" ivy-imenu-goto "imenu")
  ("q" nil))

(bind-chords
 ("jh" . prelude-switch-to-previous-buffer)
 ("hj" . prelude-switch-to-previous-buffer))

(bind-keys
("s-0" . delete-window)
("s-1" . delete-other-windows)
("s-2" . swap-windows)
("s-3" . split-window-right)
("s-4" . nuke-all-buffers)
("s-5" . delete-frame)
("s-6" . toggle-window-split)
("S-C-<left>" . shrink-window-horizontally)
("S-C-<right>" . enlarge-window-horizontally)
("S-C-<down>" . shrink-window)
("S-C-<up>" . enlarge-window)
("C-x c" . save-buffers-kill-emacs)
("C-x w" . delete-frame)
;; ("s-." . helm-buffers-list)
;; ("C-c i" . ivy-imenu-goto)	
;; ("C-c b" . ivy-bookmark-goto)
("C-x C-b" . ibuffer)
("RET" . newline-and-indent)
("M-/" . hippie-expand)
("C-+" . text-scale-increase)
("C--" . text-scale-decrease)
("C-c C-k" . compile)
("<s-backspace>" . kill-whole-line)
("s-t" . hydra-toggle/body)
("s-p" . hydra-markdown/body)
("s-l" . hydra-locate/body)
("C-c f" . hydra-locate/body)
("C-c k" . prelude-kill-other-buffers)
("C-c u" . unfill-paragraph)
("s-d" . bjm/ivy-dired-recent-dirs)
("C-c v" . counsel-M-x)
("s-=" . endless/ispell-word-then-abbrev)
("<f5>" . call-last-kbd-macro))

;; http://apple.stackexchange.com/questions/48043/how-to-ssh-from-ipad-with-external-keyboard-and-emacs
(if (eq system-type 'gnu/linux)
	(let ((translations '( 229 [?\M-a]  nil [?\M-b]   231 [?\M-c]  8706 [?\M-d]  nil [?\M-e]
							   402 [?\M-f]  169 [?\M-g]   729 [?\M-h]   nil [?\M-i]  8710 [?\M-j]
							   730 [?\M-k]  172 [?\M-l]   181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
							   960 [?\M-p]  339 [?\M-q]   174 [?\M-r]   223 [?\M-s]  8224 [?\M-t]
							   nil [?\M-u] 8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]   165 [?\M-y]
							   937 [?\M-z]
							   197 [?\M-A]  305 [?\M-B]   199 [?\M-C]   206 [?\M-D]   nil [?\M-E]
							   207 [?\M-F]  733 [?\M-G]   211 [?\M-H]   nil [?\M-I]   212 [?\M-J]
							   63743 [?\M-K]  210 [?\M-L]   194 [?\M-M]   nil [?\M-N]   216 [?\M-O]
							   8719 [?\M-P]  338 [?\M-Q]  8240 [?\M-R]   205 [?\M-S]   711 [?\M-T]
							   nil [?\M-U] 9674 [?\M-V]  8222 [?\M-W]   731 [?\M-X]   193 [?\M-Y]
							   184 [?\M-Z]
							   nil [?\M-~]  161 [?\M-1]   162 [?\M-4]   163 [?\M-3]   167 [?\M-6]
							   170 [?\M-9]  171 [?\M-\\]  175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
							   182 [?\M-7]  183 [?\M-\(]  186 [?\M-0]   187 [?\M-|]   191 [?\M-\?]
							   198 [?\M-\"] 230 [?\M-']   247 [?\M-/]   728 [?\M->]  8211 [?\M-\-]
							   8212 [?\M-_] 8216 [?\M-\]] 8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[]
							   8221 [?\M-{] 8225 [?\M-&]  8226 [\?M-8]  8249 [?\M-#]  8250 [?\M-$]
							   8260 [?\M-!] 8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]  8800 [?\M-=]
							   8804 [?\M-,] 8805 [?\M-.] 64257 [?\M-%] 64258 [?\M-^])))
	  (while translations
		(let ((key (car translations)) (def (cadr translations)))
		  (if key
			  (define-key key-translation-map (make-string 1 key) def)))
		(setq translations (cddr translations))))
  )

