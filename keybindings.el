;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

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
  ("r" read-only-mode "read-only") 
  ("t" toggle-truncate-lines "truncate")
  ("w" whitespace-mode "whitespace")
  ("q" nil "cancel"))
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
(global-set-key (kbd "s-.") 'hydra-blog/body)

