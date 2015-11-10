;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown and Pandoc ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  )

;Make it easier to bold and italicize in Markdown Mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "s-b") 'markdown-insert-bold)
            (local-set-key (kbd "s-i") 'markdown-insert-italic)))

;; I haven't yet figured out why, but pressing =RET= deletes whitespace at the end of the line. That's useful for writing code, I'm sure, but not for writing Markdown text requiring hard line breaks. This little function just inserts two spaces at the end of the line and moves to the next line. I use it for prayers and poetry that I post on the blog, so it's called "mdpoetry." 
(fset 'mdpoetry
      "\C-e  \C-n")
(global-set-key (kbd "<f9>") 'mdpoetry)

;; Use Pandoc
(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
