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

;; Turn on Orgtbl-mode in Markdown-mode EDIT: Orgtble used C-c C-c, which conflicts with some Markdown keybindings.
;; (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

;; Use Pandoc
(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

;; Pandoc Conversion
;; Converts Markdown files to LaTeX articles and handouts using fish shell functions.
(defun pandoc-article ()
  "Convert file to LaTeX article"
  (interactive)
  (shell-command (concat "article " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".tex"))
  (find-file (concat (file-name-sans-extension buffer-file-name) ".tex")))

(defun pandoc-beamer ()
  "Convert file to LaTeX beamer file"
  (interactive)
  (shell-command (concat "beamer " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".tex"))
  (find-file (concat (file-name-sans-extension buffer-file-name) ".tex")))

(defun pandoc-slides ()
  "Convert file to Beamer slides"
  (interactive)
  (shell-command (concat "slides " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".pdf"))
  (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")))


(defun pandoc-obuletter ()
  "Convert file to LaTeX OBU letter"
  (interactive)
  (shell-command (concat "obuletter " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".tex"))
  (find-file (concat (file-name-sans-extension buffer-file-name) ".tex")))

(defun pandoc-pdf ()
  "Convert file to PDF"
    (interactive)
    (shell-command (concat "article " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".tex"))
	(shell-command (concat "mkpdf " (file-name-sans-extension buffer-file-name) ".tex"))
	(shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")))

(defun pandoc-handout ()
  "Convert file to LaTeX tufte-handout"
    (interactive)
    (shell-command (concat "handout " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".tex"))
	(find-file (concat (file-name-sans-extension buffer-file-name) ".tex")))

(defun pandoc-docx ()
  "Convert file to MS Word docx"
    (interactive)
    (shell-command (concat "convert " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".docx"))
	(shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".docx")))

(defun pandoc-html ()
  "Convert file to html"
    (interactive)
    (shell-command (concat "convert " (buffer-file-name) " " (file-name-sans-extension buffer-file-name) ".html"))
	(shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".html")))

(defun pandoc-clean ()
  (interactive)
  (shell-command "panclean"))
