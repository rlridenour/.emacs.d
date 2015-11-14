
;;;;;;;;;;;
;; LaTex ;;
;;;;;;;;;;;

(use-package tex-site
  :ensure auctex)

;; **** Italics and Bold

(add-hook 'LaTeX-mode-hook
   '(lambda ()
        (define-key LaTeX-mode-map (kbd "s-i") (kbd "\C-c \C-f \C-e"))
	(define-key LaTeX-mode-map (kbd "s-b") (kbd "\C-c \C-f \C-b"))
    )
)




;; **** SyncTeX, PDF mode, Skim
;; Set up AUCTeX to work with the Skim PDF viewer.



 
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;; Start Emacs server
(server-start)

    ;; Make emacs aware of multi-file projects
    ;; (setq-default TeX-master nil)
    
    ;; Auto-raise Emacs on activation (from Skim, usually)
    (defun raise-emacs-on-aqua()
    (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
    (add-hook 'server-switch-hook 'raise-emacs-on-aqua)


;; **** Local RefTeX Settings
;; Tell RefTeX where the bibliography files are. 

    ;; Make RefTex able to find my local bib files
    (setq reftex-bibpath-environment-variables
    '("/Users/rlridenour/Dropbox/bibtex"))

    ;; Default bibliography
    (setq reftex-default-bibliography
    '("/Users/rlridenour/Dropbox/bibtex/randybib.bib"))

;; **** Load Support Packages

;; ***** Load RefTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'pandoc-mode-hook 'turn-on-reftex)  ; with Pandoc mode
  (autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
  (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
  
  ;; Make RefTeX faster
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)
  
  ;; Make RefTeX work with Org-Mode
  ;; use 'C-c (' instead of 'C-c [' because the latter is already
  ;; defined in orgmode to the add-to-agenda command.
  (defun org-mode-reftex-setup ()
    (load-library "reftex") 
    (and (buffer-file-name)
    (file-exists-p (buffer-file-name))
    (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c (") 'reftex-citation))
  
  (add-hook 'org-mode-hook 'org-mode-reftex-setup)
  
  ;; RefTeX formats for biblatex (not natbib), and for pandoc
  (setq reftex-cite-format
        '(
          (?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?P . "[@%l]")
          (?T . "@%l [p. ]")
          (?x . "[]{%l}")
          (?X . "{%l}")
          ))
  
  (setq font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{") 
          ("citetitle" "[{") 
          ("citetitles" "[{") 
          ("headlessfullcite" "[{")))
  
  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)
  


;; ***** Load ebib
;;     ebib is a bibtex database manager that works inside emacs. It can
;;     talk to org-mode. See [[http://ebib.sourceforge.net/][the ebib project page]] for more. When Ebib is
;;     loaded, you can run it with =M-x ebib=.
  
;; #+source: ebib-load

;;   (autoload 'ebib "ebib" "Ebib, a BibTeX database manager." t)
;;   (setq ebib-preload-bib-files 
;;         '("/Users/kjhealy/Documents/bibs/socbib.bib"))
;;   (add-hook 'LaTeX-mode-hook #'(lambda ()
;;           (local-set-key "\C-cb" 'ebib-insert-bibtex-key)))


;; **** Configure AucTeX 
;; ***** Configure Biber
;; Allow AucTeX to use biber as well as/instead of bibtex.


    ;; Biber under AUCTeX
    (defun TeX-run-Biber (name command file)
      "Create a process for NAME using COMMAND to format FILE with Biber." 
     (let ((process (TeX-run-command name command file)))
        (setq TeX-sentinel-function 'TeX-Biber-sentinel)
        (if TeX-process-asynchronous
            process
          (TeX-synchronous-sentinel name file process))))
    
    (defun TeX-Biber-sentinel (process name)
      "Cleanup TeX output buffer after running Biber."
      (goto-char (point-max))
      (cond
       ;; Check whether Biber reports any warnings or errors.
       ((re-search-backward (concat
                             "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                             "\\(warnings?\\|error messages?\\))") nil t)
        ;; Tell the user their number so that she sees whether the
        ;; situation is getting better or worse.
        (message (concat "Biber finished with %s %s. "
                         "Type `%s' to display output.")
                 (match-string 1) (match-string 2)
                 (substitute-command-keys
                  "\\\\[TeX-recenter-output-buffer]")))
       (t
        (message (concat "Biber finished successfully. "
                         "Run LaTeX again to get citations right."))))
      (setq TeX-command-next TeX-command-default))
  
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
    )    



;; ***** Use Biblatex key template by default in bib files
;; #+source: bibtex-dialect
 ;; :tangle no
  ;; (setq bibtex-dialect "BibTeX")


;; ***** Use '-' as the separator in auto-generated bibtex keys
;; We use this (rather than the underscore character) for compatibilty with Pandoc.

;; #+source: bibtex-key-separator
 ;; :tangle no
  ;; (setq bibtex-autokey-titleword-separator "_")
  ;; (setq bibtex-autokey-year-title-separator ":_")


;; **** Configure RefTeX
;; ***** Default Bibliography
;;     This is important when editing source code in Org-babel, since the
;;       LaTeX source code block being edited probably doesn't include
;;       the \bibliography{} command that RefTeX uses to find
;;       bibliographic database(s). Make certain also that RefTeX has a
;;       path to the bibliographic databases. This source-code block is
;;       turned off be default as it should be configured by the user in
;;       a personal file/directory.
;; #+srcname: default-bibliography
;;  :tangle no
;;   (setq reftex-default-bibliography
;;         (quote
;;          ("user.bib" "local.bib")))

