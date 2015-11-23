;; Configuration for the eminently useful [[http://orgmode.org/][Org Mode]].

;; Org-mode is for keeping notes, maintaining ToDo lists, doing project
;; planning, and authoring with a fast and effective plain-text system.
;; Org Mode can be used as a very simple folding outliner or as a complex
;; GTD system or tool for reproducible research and literate programming.

;; For more information on org-mode check out [[http://orgmode.org/worg/][worg]], a large Org-mode wiki
;; which is also implemented using Org-mode and [[http://git-scm.com/][git]].

;; **** Settings

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)


;; **** Evil-Org

;; Use evil-org for evil keybindings in org mode.


;; (require 'evil-org)


;; **** Use Org Mode for TXT files

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  
;; **** Archive Settings
    ;; Where archived projects and tasks go.


  (setq org-archive-location "~/Dropbox/Org/archive.org::From %s")


;; **** Mobile Settings
   ;; Sync orgmode files with Dropbox and iPhone. 


   ;; Set to the location of your Org files on your local system
   (setq org-directory "~/Dropbox/Org")
   ;; Set to <your Dropbox root directory>/MobileOrg.
   (setq org-mobile-directory "~/Dropbox/MobileOrg")
   ;; Set to the files (or directory of files) you want sync'd
   (setq org-agenda-files (quote ("~/Dropbox/Org")))
   ;; Set to the name of the file where new notes will be stored
   (setq org-mobile-inbox-for-pull "~/Dropbox/Org/from-mobile.org")
   



;; **** Babel Settings
   ;; Configure org-mode so that when you edit source code in an indirect buffer (with C-c '), the buffer is opened in the current window. That way, your window organization isn't broken when switching.



  (setq org-src-window-setup 'current-window)

;; **** Exporter Settings and Helpful Packages
;; HTML and LaTeX exporters are shown by default. We add the Markdown exporter to the menu.


;; Autocomplete for orgmode
;; (require 'org-ac)
;; (org-ac/config-default)

;; Markdown exporter
(require 'ox-md)

(setq org-completion-use-ido t)
;; (require 'org-special-blocks)
;; (if window-system (require 'org-mouse))

;; Compatibility with WindMove
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
;; (if window-system (require 'org-mouse))


;; **** Use latexmk



(setq org-latex-to-pdf-process (list "/usr/texbin/latexmk -f -pdf %f"))


;; **** Org-Mode Hooks
;; Make yasnippet work properly with org-mode. 


;;  (defun yas/org-very-safe-expand ()
;;    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)
            ))




  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\M-\C-n" 'outline-next-visible-heading)
              (local-set-key "\M-\C-p" 'outline-previous-visible-heading)
              (local-set-key "\M-\C-u" 'outline-up-heading)
              ;; table
              (local-set-key "\M-\C-w" 'org-table-copy-region)
              (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
              (local-set-key "\M-\C-l" 'org-table-sort-lines)
              ;; display images
              (local-set-key "\M-I" 'org-toggle-iimage-in-org)
              ;; yasnippet (using the new org-cycle hooks)
              ;;(make-variable-buffer-local 'yas/trigger-key)
              ;;(setq yas/trigger-key [tab])
              ;;(add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              ;;(define-key yas/keymap [tab] 'yas/next-field)
              ))


;; **** Speed keys
;; Speed commands enable single-letter commands in Org-mode files when the point is at the beginning of a headline, or at the beginning of a code block.

;; See the =org-speed-commands-default= variable for a list of the keys and commands enabled at the beginning of headlines.  All code blocks are available at the beginning of a code block, the following key sequence =C-c C-v h= (bound to =org-babel-describe-bindings=) will display a list of the code blocks commands and their related keys.


  (setq org-use-speed-commands t)


;; **** Code blocks
;; This activates a number of widely used languages, you are encouraged to activate more languages using the customize interface for the =org-babel-load-languages= variable, or with an elisp form like the one below.  The customize interface of =org-babel-load-languages= contains an up to date list of the currently supported languages.

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (R . t)
     (perl . t)
     (ruby . t)
     (python . t)
     (js . t)
     (haskell . t)))


;; The next block makes org-babel aware that a lower-case 'r' in a =src= block header should be processed as R. 

;; #+source: add-r

    (add-to-list 'org-src-lang-modes
                 '("r" . ess-mode))


;; **** Code block fontification

;; The following displays the contents of code blocks in Org-mode files using the major-mode of the code.  It also changes the behavior of =TAB= to as if it were used in the appropriate major mode.  This means that reading and editing code form inside of your Org-mode files is much more like reading and editing of code using its major mode.

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)


;; Don't ask for confirmation on every =C-c C-c= code-block compile. 


  (setq org-confirm-babel-evaluate nil)


;; **** Nice Bulleted Lists
  ;; (require 'org-bullets)
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))





;; **** Configure Org-babel
   ;; - Add LaTeX to the list of languages Org-babel will recognize.
 
     (require 'ob-latex)
  ;; (org-babel-add-interpreter "latex")
  ;; (add-to-list 'org-babel-tangle-langs '("latex" "tex"))

   ;; - Add LaTeX to a list of languages that raise noweb-type errors.
 
  (add-to-list 'org-babel-noweb-error-langs "latex")





;; **** Org Capture

;; Use C-c c for Org Capture to ~/Dropbox/notes.org


;;(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Org/tasks.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/Org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))
     (define-key global-map "\C-cc" 'org-capture)

;; **** Tab doesn't split headings

(setq org-M-RET-may-split-line '((item) (default . t)))

;; **** Reference Links

;; Use Markdown-style reference links in Org Mode. From [[http://endlessparentheses.com/markdown-style-link-ids-in-org-mode.html][Artur Malabarba]]. Links have this format: [[lid:name][link text]] The reference id's have this form: 
;; #+LINK-ID: name http://www.url.com


(org-add-link-type "lid" 'endless/open-id-link 'endless/export-id-link)

(defun endless/open-id-link (path)
  "Follow an ID link to PATH."
  (browse-url (endless/find-id-link path)))

(defun endless/export-id-link (path desc format)
  "Create the export version of an ID link specified by PATH and DESC.
FORMATs understood are 'latex and 'html."
  (setq path (endless/find-id-link path))
  (cond
   ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
   ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
   (t desc)))

(defun endless/find-id-link (id &optional noerror)
  "Find \"#+LINK-ID: ID\" in current buffer and return the link.
Unless NOERROR is non-nil, throw an error if link not found."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward-regexp 
             (format "^#\\+LINK-ID: \\b%s\\b +\\(.*\\) *$" id)
             nil noerror)
        (match-string-no-properties 1)))))

;; **** Ispell for Org

;; From [[http://endlessparentheses.com/ispell-and-org-mode.html?source=rss][Endless Parentheses]]


(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)


;; tufte-book class for writing classy books
(require 'ox-latex) 
(add-to-list 'org-latex-classes
'("tuftebook"
"\\documentclass{tufte-book}\n
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 ;; tufte-handout class for writing classy handouts and papers
(require 'ox-latex) 
(add-to-list 'org-latex-classes
'("tuftehandout"
"\\documentclass{tufte-handout}
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

