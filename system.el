;; Keep custom-set variables separate from configuration file.
(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set modifier keys in OS X
   
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper) 
   



;; Always prefer UTF-8


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; Save When Losing Focus

(defun save-all ()
  (interactive)
(save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)


;; Use "y" and "n":
(defalias 'yes-or-no-p 'y-or-n-p)



;; Confirm killing emacs on graphical sessions:
(when (window-system)
(setq confirm-kill-emacs 'yes-or-no-p))


;; Edit by Visual Lines
(global-visual-line-mode t)



;; Navigate visual lines:
(setq line-move-visual t)



;; Single space ends sentence:
(setq sentence-end-double-space nil)



;; Don't make backup files
(setq make-backup-files nil)


;; Spelling
(setq flyspell-issue-welcome-flag nil)


;; From [[https://joelkuiper.eu/spellcheck_emacs][Joel Kuiper]]

;; Enable flyspell mode for highlighting spelling errors.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

   
;; Check comments and strings when coding.
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook
                R-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))


;; Use F7 to check current word, M-F7 for next word.
(global-set-key (kbd "<f7>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f7>") 'flyspell-check-next-highlighted-word)


;; Spell-check with right mouse button.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))


;; Use hunspell with US English dictionary.
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))


;; Store personal dictionary in Dropbox to sync between machines.
(setq ispell-personal-dictionary "/Users/rlridenour/Dropbox/emacs/ridenour-ispell-dictionary ")

  

;; Hide various file types, most LaTeX auxiliary files, in Dired.
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-omit-extensions '("fdb_latexmk" "aux" "bbl" "blg" "fls" "glo" "idx" "ilg" "ind" "ist" "log" "out" "gz" "DS_Store"))
(setq dired-dwim-target t)

;; Load Abbreviations
(load "~/Dropbox/emacs/my-emacs-abbrev")


;; Bookmarks
(require 'bookmark)
(bookmark-bmenu-list)


;; Recent Files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Don't ask for confirmation to kill processes when exiting Emacs. Credit to [[http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html][Timothy Pratley]].
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           (flet ((process-list ())) ad-do-it))
     

;;Shell
(setq multi-term-program "/usr/local/bin/zsh")

;; Kill contents of scratch buffer, not the buffer itself. From [[http://emacswiki.org/emacs/RecreateScratchBuffer][TN]].


(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)



;; Mark date and time that files were saved.
(add-hook 'before-save-hook 'time-stamp)



(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))



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


;; == LaTex / AucTeX ==
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  
  ;; Don't use Helm for the reftex-citation lookup
;;  (eval-after-load 'helm-mode
;;    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
;;    )
  
  )





;;; Functions


;;; Insert date


(defun insert-date ()
  "Insert current date yyyymmdd."
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(global-set-key (kbd "<f8>") 'insert-date)
(global-set-key (kbd "C-c d") 'insert-date)


;;; Compact-Uncompact Block


  ;; Fill-paragraph from Xah Lee (http://ergoemacs.org/emacs/modernization_fill-paragraph.html)

  (defun rlr/compact-uncompact-block ()
    "Remove or add line ending chars on current paragraph.
  This command is similar to a toggle of `fill-paragraph'.
  When there is a text selection, act on the region."
    (interactive)

    ;; This command symbol has a property “'stateIsCompact-p”.
    (let (currentStateIsCompact (bigFillColumnVal 90002000) (deactivate-mark nil))
      ;; 90002000 is just random. you can use `most-positive-fixnum'

      (save-excursion
        ;; Determine whether the text is currently compact.
        (setq currentStateIsCompact
              (if (eq last-command this-command)
                  (get this-command 'stateIsCompact-p)
                (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

        (if (use-region-p)
            (if currentStateIsCompact
                (fill-region (region-beginning) (region-end))
              (let ((fill-column bigFillColumnVal))
                (fill-region (region-beginning) (region-end))) )
          (if currentStateIsCompact
              (fill-paragraph nil)
            (let ((fill-column bigFillColumnVal))
              (fill-paragraph nil)) ) )

        (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )


;;; Focus Emacs 


(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

   

;;; Smart Open Line
;;    From [[https://github.com/grettke/home/blob/master/.emacs.el#L436][Grant Rettke]].


(defun rlr/smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))



;;; Kill Buffer and Delete File
;; From [https://github.com/bbatsov/prelude][Emacs Prelude]

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))


;;; Rename Buffer and File

(defun rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

    
;;; Open With External App

(defun open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (start-process "prelude-open-with-process" nil program current-file-name)))


;;; Switch to Previous Buffer

(defun prelude-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


     
;;; Double Capitals
;; From  [[http://endlessparentheses.com/fixing-double-capitals-as-you-type.html?source=rss][Endless Parentheses]]


;; Define function
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

;; (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
;; Define minor mode
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))
;; Add hook to text mode
(add-hook 'text-mode-hook #'dubcaps-mode)


;;; Byte-Compile Config


(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))


;; Start eshell
(global-set-key (kbd "C-x m") (lambda () (interactive) (eshell t)))
;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell
(global-set-key (kbd "C-x M-m") 'shell)

;; CUA mode for rectangle editing
;; Sometimes very useful (but we don't use the core cua keys.)

  (setq cua-enable-cua-keys nil)
  (cua-mode)

;; To start a rectangle, use [C-return] and extend it using the normal
;; movement keys (up, down, left, right, home, end, C-home,
;; C-end). Once the rectangle has the desired size, you can cut or
;; copy it using C-w and M-w, and you can
;; subsequently insert it - as a rectangle - using C-y.  So
;; the only new command you need to know to work with cua-mode
;; rectangles is C-return!
;;
;; Normally, when you paste a rectangle using C-v (C-y), each line of
;; the rectangle is inserted into the existing lines in the buffer.
;; If overwrite-mode is active when you paste a rectangle, it is
;; inserted as normal (multi-line) text.
;;
;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [C-return] cancels the rectangle
;; [C-space] activates the region bounded by the rectangle

;; cua-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] copies the rectangle as normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles virtual straight rectangle edges
;; [M-P] inserts tabs and spaces (padding) to make real straight edges
;; [M-q] performs text filling on the rectangle
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].
  
;; set shortcut to kill whole emacs session
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)

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
(global-set-key (kbd "s-<return>") 'rlr/smart-open-line)
(global-set-key (kbd "<s-backspace>") 'kill-whole-line)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)
(global-set-key (kbd "C-c r") 'rename-buffer-and-file)
(global-set-key (kbd "C-c o") 'open-with)


;; Blog settings

(defun jekyll-timestamp ()
  "Update existing date: timestamp on a Jekyll page or post."
  (interactive)
  (save-excursion (
		   goto-char 1)
		  (re-search-forward "^date:")
		  (let ((beg (point)))
		    (end-of-line)
		    (delete-region beg (point)))
		  (insert (concat " " (format-time-string "%Y-%m-%d %H:%M:%S"))))
  )
;; TODO: Make the function add a date variable if none exists.

;; (defun jekyll-timestamp ()
;;   "Insert a time stamp suitable for use in a Jekyll page or post.  Replaces current text selection."
;;   (interactive)
;;   (when (region-active-p) (delete-region (region-beginning) (region-end) ) )
;;   (insert (format-time-string "%Y-%m-%d %H:%M:%S %z")))

;; All of the below is taken from http://www.gorgnegre.com/linux/using-emacs-orgmode-to-blog-with-jekyll.html
;; (Later tweaked a bit.)

(global-set-key (kbd "C-x j n") 'jekyll-draft-post)
(global-set-key (kbd "C-x j p") 'jekyll-publish-post)
(global-set-key (kbd "C-x j t") 'jekyll-timestamp)
(global-set-key (kbd "C-x j o") (lambda () (interactive) (find-file "~/Sites/rlridenour.github.io/")))

(global-set-key (kbd "C-x j P") (lambda () (interactive) (find-file "~/Sites/rlridenour.github.io/_posts/")))
(global-set-key (kbd "C-x j D") (lambda () (interactive) (find-file "~/Sites/rlridenour.github.io/_drafts/")))

(defvar jekyll-directory "~/Sites/rlridenour.github.io/" "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/" "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/" "Relative path to posts directory.")
(defvar jekyll-post-ext ".md"  "File extension of Jekyll posts.")
(defvar jekyll-post-template "---\nlayout: post\ntitle: %s\ntags:\n- \ncomments: true\ndate: \n---\n"
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-make-slug (s) "Turn a string into a slug."
  (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s) "Escape a string for YAML."
  (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

(defun jekyll-draft-post (title) "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
                            (jekyll-make-slug title)
                            jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-publish-post () "Move a draft post to the posts directory, and rename it so that it contains the date."
  (interactive)
  (cond
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (expand-file-name (concat jekyll-directory jekyll-drafts-dir))))
    (message "This is not a draft post.")
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
            (concat jekyll-directory jekyll-drafts-dir)))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat jekyll-directory jekyll-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

(provide 'setup-jekyll)










;; Turn off debugging and set default directory

;;(setq debug-on-error nil)
;;(setq debug-on-quit nil)
;;(setq default-directory "~/")
