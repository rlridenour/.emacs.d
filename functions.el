;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;;; Insert date
(defun insert-date-string ()
  "Insert current date yyyymmdd."
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun insert-standard-date ()
    "Inserts standard date time string." 
    (interactive)
    (insert (format-time-string "%B %e, %Y")))


(global-set-key (kbd "<f8>") 'insert-standard-date)
(global-set-key (kbd "C-c d") 'insert-date-string)

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
(global-set-key (kbd "s-<return>") 'rlr/smart-open-line)

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
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

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
(global-set-key (kbd "C-c r") 'rename-buffer-and-file)

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
(global-set-key (kbd "C-c o") 'open-with)

;;; Switch to Previous Buffer
(defun prelude-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(require 'dash)

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))

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

;; join line to next line
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; Count words
(defun rlr-count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))


;; From http://blog.binchen.org/posts/hello-ivy-mode-bye-helm.html

(defun ivy-imenu-get-candidates-from (alist  &optional prefix)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                       (ivy-imenu-get-candidates-from
                        (cl-loop for (e . v) in (cdr elm) collect
                                 (cons e (if (integerp v) (copy-marker v) v)))
                        (concat prefix (if prefix ".") (car elm)))
                   (and (cdr elm) ; bug in imenu, should not be needed.
                        (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                        (list (cons (concat prefix (if prefix ".") (car elm))
                                    (copy-marker (cdr elm))))))))


(defun ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setq items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))


(defun ivy-bookmark-goto ()
  "Open ANY bookmark"
  (interactive)
  (let (bookmarks filename)
    ;; load bookmarks
    (unless (featurep 'bookmark)
      (require 'bookmark))
    (bookmark-maybe-load-default-file)
    (setq bookmarks (and (boundp 'bookmark-alist) bookmark-alist))
    ;; do the real thing
    (ivy-read "bookmarks:"
              (delq nil (mapcar (lambda (bookmark)
                                  (let (key)
                                    ;; build key which will be displayed
                                    (cond
                                     ((and (assoc 'filename bookmark) (cdr (assoc 'filename bookmark)))
                                      (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'filename bookmark)))))
                                     ((and (assoc 'location bookmark) (cdr (assoc 'location bookmark)))
                                      ;; bmkp-jump-w3m is from bookmark+
                                      (unless (featurep 'bookmark+)
                                        (require 'bookmark+))
                                      (setq key (format "%s (%s)" (car bookmark) (cdr (assoc 'location bookmark)))))
                                     (t
                                      (setq key (car bookmark))))
                                    ;; re-shape the data so full bookmark be passed to ivy-read:action
                                    (cons key bookmark)))
                                bookmarks))
              :action (lambda (bookmark)
                        (bookmark-jump bookmark)))
    ))


;; Title-case from http://ergoemacs.org/emacs/elisp_title_case_text.html

(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φbegin φend are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           ξp1
           ξp2
           (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward ξskipChars (line-beginning-position))
         (setq ξp1 (point))
         (skip-chars-forward ξskipChars (line-end-position))
         (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
         (ξstrPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-excursion 
      (save-restriction
        (narrow-to-region φbegin φend)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while
                 (search-forward (aref ξx 0) nil t)
               (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
           ξstrPairs))))))

;; Find non-ascii characters in current buffer
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

