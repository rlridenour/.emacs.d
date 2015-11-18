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
