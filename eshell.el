
;; Start eshell
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))


;;Shell
(setq multi-term-program "/usr/local/bin/fish")

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

;; From http://oremacs.com/2015/01/01/three-ansi-term-tips/
(add-hook 'term-exec-hook 'oleh-term-exec-hook)
(add-hook 'ansi-term-exec-hook 'oleh-term-exec-hook)
;; Make completion case-insensitive in eshell
(setq eshell-cmpl-ignore-case t)
(setq pcomplete-ignore-case t)
;; Start a regular shell
(global-set-key (kbd "C-x M-m") 'ansi-term)

(defalias 'e 'find-file)
(defalias 'eo 'find-file-other-window)


(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; (defun eshell/x ()
;;   (insert "exit")
;;   (eshell-send-input)
;;   (delete-window))

