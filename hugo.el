;; Hugo Settings

(defun hugo-timestamp ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
				   goto-char 1)
				  (re-search-forward "^date:")
				  (let ((beg (point)))
					(end-of-line)
					(delete-region beg (point)))
				  (insert (concat " " (format-time-string "\"%Y-%m-%dT%H:%M:%S\"")))))

(defvar hugo-directory "~/Sites/hugo/source/" "Path to Hugo blog.")
(defvar hugo-posts-dir "content/post/" "Relative path to posts directory.")
(defvar hugo-post-ext ".md"  "File extension of Hugo posts.")
(defvar hugo-post-template "---\ntitle: \"%s\"\ndraft: true\ncategories: []\ntags:\n- \ncomments: true\ndate: \nhighlight: true\nmarkup: \"\"\nmath: false\nurl: \"\"\n---\n"
  "Default template for Hugo posts. %s will be replace by the post title.")

(defun hugo-make-slug (s) "Turn a string into a slug."
	   (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun hugo-yaml-escape (s) "Escape a string for YAML."
	   (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

(defun hugo-draft-post (title) "Create a new Hugo blog post."
	   (interactive "sPost Title: ")
	   (let ((draft-file (concat hugo-directory hugo-posts-dir
								 (format-time-string "%Y-%m-%d-")
								 (hugo-make-slug title)
								 hugo-post-ext)))
		 (if (file-exists-p draft-file)
			 (find-file draft-file)
		   (find-file draft-file)
		   (insert (format hugo-post-template (hugo-yaml-escape title))))))

(defun hugo-publish-post ()
  "Update timestamp and set draft to false."
  (interactive)
  (hugo-timestamp)
  (save-excursion (
				   goto-char 1)
				  (re-search-forward "^draft:")
				  (let ((beg (point)))
					(end-of-line)
					(delete-region beg (point)))
				  (insert " false"))) 

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(progn (setq ,orig-dir default-directory)
            (cd ,DIR) ,@FORMS (cd ,orig-dir))))

(defun hugo-deploy ()
  "Push changes upstream."
  (interactive)
  (with-dir hugo-directory
            (shell-command "git add .")
            (--> (current-time-string)
                 (concat "git commit -m \"" it "\"")
                 (shell-command it))
            (magit-push-current-to-upstream nil)))

(global-set-key (kbd "C-c h n") 'hugo-draft-post)
(global-set-key (kbd "C-c h p") 'hugo-publish-post)
(global-set-key (kbd "C-c h t") 'hugo-timestamp)
(global-set-key (kbd "C-c h O") (lambda () (interactive) (find-file "~/Sites/hugo/source/")))
(global-set-key (kbd "C-c h P") (lambda () (interactive) (find-file "~/Sites/hugo/source/content/post/")))
(provide 'setup-hugo)
(global-set-key (kbd "C-c h d") 'hugo-deploy)
