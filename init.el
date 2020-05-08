
(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))
(put 'downcase-region 'disabled nil)
