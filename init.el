(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "conf.org"
                   user-emacs-directory))

