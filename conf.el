(load "~/Dropbox/.emacs.secrets")

(server-start)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq frame-title-format "%b")

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(global-hl-line-mode 1)
(set-default 'cursor-type 'bar)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(add-to-list 'load-path
             "~/.emacs.d/elisp")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq custom-file "~/.emacs.d/customs.el")
(load custom-file)

(use-package ample-theme  
  :ensure t
  :config (load-theme 'ample-flat t))
(add-to-list 'default-frame-alist 
	     '(font . "Fira Mono-10"))

(let ((basedir "~/.emacs.d/themes/"))
      (dolist (f (directory-files basedir))
        (if (and (not (or (equal f ".") (equal f "..")))
                 (file-directory-p (concat basedir f)))
            (add-to-list 'custom-theme-load-path (concat basedir f)))))

(set-register ?a '(file . "~/.config/awesome/rc.lua")) ; Awesome window manager config
(set-register ?e '(file . "~/.emacs.d/conf.org")) ; Emacs config

(setq calendar-week-start-day 1
      calendar-day-name-array
      ["sunnuntai" "maanantai" "tiistai" "keskiviikko"
       "torstai" "perjantai" "lauantai"]
      calendar-month-name-array
      ["tammikuu" "helmikuu" "maaliskuu" "huhtikuu" "toukokuu" 
       "kesäkuu" "heinäkuu" "elokuu" "syyskuu"
       "lokakuu" "marraskuu" "joulukuu"]
      calendar-day-abbrev-array
      ["sun" "maa" "tii" "kes" "tor" "per" "lau"]
      calendar-day-header-array
      ["su" "ma" "ti" "ke" "to" "pe" "la"])

(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-right) (other-window 1)))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode 1))

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(defalias 'list-buffers 'ibuffer)

(global-set-key (kbd "C-¨") 'save-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(setq abbrev-file-name
      "~/.emacs.d/abbrevs")
(setq-default abbrev-mode t)
(setq save-abbrevs t)

(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(diminish 'abbrev-mode)
  (diminish 'which-key-mode)
;  (diminish 'yas-minor-mode)
(diminish 'visual-line-mode)

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-major-mode 'emacs-lisp-mode-hook "el")

(load "word-count")

(use-package lua-mode
  :ensure t)

(use-package keyfreq
  :ensure t    
  :config (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(setq sentence-end-double-space nil)

(setq scroll-step            1
      scroll-conservatively  10000)

(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(bind-key "M-t" nil) ;; which used to be transpose-words
(bind-key "M-t l" 'transpose-lines)
(bind-key "M-t w" 'transpose-words)
(bind-key "M-t t" 'transpose-words)
(bind-key "M-t M-t" 'transpose-words)
(bind-key "M-t s" 'transpose-sexps)

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'rr 'replace-regexp)
(defalias 'plp 'package-list-packages)

(show-paren-mode 1)
(setq show-paren-delay 0)

(defvar computer-type nil)
(defun tsl/desktop-or-laptop ()
  "Based on screen resolution, define whether Emacs is opened on desktop or laptop."
  (interactive)
  (if (>= (x-display-pixel-height) 900)
      (setq computer-type 'desktop)
    (setq computer-type 'laptop)))
(tsl/desktop-or-laptop)

(use-package base16-theme
  :ensure t)
(use-package evil
  :ensure t)
(use-package evil-org
  :ensure t)
(use-package palimpsest
  :ensure t)
(use-package paredit
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package misc-cmds
  :ensure t)
(use-package projectile
  :ensure t)
(use-package unbound
  :ensure t)

(global-unset-key (kbd "C-x m"))
;  (global-unset-key (kbd "M-x"))
  (global-set-key (kbd "C-x m") 'execute-extended-command)
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :bind (("C-z" . undo)
  ("C-S-z" . undo-tree-redo)))

(defun tsl/reload-settings ()
  (interactive)
  (org-babel-load-file "~/.emacs.d/conf.org"))

(defun tsl/writing-view-3-windows ()
  (interactive)
  (split-window-horizontally 167)
  (split-window-horizontally 70)
  (clone-indirect-buffer nil t)
  (clone-indirect-buffer nil t))

(defun tsl/writing-view-2-windows ()
  (interactive)
  (split-window-horizontally 70)
  (clone-indirect-buffer nil t))

(defun copy-region-or-whole-line (beg end flash)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      ((if ) (equal (current-column) 0)
       (goto-char end)
       (goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'copy-region-or-whole-line)

(defun cut-region-or-line ()
  (interactive (if (use-region-p)
		   (kill-region (region-beginning) (region-end))
		 (kill-line nil))))
(global-set-key [remap kill-line] 'cut-region-or-line)

(use-package magit
:ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package org-bullets
  :ensure t
  :init (setq org-bullets-bullet-list '("►" "◾" "◆"))
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

 (setq org-ellipsis " ▼")

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)))

(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key org-mode-map (kbd "C-c e") #'org-table-edit-field)

(with-eval-after-load 'org
 (define-key org-mode-map (kbd "M-p") #'outline-previous-visible-heading)
 (define-key org-mode-map (kbd "M-n") #'outline-next-visible-heading)
 (define-key org-mode-map (kbd "M-P") #'org-backward-heading-same-level)
 (define-key org-mode-map (kbd "M-N") #'org-forward-heading-same-level)
 (define-key org-mode-map (kbd "M-U") #'outline-up-heading))

(defadvice org-capture-finalize 
    (after delete-capture-frame activate)  
  "Advise capture-finalize to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
    (delete-frame)))

(defadvice org-capture-destroy 
    (after delete-capture-frame activate)  
  "Advise capture-destroy to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
    (delete-frame)))  

(use-package noflet
  :ensure t )
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(setq org-agenda-files '("~/Dropbox/org/inbox.org"
      "~/Dropbox/org/tickler.org"
      "~/Dropbox/org/todo.org"))

(setq org-log-into-drawer t)

(setq org-clock-into-drawer "CLOCKING")

(setq org-todo-keywords
           '((sequence "TODO(t)" "SEURAAVA(s)" "KESKEN(k)" "ODOTTAA(o@)" "|" "VALMIS(v!)" "PERUTTU(p@)")))

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-enforce-todo-checkbox-dependencies t)

;  (defun org-summary-todo (n-done n-not-done)
 ;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
  ;  (let (org-log-done org-log-states)   ; turn off logging
   ;   (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)
                           ("~/Dropbox/org/someday.org" :maxlevel . 3)
                           ))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-custom-commands
        '(("n" "seuraavat ja kesken" todo "SEURAAVA|KESKEN")
          ("b" "blogit" todo ""
           ((org-agenda-files '("~/Dropbox/org/blogit.org"))))
          ("d" "päivä"
           ((agenda "" ((org-agenda-span 1) (org-agenda-tag-filter-preset '("-media"))))))
          ("u" "media"
           ((agenda "" ((org-agenda-span 1) (org-agenda-tag-filter-preset '("+media"))))))
          ("p" "projektit" tags-todo "proj")
          ))

; a tT mM sS L C e

; (setq org-agenda-show-inherited-tags nil)

(fset 'tsl/blog-export
      "\C-c\C-e\C-b\C-shH\C-xh\C-w\C-x0")

(fset 'tsl/ascii-export
      "\C-c\C-etA\C-xh\C-w\C-x0")

(use-package ox-reveal
  :ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

(defun tsl/org-ascii-clean-text ()
  (save-excursion (setq org-ascii-text-width
                        (cadr (goto-longest-line (point-min) (point-max))))
                        (setq org-ascii-headline-spacing '(0 . 0))
                        (setq org-ascii-paragraph-spacing 0)
                        (setq org-ascii-inner-margin 0)
                        (setq org-ascii-underline '((ascii nil nil nil)
			(latin1 nil nil nil)
			(utf-8 nil nil nil nil nil)))))

(add-hook 'before-save-hook
          (lambda () (if (eq major-mode 'org-mode)
                         (tsl/org-ascii-clean-text))))

(setq org-extend-today-until 5)

(setq org-return-follows-link t)

(setq org-agenda-default-appointment-duration 60)
