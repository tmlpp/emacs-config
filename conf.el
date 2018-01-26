(server-start)

(load "~/Dropbox/.emacs.secrets")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)
(add-to-list 'load-path
             "~/.emacs.d/elisp")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq frame-title-format "%b")

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(use-package ample-theme  
  :ensure t
  :config (load-theme 'ample-flat t))

(add-to-list 'default-frame-alist 
             '(font . "Iosevka-12"))

(let ((basedir "~/.emacs.d/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(setq custom-file "~/.emacs.d/customs.el")
(load custom-file)

(diminish 'abbrev-mode)
  (diminish 'which-key-mode)
;  (diminish 'yas-minor-mode)
(diminish 'visual-line-mode)
(diminish 'evil-org-mode)

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-major-mode 'emacs-lisp-mode-hook "el")

(defvar computer-type nil)
(defun tsl/desktop-or-laptop ()
  "Based on screen resolution, define whether Emacs is opened on desktop or laptop."
  (interactive)
  (if (> (x-display-pixel-height) 900)
      (setq computer-type 'desktop)
    (setq computer-type 'laptop)))
(tsl/desktop-or-laptop)

(use-package powerline
  :ensure t)
(powerline-vim-theme)

(set-register ?e '(file . "~/.emacs.d/conf.org")) ; Emacs config
(set-register ?i '(file . "~/.config/i3/config")) ; i3

(setq calendar-week-start-day 1
      calendar-day-name-array
      ["sunnuntai"
       "maanantai"
       "tiistai"
       "keskiviikko"
       "torstai"
       "perjantai"
       "lauantai"]
      calendar-month-name-array
      ["tammi" "helmi" "maalis" "huhti" "touko" 
       "kesä" "heinä" "elo" "syys"
       "loka" "marras" "joulu"]
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
; (setq-default abbrev-mode t)
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (abbrev-mode 1)))
(setq save-abbrevs t)

(linum-relative-global-mode)

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

(use-package helpful
:ensure t)

(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-c C-.") #'helpful-at-point)

(load "word-count")

(use-package lua-mode
  :ensure t)

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

(defun tsl/spotify-links-to-embed ()
  "Convert Spotify song links to embed code."
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "https:/+open\\.spotify\\.com/track/\\(.+\\)" nil t)
    (replace-match "<iframe src=\"https://embed.spotify.com/?uri=spotify%3Atrack%3A\\1\" width=\"100%\" height=\"100\" frameborder=\"0\" allowtransparency=\"true\"></iframe>" nil nil)))

(defun tsl/youtube-links-to-embed ()
  "Convert Youtube song links to embed code."
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "^https:/+www\\.youtube\\.com/watch\\?v=\\(.+\\)" nil t) ; Only search for links in beginning of line to avoid replacing links in text.
    (replace-match "<iframe width=\"100%\" height=\"456\" src=\"https://www.youtube.com/embed/\\1\" frameborder=\"0\" allowfullscreen></iframe>" nil nil)))

(use-package base16-theme
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
(use-package key-chord
:ensure t)
(use-package counsel-projectile
:ensure t)

(use-package evil
  :ensure t
  :init (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config (evil-mode))

(use-package evil-org
  :ensure t)

(add-hook 'org-mode-hook
          (lambda ()
            (evil-org-mode)))

(defun tsl/evil-insert-line-below-or-open-link ()
"If cursor is on link, open the link with 'org-open-at-point. Otherwise insert new line under current one and return to evli-normal-state."
  (interactive)
  (if (and org-return-follows-link
           (or (org-in-regexp org-ts-regexp-both nil t)
               (org-in-regexp org-tsr-regexp-both nil  t)
               (org-in-regexp org-any-link-re nil t)))
      (call-interactively #'org-open-at-point)
    (evil-open-below nil)
    (evil-normal-state)))
(define-key evil-normal-state-map [return] 'tsl/evil-insert-line-below-or-open-link)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(use-package general
  :ensure t)
(general-define-key
                :prefix "SPC"
                :keymaps 'normal
                "r" 'jump-to-register
                "s" 'save-buffer
                "t" 'org-todo
                "q" 'evil-quit
                "d" 'org-cut-special
                "y" 'org-copy-special
                "ma" 'abbrev-mode
                "ml" 'linum-relative-mode
                "mp" 'electric-pair-mode
                "fo" 'find-file
                "fO" 'find-file-other-window
                "u" 'undo-tree-visualize
                "x" 'execute-extended-command
                ;"ss" 'clone-indirect-buffer-other-window
                ;"ss" 'evil-window-split
                ;"ss" 'evil-window-vsplit
                ;"ss" 'evil-window-left
                ;"ss" 'evil-window-right
                ;"ss" 'evil-window-up
                ;"ss" 'evil-window-down
                ;"ss" 'delete-other-windows
                ;"ss" 'org-refile

                )

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

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

(use-package magit
:ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package ledger-mode
  :ensure t)
(set-register ?l '(file . "~/Dropbox/ledger/my.ledger"))

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(setq browse-url-browser-function 'browse-url-xdg-open)

(use-package org-bullets
  :ensure t
  ; :init (setq org-bullets-bullet-list '("►" "◾" "◆"))
  :init (setq org-bullets-bullet-list '("●"))
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

(setq org-agenda-files
      '("~/Dropbox/org/inbox.org"
        "~/Dropbox/org/tickler.org"
        "~/Dropbox/org/todo.org"))

(setq org-log-into-drawer t)

(setq org-clock-into-drawer t)

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
                           ("~/Dropbox/org/someday.org" :maxlevel . 2)
                           ("~/Dropbox/org/media.org" :maxlevel . 2)
                           ))

(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-custom-commands
      '(("n" "seuraavat ja kesken" todo "SEURAAVA|KESKEN"
         ((org-agenda-overriding-header "Kesken olevat ja projektien seuraavat")))

        ("b" "blogit" todo ""
         ((org-agenda-files '("~/Dropbox/org/blogit.org"))
          (org-agenda-overriding-header "Blogitekstit")))

        ("j" "joskus" todo ""
         ((org-agenda-files '("~/Dropbox/org/someday.org"))))

        ("d" "päivä"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-overriding-header "Tänään")))
          (todo "KESKEN"
                ((org-agenda-overriding-header "Kesken olevat")))
          (todo "SEURAAVA"
                ((org-agenda-overriding-header "Projektien seuraavat")))
          (agenda "" ((org-agenda-span 1) (org-agenda-files '("~/Dropbox/org/media.org"))
                      (org-agenda-overriding-header "Media")))
          (agenda "" ((org-agenda-span 1) (org-agenda-files '("~/Dropbox/org/kirjasto.org"))
                      (org-agenda-overriding-header "Kirjasto")))))

        ("v" "viikko"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-overriding-header "Tällä viikolla")))
          (todo "KESKEN"
                ((org-agenda-overriding-header "Kesken olevat")))
          (todo "SEURAAVA"
                ((org-agenda-overriding-header "Projektien seuraavat")))
          (agenda "" ((org-agenda-span 7) (org-agenda-files '("~/Dropbox/org/media.org"))
                      (org-agenda-overriding-header "Media")))))

        ("p" "projektit" tags-todo "proj")
        ))
                                        ; Keys reserved for built-in commands are:
                                        ; a t T m M s S L C e / ? < > * #

(setq org-agenda-block-separator ?▰)

(setq org-agenda-show-inherited-tags t)

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

(defun tsl/org-timestamp ()
    (interactive)
    (if (org-at-heading-p)
        (progn (org-end-of-line)
               (org-return)
               (org-time-stamp nil)) 
      (org-time-stamp nil)))

(define-key org-mode-map (kbd "C-c .") 'tsl/org-timestamp)

(setq org-agenda-window-setup 'only-window)

(setq org-agenda-prefix-format
'((agenda . "%-10:c%-12t% s")
 (todo . "%-12:c%-12t")
 (tags . "%-12:c")
 (search . "%-12:c")))

(setq org-agenda-scheduled-leaders '("Sch: " "Sch.%2dx"))
(setq org-agenda-deadline-leaders '("DL: " "In.%3d: " "%2d ago: "))
