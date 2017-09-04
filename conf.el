(defun tsl/org-timestamp ()
    (interactive)
    (if (org-at-heading-p)
        (progn (org-end-of-line)
               (org-return)
               (org-time-stamp nil)) 
      (org-time-stamp nil)))

(define-key org-mode-map (kbd "C-c .") 'tsl/org-timestamp)
