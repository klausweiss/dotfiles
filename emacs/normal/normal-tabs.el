(require 'awesome-tab)

(with-eval-after-load 'awesome-tab
  (setq awesome-tab-height 120)
  (awesome-tab-mode t)

  ;; copied from tabbar-ruler.el
  (defvar awesome-tab-projectile-buffer-group-calc nil
    "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")
  (defun awesome-tab-projectile-buffer-groups ()
    "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
    (if awesome-tab-projectile-buffer-group-calc
	(symbol-value 'awesome-tab-projectile-buffer-group-calc)
      (set (make-local-variable 'awesome-tab-projectile-buffer-group-calc)

           (cond
            ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
            ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
            ((condition-case err
		 (projectile-project-root)
               (error nil)) (list (projectile-project-name)))
            ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
            ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
            ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
            ((memq major-mode '(dired-mode)) '("Dir"))
            (t '("Main"))))
      (symbol-value 'awesome-tab-projectile-buffer-group-calc)))

  (setq awesome-tab-buffer-groups-function #'awesome-tab-projectile-buffer-groups)
  )

(provide 'normal-tabs)
