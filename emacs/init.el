(require 'seq)
(require 'sort)

(defun directories-with-elisp-files (dir)
  (let* ((elisp-files (directory-files-recursively dir "\\.elc?$" t))
	 (directories-with-elisp-files (mapcar #'file-name-directory elisp-files)))
    (delete-dups directories-with-elisp-files)))

(defun emacs.d-path (p) (concat user-emacs-directory p))

(if (not (fboundp 'flatten-list))
    (defun flatten-list (list-of-lists) (apply #'append list-of-lists)))

(let* ((subdirectories-names '("lib" "themes" "normal"))
       (subdirectories (mapcar #'emacs.d-path subdirectories-names))
       (directories-to-add (flatten-list (mapcar #'directories-with-elisp-files subdirectories)))
       )
  (dolist (path directories-to-add)
    (add-to-list 'load-path path))
  )

(require 'normal-behavior)
(require 'normal-completion)
(require 'normal-interface)
(require 'normal-ivy)
(require 'normal-lsp)
(require 'normal-magit)
(require 'normal-menu)
(require 'normal-project-integration)
(require 'normal-search)
(require 'normal-smart-shift)
(require 'normal-tabs)
(require 'normal-undo)
(require 'normal-which-key)
(require 'normal-winum)

(require 'normal-keymap)
