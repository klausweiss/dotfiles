(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup")))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package flx)
