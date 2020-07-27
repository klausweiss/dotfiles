(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'normal-behavior)
