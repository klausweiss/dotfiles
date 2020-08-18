(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(cua-mode t)
(setq cua-prefix-override-inhibit-delay 0.01)

(provide 'normal-behavior)
