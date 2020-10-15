(require 'elec-pair)

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq cua-prefix-override-inhibit-delay 0.01
      cua-remap-control-v nil
      cua-remap-control-z nil
      )
(cua-mode t)

(electric-pair-mode)

;; customization
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'no-error)

;; system clipboard interaction

(require 'simpleclip)
(setq simpleclip-less-feedback t)
(simpleclip-mode 1)

(defun cua-cut-region (arg)
  "Cut the region and copy to the kill ring.
With numeric prefix arg, copy to register 0-9 instead."
  (interactive "P")
  (setq cua--last-killed-rectangle nil)
  (if buffer-read-only
      (cua-copy-region arg)
    (setq arg (cua--prefix-arg arg))
    (let ((start (mark)) (end (point)))
      (or (<= start end)
	  (setq start (prog1 end (setq end start))))
      (cond
       (cua--register
	(copy-to-register cua--register start end t 'region))
       ((eq this-original-command 'clipboard-kill-region)
	(clipboard-kill-region start end 'region))
       (t
	(simpleclip-cut start end))))  ;; this was changed
    (cua--deactivate)))

(defun cua-copy-region (arg)
  "Copy the region to the kill ring.
With numeric prefix arg, copy to register 0-9 instead."
  (interactive "P")
  (setq arg (cua--prefix-arg arg))
  (setq cua--last-killed-rectangle nil)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (cond
     (cua--register
      (copy-to-register cua--register start end nil 'region))
     ((eq this-original-command 'clipboard-kill-ring-save)
      (clipboard-kill-ring-save start end 'region))
     (t
      (simpleclip-copy start end)))  ;; this was changed
    (if cua-keep-region-after-copy
	(cua--keep-active)
      (cua--deactivate))))

(provide 'normal-behavior)
