(provide 'find-config-file)
(provide 'reload-config)

(defun find-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))