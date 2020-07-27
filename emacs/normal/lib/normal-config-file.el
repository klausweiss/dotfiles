(defun find-config-file ()
  (interactive)
  (find-file user-init-file))

(defun reload-config ()
  (interactive)
  (load-file user-init-file))

(provide 'normal-config-file)
