(defun normal-close-window-with-buffer ()
  (interactive)
  (if (>= 1 (length (get-buffer-window-list)))
      (kill-buffer-and-window)
    (delete-window)))

(provide 'normal-buffers)
