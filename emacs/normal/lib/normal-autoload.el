(defun autoload-all (file &rest symbols)
  (dolist (s symbols)
    (autoload s file nil t)))

(provide 'normal-autoload)
