(provide 'load-directory)

(defun load-directory (dir &optional recursive)
  (let* ((el-regex "^[^#].*\.el$")
	 (find-files-flat      (lambda (dir) (directory-files             dir 't el-regex)))
	 (find-files-recursive (lambda (dir) (directory-files-recursively dir    el-regex)))
	 (find-files           (if recursive find-files-recursive find-files-flat)))
    (mapc #'load-file (funcall find-files dir))))
