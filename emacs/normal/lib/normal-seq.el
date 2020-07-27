(require 'seq)

(defun seq-find-by-first-val (first-val seq)
  (let ((p #'(lambda (list)
	       (and (listp list)
		    (eq first-val (car list))))))
    (seq-find p seq)))

(provide 'normal-seq)
