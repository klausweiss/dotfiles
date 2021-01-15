(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(cl-decf n)))))

(defun duplicate-current-region ()
  (interactive)
  (let* ((start (region-beginning))
	 (end (region-end))
	 (text (buffer-substring start end)))
    (goto-char end)
    (insert text)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun duplicate-current-region-or-line ()
  "Duplicate current region if selected, current line otherwise."
  (interactive)
  (if (use-region-p)
      (call-interactively #'duplicate-current-region)
    (call-interactively #'duplicate-current-line)))

;; https://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we are not at the end of the line,
  then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun newline-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun newline-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line))

(defun join-next-line ()
  (interactive)
  (next-line)
  (delete-indentation))

(provide 'normal-text-manipulation)
