(defun normal-yafolding-hide-element-or-parent ()
  (interactive)
  (if (not (yafolding-hide-element))
      (yafolding-hide-parent-element)))

(provide 'normal-yafolding)
