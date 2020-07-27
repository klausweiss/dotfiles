(require 'normal-autoload)

(autoload-all "../lib/swiper/swiper"
	      #'swiper-isearch
	      #'swiper-isearch-thing-at-point
	      )
(autoload-all "../lib/visual-regexp"
	      #'vr/replace
	      )

(defun swiper-isearch-with-current-region ()
  (interactive)
  (let ((swiper-function (if (use-region-p)
			     (swiper-isearch-thing-at-point)
			   (swiper-isearch)))
	(call-interactively swiper-function))))


(defun vr/replace-in-buffer ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'vr/replace)
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'vr/replace)))
  )

(setq vr/default-replace-preview t)


(provide 'normal-search)
