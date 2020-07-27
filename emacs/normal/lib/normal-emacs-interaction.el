;; https://stackoverflow.com/a/10166400/3103257
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defvar font-size-at-startup (face-attribute 'default :height))

(defun change-font-size (x)
  (let ((current-font-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-font-size x))))

(defun increase-font-size ()
  (interactive)
  (change-font-size 10))

(defun decrease-font-size ()
  (interactive)
  (change-font-size -10))

(defun reset-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height font-size-at-startup))

(setq help-window-select t)

(provide 'normal-emacs-interaction)
