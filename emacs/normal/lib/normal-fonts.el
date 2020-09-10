;; https://emacs.stackexchange.com/a/55936
(defconst normal-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?." "::" "&="
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    ;; ligatures that break stuff
    ;; "?:" vc/replace
    ))

(defun normal-disable-composition ()
  (setq-local auto-composition-mode nil))

(defun normal-fonts-enable-ligatures ()
  (dolist (pat normal-ligature-mode--ligatures)
    (set-char-table-range composition-function-table
			  (aref pat 0)
			  (nconc (char-table-range composition-function-table (aref pat 0))
				 (list (vector (regexp-quote pat)
                                               0
					       'compose-gstring-for-graphic)))))
  (add-hook 'magit-log-mode-hook #'normal-disable-composition)
  (add-hook 'ediff-mode-hook #'normal-disable-composition)
  )

(provide 'normal-fonts)
