;;; workarounds.el -*- lexical-binding: t; -*-

;; A place to keep workarounds, which can be messy looking

(after org!
       ;; fix <> parenthesis matching
             ;; Taken from: https://emacs.stackexchange.com/questions/50216/org-mode-code-block-parentheses-mismatch
      (defun org-mode-<>-syntax-fix (start end)
	"Change syntax of characters ?< and ?> to symbol within source code blocks."
	(let ((case-fold-search t))
	  (when (eq major-mode 'org-mode)
	    (save-excursion
	      (goto-char start)
	      (while (re-search-forward "<\\|>" end t)
		(when (save-excursion
			(and
			 (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
			 (string-equal (match-string 1) "begin")))
		  ;; This is a < or > in an org-src block
		  (put-text-property (point) (1- (point))
				     'syntax-table (string-to-syntax "_"))))))))

      (defun org-setup-<>-syntax-fix ()
	"Setup for characters ?< and ?> in source code blocks.
			      Add this function to `org-mode-hook'."
	(setq syntax-propertize-function 'org-mode-<>-syntax-fix)
	(syntax-propertize (point-max)))

      (add-hook 'org-mode-hook #'org-setup-<>-syntax-fix)

       )
