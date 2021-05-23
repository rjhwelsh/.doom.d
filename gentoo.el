;;; gentoo.el -*- lexical-binding: t; -*-

;; Gentoo specific configuration options for Emacs

;; :os gentoo
;; https://github.com/rolandwalker/anaphora
(use-package! anaphora :load-path "/usr/share/emacs/site-lisp/anaphora")

;; https://gitlab.com/akater/elisp-akater-misc (causing tramp to hang)
;; https://gitlab.com/akater/elisp-akater-sh
;; https://gitlab.com/akater/elisp-file-tree
(use-package! akater-misc :load-path "/usr/share/emacs/site-lisp/akater-misc")
(use-package! akater-sh   :load-path "/usr/share/emacs/site-lisp/akater-sh")
(use-package! file-tree   :load-path "/usr/share/emacs/site-lisp/file-tree")

(use-package! akater-conf :load-path "/usr/share/emacs/site-lisp/akater-conf")
					;
;; https://gitlab.com/akater/emacs-gentoo-cache
(use-package! gentoo-cache
	      :load-path "/usr/share/emacs/site-lisp/gentoo-cache"
	      :commands (gentoo-cache-get-package-names)
	      :init
	      (defun gentoo-cache-get-packages (&rest eix-args)
		"Returns a list packages (category/name) returned by querying eix with EIX-ARGS."
		(let (result)
		  (with-temp-buffer
		    (save-excursion
		      (insert ?\()
		      (shell-command (eval `(sh-wrap
					     ((:eix-limit 0)
					      ,(append
						(list 'eix :pure-packages :format "'\"<category>/<name>\"\\n'")
						eix-args))))
				     (current-buffer))

		      (goto-char (point-max))
		      (insert ?\))
		      (goto-char (point-min)))
		    (setq result (read (current-buffer))))
		  result)))

;; https://gitlab.com/akater/emacs-ebuild-tools
(use-package! ebuild-tools
	      :load-path "/usr/share/emacs/site-lisp/ebuild-tools"
	      :commands
	      (
	       ebuild-tools-goto-ebuild goto-ebuild
	       ebuild-tools-fork-ebuild fork-ebuild
	       ebuild-tools-diff-ebuilds diff-ebuilds
	       )
	      :custom (ebuild-tools-repositories-dir "/var/db/repos/")
	      :config
	      (require 'gentoo-cache) ;; Work around missing dependency
	      (defalias 'goto-ebuild 'ebuild-tools-goto-ebuild)
	      (defalias 'fork-ebuild 'ebuild-tools-fork-ebuild)
	      (defalias 'diff-ebuilds 'ebuild-tools-diff-ebuilds)
	      )

;; :email mu4e
;; Bookmark
(after! mu4e
  (require 'gentoo-cache)
  (setq rjh/gentoo-keywords
	(let* ((plist (gentoo-cache-get-packages :installed))
	       ;;(plist (nthcdr (- (length plist0) 100) (reverse plist0)))
	       )
	  (append
	   ;; System updates
	   '("amd64")

	   ;; World packages
	   (delete-dups
	    (sort
	     (seq-filter
	      (lambda (elt) (> (length elt) 3))
	      (flatten-list
	       (mapcar (lambda (A)
		         (let* ((pc (split-string A "/"))
			        (p (cadr pc))
			        (c (car pc)))
			   A)
		         )
		       plist))
	      )
	     'string<
	     )))))
  ;; Set dynamic-bookmark-define
  ;; Which becomes static after being called
  (let ((desc "Gentoo highlights")
	(key ?g))
    (mu4e-bookmark-define
     `(lambda ()
	(let* ((sep "\\([^a-zA-Z0-9]|$|^\\)")
	       (query
	        (concat
		 " list:/gentoo/ AND "
		 " not flag:trashed AND "
		 (concat "/"
			 (mapconcat 'identity
				    (mapcar
				     ;; 'identity
				     (lambda (keyword)
				       (concat sep
					       keyword
					       sep
					       ))
				     rjh/gentoo-keywords)
				    "|")
			 "/"
			 ))
	        ))
	  ;; Dyanmically redefine bookmark here
	  ;; (mu4e-bookmark-define query ,desc ,key)
	  query))
     desc
     key))
  ;; Search pattern to find all matching words to gentoo-packages in current buffer.
  (defun rjh/gentoo-keywords-hl ()
    "Highlight all mentions of a world package name in the current buffer"
    (interactive)
    (unhighlight-regexp t)
    ;; Only highlight if the last query contains the keyword "gentoo"
    (if (and mu4e~headers-last-query (string-match "gentoo" mu4e~headers-last-query))
	(dolist (r rjh/gentoo-keywords-re)
	  (highlight-regexp r 'mu4e-highlight-face))))
  (add-hook 'mu4e-view-mode-hook 'rjh/gentoo-keywords-hl)
  ;; A list of regexps
  (setq
   rjh/gentoo-keywords-re
   (mapcar
    (lambda (p)
      (rx
       (seq
	(not word)
	(literal p)
	(not word)
	)))
    rjh/gentoo-keywords))
  )
