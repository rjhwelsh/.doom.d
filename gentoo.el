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
