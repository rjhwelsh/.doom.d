;; Compatibility file to load literate configuration

;; Dummy functions for rjh
(defun rjh/use (&rest c) nil)

;; Currently used config
(defvar rjh-old-init-dir "~/domestic/static/emacs.d/init/")
(defvar rjh-old-private-dir "~/.emacs.rjh/private/")
(defvar rjh-old-user-emacs-directory "~/.emacs.rjh/")

;; Config for org-mode
(defvar rjh-org-mode-config-list
  (append (mapcar
	   (lambda (n) (concat rjh-old-private-dir n ".org"))
	   '(
	     "org/org"
	     ))
	  (mapcar
	   (lambda (n) (concat rjh-old-init-dir n ".org"))
	   '(
	     "org/org"
	     "rjh/solar"
	     "org/gtd"
	     ))))

;; The following steps are required outside of Emacs
;; Symlink ~/.emacs.doom/rjh          -> ~/domestic/static/emacs.d/
;; Symlink ~/.emacs.doom/agenda-files -> ~/.emacs.rjh/agenda-files
;; Symlink ~/.emacs.doom/org          -> ~/domestic/static/org-local/ 

(after! org
	;; org-solar config
	(setq org-icalendar-with-timestamps t)
	;; lon/lat information in private.el

	;; Load literate configuration
	(mapc (lambda (f) (org-babel-load-file f)) rjh-org-mode-config-list)

	;; Org-edna
	(use-package! "org-edna"
		      :init
		      (require 'org-edna)
		      (org-edna-mode)
		      (setq org-edna-use-inheritance t)
		      )
	)

;; :emacs files
;; (How emacs handles files, backups, buffers, etc)
(org-babel-load-file (concat rjh-old-init-dir "emacs/files.org"))

;; Function to move files in Emacs
(org-babel-load-file (concat rjh-old-init-dir "zck.me/emacs-move-file.org"))

;; :email mu4e
(after! mu4e
	(org-babel-load-file (concat rjh-old-init-dir    "djcb/mu4e.org"))
	(org-babel-load-file (concat rjh-old-private-dir "my/email.org"))
	(org-babel-load-file (concat rjh-old-private-dir "djcb/mu4e.org"))
	)

;; :os notify
;; https://www.emacswiki.org/emacs/notify.el
(use-package! notify
	      :defer t
	      :init
	      (add-to-list 'load-path (concat rjh-old-user-emacs-directory "/emacswiki/")))

;; --------------------------------------------------------------------------------

;; :org ox-json
;; (Exports org-files as json)
(after! org
        (require 'ox-json)) ;; CC-mode incompatible with emacs version

