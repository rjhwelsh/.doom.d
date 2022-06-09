;; Compatibility file to load literate configuration

;; Dummy functions for rjh
(defun rjh/use (&rest c) nil)

;; Currently used config
(defvar rjh-old-init-dir (concat doom-private-dir "literate/"))

;; Config for org-mode
(defvar rjh-org-mode-config-list
  (append
	  (mapcar
	   (lambda (n) (concat rjh-old-init-dir n ".org"))
	   '(
	     "rjh/solar"
	     ))))

;; The following steps are required outside of Emacs
;; Symlink ~/.emacs.doom/org          -> ~/domestic/static/org-local/
;; (after! org
;;   	;; Load literate configuration
;; 	(mapc (lambda (f) (and
;;                            (file-exists-p f)
;;                            (org-babel-load-file f)))
;;               rjh-org-mode-config-list)
;;         )
;; Email
;;
;; :email mu4e
;; (after! mu4e
;; 	(org-babel-load-file (concat rjh-old-init-dir    "djcb/mu4e.org"))
;; 	)



;; --------------------------------------------------------------------------------

;; Files

;; :emacs files
;; (How emacs handles files, backups, buffers, etc)
(org-babel-load-file (concat rjh-old-init-dir "emacs/files.org"))

;; Function to move files in Emacs
(org-babel-load-file (concat rjh-old-init-dir "zck.me/emacs-move-file.org"))
