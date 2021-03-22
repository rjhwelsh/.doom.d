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

        ;; idle time
        ;; https://orgmode.org/manual/Resolving-idle-time.html
        ;; TODO Add to literate configuration
        (setq org-clock-idle-time 15)

	;; Load literate configuration
	(mapc (lambda (f) (org-babel-load-file f)) rjh-org-mode-config-list)

	;; Org-edna
	(use-package! "org-edna"
		      :init
		      (require 'org-edna)
		      (org-edna-mode)
		      (setq org-edna-use-inheritance t)
		      )

        ;; Org-crypt
        ;; :emacs epa-file
        ;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
        (require 'epa-file)
        (epa-file-enable)

        ;; Decrypt entries with M-x org-decrypt-entry
        (use-package! "org-crypt"
          :config
          (org-crypt-use-before-save-magic)
          (add-to-list 'org-tags-exclude-from-inheritance "crypt")
          :custom
          (org-crypt-key "8CFC62043A386EBEE9E9E9EE467F89276759B9A1")
          (org-crypt-disable-auto-save 't))
        )

;; --------------------------------------------------------------------------------

;; :org ox-json
;; (Exports org-files as json)
(after! org
        (require 'ox-json))


;; --------------------------------------------------------------------------------

;; Curriculum Vitae Tools

;; :org cv
;; https://gitlab.com/Titan-C/org-cv

;; Fork of org-cv with awesome-cv support
;; :org cv
;; https://gitlab.com/zzamboni/org-cv/
;; https://github.com/posquit0/Awesome-CV
(after! org

  (require 'ox-hugo)
  (require 'ox-hugocv)

  (require 'ox-moderncv)
  (require 'ox-altacv)

  (require 'ox-awesomecv)
  )

;; Still TODO
;; https://www.aidanscannell.com/post/org-mode-resume/
;; https://github.com/aidanscannell/my-org-resume

;; :org ox-extra
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))

;; :org ox-latex
(after! org
  ;; Import ox-latex to get org-latex-classes and other funcitonality
  ;; for exporting to LaTeX from org
  (use-package! ox-latex
    :config
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))))

;; :org ox-koma-letter
;; Create beautiful letters using org-mode
;; https://orgmode.org/worg/exporters/koma-letter-export.html
(eval-after-load 'ox '(require 'ox-koma-letter))

;; --------------------------------------------------------------------------------

;; Email
;;
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

;; Files

;; :emacs files
;; (How emacs handles files, backups, buffers, etc)
(org-babel-load-file (concat rjh-old-init-dir "emacs/files.org"))

;; Function to move files in Emacs
(org-babel-load-file (concat rjh-old-init-dir "zck.me/emacs-move-file.org"))
