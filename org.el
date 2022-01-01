;;; org.el -*- lexical-binding: t; -*-

;; org-mode configuration

;; From config.el
(setq org-agenda-files (concat org-directory "agenda-files"))
;; Reset org-babel-after-executed-hook (fixes redisplay issues when inlineimages are off)
;; Redisplay images is added in :lang org default config.
(after! org
  (setq org-babel-after-execute-hook nil)
  ;; :lang org
  (setq org-clock-sound (concat doom-private-dir "sounds/86773__juskiddink__gong.wav"))
  ;; export options
  (setq org-export-with-broken-links t)
  (setq org-export-use-babel nil)
  )

(after! org-roam
  ;; :lang org +roam2
  ;; Set graph direction for org-roam (v2)
  (setq org-roam-graph-extra-config '(("rankdir" . "LR")))
  )


;; From rjh-compat.el
(after! org
	;; org-solar config
	(setq org-icalendar-with-timestamps t)
	;; lon/lat information in private.el

        ;; idle time
        ;; https://orgmode.org/manual/Resolving-idle-time.html
        ;; TODO Add to literate configuration
        (setq org-clock-idle-time 15)

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
