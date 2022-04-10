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

;; From literate/org/org.org

(after! org
  ;; Base config
  (require 'org-agenda)
  (require 'ps-print)

  (cond
   ((eq system-type 'windows-nt)
        (setq org-file-apps
          '((auto-mode . system)
            (directory . system)
            )))
   (t
    (setq org-file-apps
          '((auto-mode . emacs)
            (directory . emacs)
            ("\\.x?html?\\'" . "firefox %s")
            ("\\.pdf\\'" . "evince \"%s\"")
            ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
            ("\\.mm\\'" . default)))))

  ;; Behaviour
     (setq org-log-done 'time)   ;; Record when a task moves to the DONE state
     (setq org-log-refile 'time) ;; Record when an item is refiled.

  ;; Keybindings
     (define-key global-map "\C-cnl" 'org-store-link)
     (define-key global-map "\C-cna" 'org-agenda)
     (define-key global-map "\C-cnn" 'org-capture)
     (define-key global-map (kbd "C-c C-x C-j") 'org-clock-goto)
     (define-key global-map (kbd "C-c C-x C-o") 'org-clock-out)
     (define-key global-map (kbd "C-c C-x C-i") 'org-clock-in)

  ;; Hooks
     (add-hook 'org-mode-hook 'flyspell-mode)
     (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

  ;; align tags before saving org-mode file
  ;; TODO Cleanup hook
     (add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook (lambda () (org-align-tags t)) nil 'local)))


  ;; Extension modules
    (eval-after-load 'org
      '(org-load-modules-maybe t))
    ;;
     (require 'org-checklist) ;; Resets check-boxes with =RESET_CHECK_BOXES= set to =t=

     ;; super memo based repetition scheduling
     (require 'org-learn)
     (define-key org-mode-map "\C-cm" 'org-smart-reschedule)

     ;; quick templates
     (require 'org-tempo)
     (setq org-structure-template-alist
           (append
            org-structure-template-alist
            '(
              ("el" . "src emacs-lisp")
              ("py" . "src python")
              ("sh" . "src sh"))
            ))
     ;; clocking
     (require 'org-clock)
     (setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)

     (require 'org-capture)
     (require 'org-archive)

     (require 'org-id)
     (setq org-id-link-to-org-use-id 'use-existing) ;; use ids if available, prevent proliferation of ids if not

     ;;(require 'org-babel)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)  ;; This includes support for C++
        (emacs-lisp . t)
        (ruby . t)
        (dot . t)
        (gnuplot . t)
        (plantuml . t)
        (R . t)
        ))
     (setq org-confirm-babel-evaluate nil)
     (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
     (setq org-src-fontify-natively t)
     (setq org-src-tab-acts-natively t)
     (add-hook 'org-babel-after-execute-hook
               (lambda ()
                 (when org-inline-image-overlays
                   (org-redisplay-inline-images))))

     ;; export settings
     (require 'ox-html)
     (setq org-html-checkbox-type 'html) ;; render checkboxes in html
     (setq org-export-with-smart-quotes t)

     ;; icalendar export settings
     (require 'ox-icalendar)
     (require 'icalendar)
     (setq org-icalendar-use-scheduled '(event-if-todo)
           org-icalendar-use-deadline  '(event-if-todo todo-due)
           org-icalendar-alarm-time 40
           icalendar-export-sexp-enumerate-all t
           )
  )
