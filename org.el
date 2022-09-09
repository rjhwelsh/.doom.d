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

;; Remove org-id from roam files
(after! org-roam
  (add-hook! 'org-mode-hook
    (add-hook 'before-save-hook
              (lambda ()
                (when  (org-roam-buffer-p) (org-map-entries '(org-delete-property "ID") nil 'file)))
                nil 'local))
  )

;; Add project capture template to org-roam
(after! org-roam
  (setq org-roam-capture-templates
        (append org-roam-capture-templates
                '(
                  ("p" "project" plain
                   (file "template/org-roam/project.txt")
                   :target (file "%<%Y%m%d%H%M%S>-${slug}.org") :unnarrowed t)
                  ))
        ))

;; Automatically created timestamps in headers
(after! org
  (defvar org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

  (defun org-set-created-property (&optional active name mod)
    "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified.

If given the `mod' argument, use the file's modification time.
"
    (interactive)
    (let* ((created (or name org-created-property-name))
           (fmt (if active "<%s>" "[%s]"))
           (modtime (when mod (file-attribute-modification-time (file-attributes (buffer-file-name)))))
           (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M" modtime))))
      (unless (org-entry-get (point) created nil)
        (org-set-property created now))))

  ;; (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)
  (add-hook 'org-insert-heading-hook #'org-set-created-property)

  ;; Add creation times for any missed items
  (add-hook! 'org-mode-hook
    (add-hook 'before-save-hook
              (lambda () (org-map-entries '(org-set-created-property) nil 'file))
               nil 'local))
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

        ;; Set time format for durations
        (setq org-duration-format 'h:mm)

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

;; Org-mode table functions 220714
(after! org
  (defun org-table-get-buffer-remote-range-eval (name-or-id form &optional buffer )
    "Get a field value or a list of values in a range from table at ID, in BUFFER.
     This function acts as a wrapper to `org-table-get-remote-range' to obtain table values from an alternate buffer.
     The result will be stripped of font decoration and fed to `read', so be careful out there."
    (when buffer
      (with-current-buffer buffer
        (read
         (substring-no-properties
          (org-table-get-remote-range
           name-or-id
           ; If form does not start with a reference, interpret as a var
           (if (string-match "^[a-zA-Z]" form)
               (format "$%s" form)
               form)
           ))))))

  (defun org-table-get-buffer-constant (name &optional buffer)
    "Obtain the value of an `org-mode' constant with NAME in BUFFER.
See `org-table-formula-constants-local'."
    (when buffer
      (with-current-buffer buffer
        (alist-get name
                   org-table-formula-constants-local "?" nil 'equal))))
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

;; Org link parameters
(after! org
  (cond
   ((eq system-type 'windows-nt)
    (let
        ((org-file-apps-pdf-browser "\"C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe\""))
        (setq org-file-apps
              `(
                ("\\.pdf::\\([0-9]+\\)?\\'" . ,(format "%s file:///%%s#page=%%1" org-file-apps-pdf-browser))
                ("\\.org.*\\'" . emacs) ;; open org-files natively
                ("\\.[a-z]*\\'" . system)
                (auto-mode . system)
                (directory . system)
                ))
        ;; Revizto5 is a issue tracking software
        (org-link-set-parameters
         "revizto5"
         :follow (lambda (issue_uri)
                   (let (
                         (org-link-revizto5-path "C:\\Program Files\\Vizerra LLC\\Revizto5\\Service\\ReviztoServiceGUI.exe")
                         ;; TODO implement windows to posix path conversion (string-match)
                         ;; - C: => /c/
                         ;; \\ => /
                         ;;   => \ (spaces)
                         (org-link-revizto5 "/c/Program\\ Files/Vizerra\\ LLC/Revizto5/Service/ReviztoServiceGUI.exe"))
                     (start-process-shell-command "revizto5" "*revizto5-issue*" org-link-revizto5 "-browser_protocol" (format "revizto5:%s" issue_uri)))))
        ))
   (t
    (setq org-file-apps
          '((auto-mode . emacs)
            (directory . emacs)
            ("\\.x?html?\\'" . "firefox %s")
            ("\\.pdf\\'" . "evince \"%s\"")
            ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
            ("\\.mm\\'" . default)))))
  )


;; From literate/org/org.org

(after! org
  ;; Base config
  (require 'org-agenda)
  (require 'ps-print)

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
