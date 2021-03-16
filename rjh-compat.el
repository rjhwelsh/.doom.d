;; Compatibility file to load literate configuration

;; Dummy functions for rjh
(defun rjh/use (&rest c) nil)

;; Currently used config
(defvar rjh-old-init-dir "~/domestic/static/emacs.d/init/")
(defvar rjh-old-private-dir "~/.emacs.rjh/private/")

(defvar rjh-old-init-config-list
  '(
    ;;"org/org"
    "joaotavora/yasnippet"
    "malabarba/aggressive-indent-mode"
    "git/magit"
    ;;"rjh/solar"
    "emacs/faces"
    "emacs/builtin"
    "alpaker/fill-column-indicator"
    "Fanael/rainbow-delimiters"
    "emacs/paren"
    "bbatsov/projectile"
    "justbur/which-key"
    "auto/electric-pair"
    "xahlee/xah-fly-keys"
    "emacs/files"
    "bbatsov/crux"
    "quelpa/quelpa"
    "skuro/plantuml-mode"
    "openscad/scad-mode"
    "company-mode/company-mode"
    "immerrr/lua-mode"
    "dengste/minimap"
    "emacs/display-line-numbers"
    "theme/theme-scheduler"
    "jaypei/emacs-neotree"
    "domtronn/all-the-icons"
    ;;"hlissner/emacs-doom-themes"
    ;;"theme/doom-solarized"
    "org/org-edna"
    "rjh/session_state"
    "wedler/emacs_session"
    "emacs/desktop"
    "emacswiki/frame-restore"
    "tlh/workgroups"
    "zck.me/emacs-move-file"
    "org/gtd"
    ;;"seagle0128/doom-modeline-conf"
    "jlumpe/ox-json"
    ))

(defvar rjh-old-private-config-list
  '(
    ;;"org/org"
    ;;"emacs/solar"
    ;;"rjh/solar"
    "my/contact"
    "emacs/auth"
    "emacs/games"
    ))


;; Config for org-mode
(defvar rjh-org-mode-config-list
  (append (mapcar
	   (lambda (n) (concat rjh-old-private-dir n ".org"))
	   '(
	     "org/org"
	     ;;"emacs/solar"
	     ;;"rjh/solar"
	     ))
	  (mapcar
	   (lambda (n) (concat rjh-old-init-dir n ".org"))
	   '(
	     "org/org"
	     "rjh/solar"
	     ;; "org/org-edna"
	     "org/gtd"
	     ))))

(after! org
	;; org-solar config
	(setq org-icalendar-with-timestamps t)
	;; lon/lat information in private.el
	;; Symlink required in ~/.emacs.doom/ to ~/domestic/static/emacs.d/

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
