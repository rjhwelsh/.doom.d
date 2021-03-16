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
	      :init
	      (add-to-list 'load-path (concat rjh-old-user-emacs-directory "/emacswiki/")))

;; --------------------------------------------------------------------------------

;; :tools magit
(after! magit
	(map! "C-x g" #'magit-status))


;; rainbow delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package! "rainbow-delimiters"
	      :init
	      (require 'rainbow-delimiters)
	      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; :emacs paren
(show-paren-mode t)
(setq show-paren-style 'expression)
(electric-pair-mode)

;; :ui whichkey
(use-package! "which-key"
	      :after xah-fly-keys
	      :init
	      (require 'which-key)
	      (which-key-mode)
	      :hook (
		     (xah-fly-command-mode-activate . which-key-show-top-level)
		     (xah-fly-insert-mode-activate . which-key-show-top-level)
		     ))

;; :editor xah-fly-keys
(setq xah-fly-use-meta-key nil)
(setq xah-fly-use-control-key nil)
(setq xah-fly-use-isearch-arrows nil)

(use-package! "xah-fly-keys"
	      :bind (
		     ("<f5>" . xah-fly-insert-mode-activate)
		     ("<f6>" . xah-fly-command-mode-activate)

		     ;; Use M-x to activate command mode
		     ("M-x" . xah-fly-command-mode-activate)
		     )

	      :init
	      (require 'xah-fly-keys)

	      (xah-fly-keys-set-layout 'colemak)
	      (xah-fly-keys 1)

	      ;; Add highlight hooks
	      (defun my-highlight-line-on () (global-hl-line-mode 1))
	      (defun my-highlight-line-off () (global-hl-line-mode 0))

	      (xah-fly-command-mode-activate)

	      :hook (
		     (xah-fly-command-mode-activate . my-highlight-line-on)
		     (xah-fly-insert-mode-activate . my-highlight-line-off)))

;; :tools crux
(use-package! crux
	      :bind (
		     ;; ("C-c o" . crux-open-with)
		     ;; ("C-k" . crux-smart-kill-line)
		     ;; ("C-S-RET" . crux-smart-open-line-above)
		     ;; ("S-RET" . crux-smart-open-line)
		     ;; ("C-c n" . crux-cleanup-buffer-or-region)
		     ;; ("C-c f" . crux-recentf-find-file)
		     ;; ("C-c u" . crux-view-url)
		     ;; ("C-c e" . crux-eval-and-replace)
		     ;; ("C-x 4 t" . crux-transpose-windows)
		     ("C-c D" . crux-delete-file-and-buffer)
		     ;; ("C-c c" . crux-copy-file-preserve-attributes)
		     ;; ("C-c d" . crux-duplicate-current-line-or-region)
		     ;; ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
		     ;; ("C-c r" . crux-rename-file-and-buffer)
		     ;; ("C-c t" . crux-visit-term-buffer)
		     ;; ("C-c k" . crux-kill-other-buffers)
		     ;; ("C-M z" . crux-indent-defun)
		     ;; ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
		     ;; ("C-c I" . crux-find-user-init-file)
		     ;; ("C-c ," . crux-find-user-custom-file)
		     ;; ("C-c S" . crux-find-shell-init-file)
		     ;; ("Super-j" . crux-top-join-line)
		     ;; ("Super-k " . crux-kill-whole-line)
		     ;; ("C-Backspace" . crux-kill-line-backwards)
		     ;; ("C-c i" . crux-ispell-word-then-abbrev)
		     ;; ("C-x C-u" . crux-upcase-region)
		     ;; ("C-x C-l" . crux-downcase-region)
		     ;; ("C-x M-c" . crux-capitalize-region)
		     ))

;; :lang plantuml
(after! plantuml
	(add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))
	(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))
	(add-to-list
	 'org-src-lang-modes '("plantuml" . plantuml))
	;; Possible loadpaths for plantuml binary
	(setq plantuml-jar-path
	      (locate-file "plantuml.jar" 
			   '("~"
			     "/usr/share/plantuml/lib/"
			     "/usr/share/plantuml/")))
	(setq org-plantuml-jar-path plantuml-jar-path)
	(setq plantuml-default-exec-mode 'jar)
	;; Default output type
	(require 'plantuml-mode)
	(plantuml-set-output-type "svg")
	)

;; :lang lua
(after! lua-mode
	(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
	(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
	(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
	)

;; :ui minimap
(after! minimap
	(setq minimap-width-fraction 0.1)
	(setq minimap-minimum-width 15)
	(setq minimap-recenter-type 'middle)
	(setq minimap-window-location 'right)
	(setq minimap-mode t)
	)

;; :ui neotree
(after! neotree
	(map! "C-x d" #'neotree-toggle)
	(setq find-directory-functions '(neotree-dir)))

;; :org ox-json
;; (Exports org-files as json)
;; (require 'ox-json) ;; CC-mode incompatible with emacs version

;; :os auth
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(require 'auth-source)
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; :chat jabber
;; http://emacs-jabber.sourceforge.net/
;; https://www.emacswiki.org/emacs/JabberEl
(use-package! "jabber"
	      :after (dired-x notify) ;; dired-x overrides jabber-global-keymap prefix
	      :init
	      (require 'jabber-chatbuffer)
	      :config

	      ;; Enable history
	      (setq
	       jabber-history-enabled t
	       jabber-use-global-history nil
	       jabber-backlog-number 40
	       jabber-backlog-days 30
	       )

	      ;; Disable avatar images in roster "-%a"
	      (setq jabber-roster-line-format " %c %-25n %u %-8s  %S")

	      ;; Setup OS notifications
	      (defun notify-jabber-notify (from buf text proposed-alert)
		"(jabber.el hook) Notify of new Jabber chat messages via notify.el"
		(when (or jabber-message-alert-same-buffer
			  (not (memq (selected-window) (get-buffer-window-list buf))))
		  (if (jabber-muc-sender-p from)
		      (notify (format "(PM) %s"
				      (jabber-jid-displayname (jabber-jid-user from)))
			      (format "%s: %s" (jabber-jid-resource from) text)))
		  (notify (format "%s" (jabber-jid-displayname from))
			  text)))

	      (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

	      ;; :bind-keymap ("C-x C-j" . jabber-global-keymap)
	      :bind (:map jabber-chat-mode-map
			  ("RET" . newline)
			  ("<C-return>" . jabber-chat-buffer-send))
	      )

;; :os gentoo
;; https://github.com/rolandwalker/anaphora
(use-package! anaphora :load-path "/usr/share/emacs/site-lisp/anaphora")

;; https://gitlab.com/akater/elisp-akater-misc
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
