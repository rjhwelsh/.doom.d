;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Improve garbage collection
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
;; See also,
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#avoid-garbage-collection-at-startup

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(load! "private.el")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat doom-private-dir "org/"))
(setq org-agenda-files (concat org-directory "agenda-files"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "rjh-compat.el")
(when (string-match "gentoo" (shell-command-to-string "uname -a"))
  (load! "gentoo.el"))

;; --------------------------------------------------------------------------------
;; :emacs tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; :emacs calendar
(calendar-set-date-style 'european)

;; :os auth
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(require 'auth-source)
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; :emacs paren
(show-paren-mode t)
(setq show-paren-style 'expression)
(electric-pair-mode)

;; :pretty rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package! "rainbow-delimiters"
	      :init
	      (require 'rainbow-delimiters)
	      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  ;; (map! "C-x d" #'neotree-toggle)
	(setq find-directory-functions '(neotree-dir)))

;; :ui whichkey
(use-package! "which-key"
	      :after xah-fly-keys
	      :init
	      (require 'which-key)
	      (which-key-mode)
              )

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

;; :emacs crux
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
	      (require 'notify)
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

