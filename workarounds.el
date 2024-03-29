;;; workarounds.el -*- lexical-binding: t; -*-

;; A place to keep workarounds, which can be messy in the main config

;; Keyboard tweaks on chromebook
(defun my-keyboard-tweaks ()
  "Tweak keyboard using system-utils to optimize for Emacs, (hard-coded) based on the system host-name"
  (interactive)
  (when (and (eq system-type 'gnu/linux) (equal system-name "lethe"))
    ;; Warning! Synchronous process
    (call-process "/usr/bin/xmodmap" nil "*proc*" nil "-v" "-e" "keycode 133 = Home")
    ))
(my-keyboard-tweaks)

;; Define and fix non-existent function in < emacs-28
(defun native-comp-available-p nil)

;; Redefine play-sounds to use a separate emacs-instance
(define-advice play-sound-file
    ( :override (file &optional volume device) parallel-emacs-play-sound-file)
  "Uses another emacs-instance to play sounds, concurrently."
  (start-process "sound" "*sound*" "emacs" "-q" "-nl" "-nw" "--batch" "--eval" (format "(play-sound-file %S %S %S)" file volume device)))

(after! org
  ;; Disable cache -- too many warnings!
  (setq org-element-use-cache nil)
       ;; fix <> parenthesis matching
             ;; Taken from: https://emacs.stackexchange.com/questions/50216/org-mode-code-block-parentheses-mismatch
      (defun org-mode-<>-syntax-fix (start end)
	"Change syntax of characters ?< and ?> to symbol within source code blocks."
	(let ((case-fold-search t))
	  (when (eq major-mode 'org-mode)
	    (save-excursion
	      (goto-char start)
	      (while (re-search-forward "<\\|>" end t)
		(when (save-excursion
			(and
			 (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
			 (string-equal (match-string 1) "begin")))
		  ;; This is a < or > in an org-src block
		  (put-text-property (point) (1- (point))
				     'syntax-table (string-to-syntax "_"))))))))

      (defun org-setup-<>-syntax-fix ()
	"Setup for characters ?< and ?> in source code blocks.
			      Add this function to `org-mode-hook'."
	(setq syntax-propertize-function 'org-mode-<>-syntax-fix)
	(syntax-propertize (point-max)))

      (add-hook 'org-mode-hook #'org-setup-<>-syntax-fix)
      (require 'elec-pair)
      (add-hook
       'org-mode-hook
       (lambda ()
         (setq-local electric-pair-inhibit-predicate
                     `(lambda (c)
                        (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

      ;; Workaround for capture templates not working after calling org-agenda
      (setq org-agenda-finalize-hook nil)
       )
