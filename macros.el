;;; $DOOMDIR/macros.el -*- lexical-binding: nil; -*-

;; These are user-defined macros/functions to enhance the functionality of emacs

;; org
(after! org
  ;; Load outlook extensions
  (load! (expand-file-name "ext/org-outlook.el" doom-private-dir))
  (require 'org-outlook)

  (defun org-toggle-reset-check-boxes-property ()
    (interactive)
    (let* ((prop "RESET_CHECK_BOXES")
           (value (org-entry-get nil prop)))
      (if value
          (org-delete-property prop)
        (org-entry-put nil "RESET_CHECK_BOXES" "t"))))
  (define-key org-mode-map (kbd "C-c C-x r") 'org-toggle-reset-check-boxes-property)

  (define-key org-mode-map "\C-cne" 'rjh/org-tags-expire)
  (define-key org-agenda-mode-map "\C-ce" 'rjh/org-tags-expire-agenda)

  ;; Function to return org-buffer-files
  (defun ixp-org-buffer-files ()
    "Return list of opened orgmode buffer files"
    ;; org-refile functions must remove nil values
    (delete nil
            (mapcar (function buffer-file-name)
                    (org-buffer-list 'files))))

  ;; Create a hook variable to execute before the =org-refile= command using advice
  (define-advice org-refile (:before (orig-fn &rest args))
    "Add `org-before-refile-hook' to `org-refile'."
    (run-hooks 'org-before-refile-insert-hook))
  (defvar org-before-refile-insert-hook nil
    "Hook run before `org-refile' has started to execute.")

  ;; Functions to refile to parent headline
  (require 'org-id)
  (defun org-id-refile-to-prev ()
    "Uses `org-id-find' to find the parent of entry-at-point,
then refiles the entry back to it's parent."
    (interactive)
    (let* ((PPID (org-entry-get nil "PPID")) ;; Get property value at point
           (loc (if (string-empty-p PPID)
                    (progn (message "PPID is empty!") nil)
                  (org-id-find PPID)          ;; Find location of org-id
                  )))
      ;; The refile location, *RFLOC* should be of the form ='(nil filename nil position)=
      (when loc
        (org-refile nil nil
                    (list nil (car loc) nil (cdr loc))))))  ;; RFLOC
  (define-key org-mode-map "\C-c\M-r" 'org-id-refile-to-prev)
  (defun org-set-ppid-to-current ()
    "Sets :PPID: to the current parent's `org-id'"
    (let (ppid (org-id-get))
      (when ppid
        (org-entry-put                   ;; Set property value
         nil
         "PPID"		             ;; PROPERTY
         (save-excursion	             ;; VALUE
           (ignore-errors           ;; Catch error whilst..
             (outline-up-heading 1 t)  ;; ... Going up a headline
             (org-id-get))                ;; Obtain org-id
           )))))
  (add-hook 'org-before-refile-insert-hook 'org-set-ppid-to-current)  ;; Set current parent's id (before refile)

  ;; Function to count the number of entry matches
  (defun org-count-entries (&optional pom match scope)
  "Counts the items at POM, based on MATCH and within SCOPE
   Default behaviour is to count all the todo items at point within the subtree.
   (This includes the subtree itself)"
  (interactive)
  (save-excursion
    (and pom (goto-char pom))
    (let ((counter 0))
      (org-map-entries
       (lambda ()
         (setq counter (1+ counter)))
       (or match "/!")
       (or scope 'tree))
      counter)))

  ;; Function to extract keywords from org-todo-keywords
  (defun org-todo-keywords-extract (&optional n  todo-keywords)
    "Extract todo keywords from the `org-todo-keywords' variable.
   Optionally, specify sequence to extract.
   Optionally, specify todo-keywords variable."
    (let ((todo-keywords
           (mapcar
            (lambda (sequence)
              (let* ((keywords (cdr sequence)))
                (org-remove-keyword-keys keywords)
                ))
            (or todo-keywords org-todo-keywords))))
      (if n
          (nth n todo-keywords)
        todo-keywords)))

  ;; Function to reassign todo keywords according to association list
(defun org-reassign-todo-keywords (swap-alist &optional match scope)
  "Re-assign todo keywords according to SWAP-ALIST,
Organized as an association list '((oldkey1 newkey1) (oldkey2 newkey2) ...).
Scope and match are passed to `org-map-entries'"
  (org-map-entries
   (lambda ()
     (let*
         (
          (oldkey (org-get-todo-state))
          (oldkey (when oldkey (substring-no-properties oldkey)))
          (newkey (cadr (assoc oldkey swap-alist)))
          (newkey (and
                   oldkey
                   (not (string-equal oldkey newkey))
                   newkey)))
       (when newkey (org-todo newkey))))
   (or match "/!")
   (or scope)))

(load! (expand-file-name "ext/org-tags-expire.el" doom-private-dir))
(load! (expand-file-name "ext/org-score.el" doom-private-dir))
)
;; org-agenda
(after! org-agenda

  (defun org-agenda-filename-to-export-views (filename exts)
    "Returns a standard location to export agenda views to"
    (progn
      (mapcar
       (lambda (x)
         (expand-file-name
          (concat filename "." x)
          org-directory))
       exts)))

  (defun org-agenda-shuffle-entries ()
    "Assigns (or reassigns) a shuffle number to every agenda item."
    (interactive)
    (org-map-entries
     '(org-set-property "SHUFFLE" (number-to-string (random most-positive-fixnum)))
     "/!" ;; Select all todo items only
     'agenda))

  (load! (expand-file-name "ext/org-agenda-skip.el" doom-private-dir))
  )


;; org-roam
(after! org-roam

  (defun org-roam-capture-subtree (&optional n)
    "Cuts the current subtree, and pastes it into an org-roam buffer.
The title of the subtree is used as the title of the org-roam buffer.
Using a prefix argument (other than 1) will open a capture buffer before saving."
    (interactive "p")
    (unwind-protect
        (let* (
               (immediate (= n 1))
               (subtree-title (save-excursion
                                (org-back-to-heading)
                                (org-element-property :title (org-element-at-point))))
               (subtree-id (save-excursion
                             (org-back-to-heading)
                             (org-element-property :ID (org-element-at-point))))
               (subtree-beg (save-excursion
                              (org-back-to-heading)
                              (point)))
               ;; Create node based on subtree properties
               (node (org-roam-node-create :title subtree-title :id subtree-id ))
               (templates '(
                            ("s" "subtree" plain "%?%(org-paste-subtree 0)" :target
                             (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                             :unnarrowed t)))
               ;; Remove side-effects from org-capture
               (org-capture-prepare-finalize-hook nil)
               org-roam-capture-props
               )

          (org-delete-property "ID")  ;; Delete ID from subtree
          (org-copy-subtree 1 nil 't) ;; copy subtree to kill ring

          ;; interupt kill-ring to prevent text accumulation
          (delete-region
           (save-excursion
             (org-back-to-heading)
             (when (org-roam-file-p) (org-beginning-of-line)) ;; pos link
             (point))
           (progn
             (org-end-of-subtree t t)
             (when (org-roam-file-p) (backward-char)) ;; pos link
             (point)))

          ;; Synchronize org-roam database to prevent db violations
          (when (org-roam-file-p)
            (save-buffer)
            (org-roam-db-sync)
            ;; Insert a link if inside an org-roam buffer
            (setq org-roam-capture-props
                  (list
                   :insert-at (point-marker)
                   :link-description subtree-title
                  :finalize 'insert-link))
        (insert (org-link-make-string
                 (concat "id:" subtree-id)
                 subtree-title)))

      (org-roam-capture-
       :node node
       :templates templates
       :props (append
               (list
                :immediate-finish immediate)
               org-roam-capture-props))

      ;; Re-position for next heading
      (when (org-roam-file-p) (outline-next-heading)))))

(map! (:map org-mode-map)
      :localleader
      :prefix ("m" . "org-roam")
      "s" #'org-roam-capture-subtree))

;; --------------------------------------------------------------------------------
(defun insert-date-md5sum-hex ()
  "Inserts an md5sum of the current date into buffer, using the shell."
  (interactive)
  (let* ((cmd
         (format "date|md5sum"))
         (proc
          (with-temp-buffer
            (list :exit-status (call-process-shell-command cmd nil t) ;; call shell command
                  :output (car (split-string (buffer-string))) ;; limit to first word of output
            )))
         )
    (if (eq 0 (plist-get proc :exit-status))
        (insert (plist-get proc :output))
      (error "Shell process %s exited with status %d!"
             cmd
             (plist-get proc :exit-status)))
    ))
;; --------------------------------------------------------------------------------
