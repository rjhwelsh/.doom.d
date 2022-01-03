;;; $DOOMDIR/macros.el -*- lexical-binding: nil; -*-

;; These are user-defined macros/functions to enhance the functionality of emacs

;; org
(after! org

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

  (defun org-agenda-randomize-entries ()
    "Assigns (or reassigns) a random number to every agenda item."
    (org-map-entries
     '(org-set-property "RANDOM" (number-to-string (random most-positive-fixnum)))
     nil
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
        )

      (org-delete-property "ID")  ;; Delete ID from subtree
      (org-copy-subtree 1 nil 't) ;; copy subtree to kill ring

      ;; interupt kill-ring to prevent text accumulation
      (delete-region (save-excursion (org-back-to-heading) (point)) (org-end-of-subtree t t))

      (org-roam-capture-
       :node node
       :templates templates
       :props `(:immediate-finish ,immediate :finalize nil))
      ))

(map! (:map org-mode-map)
      :localleader
      :prefix ("m" . "org-roam")
      "s" #'org-roam-capture-subtree)
)
