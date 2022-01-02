;;; ext/org-agenda-skip.el -*- lexical-binding: nil; -*-

;;; TODO Add a function to skip based on subtree depth

;; Org-agenda skip functions
(defun org-agenda-skip-entry-if-blocked ()
  "Skip entry if it is blocked by another entry."
  (let ((next-headline
         (save-excursion
           (or (outline-next-heading) (point-max))))
        ;; Do not skip items blocked by checkboxes
        (org-enforce-todo-checkbox-dependencies nil))
    (if (org-entry-blocked-p) next-headline)))

(defun org-agenda-skip-entry-maxdepth (maxlevel)
  "Skip entry if level depth is greater than maxlevel."
  (let ((next-headline
         (save-excursion
           (or (outline-next-heading) (point-max)))))
    (if (> (org-current-level) maxlevel) next-headline)))

;; Regexp skip functions
(defun org-agenda-skip-if-regexp (skip-re)
  "Skip headline if regexp matches the headline"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (current-level (org-current-level))
         (match-p
          (save-excursion
            (let ((case-fold-search nil)
                  (eol (save-excursion (org-end-of-line nil) (point))))
              (re-search-forward
               skip-re eol t)))))
    (if match-p next-headline)))

(defun org-agenda-skip-element-if-property-regexp (prop skip-re)
  "Skip headline if regexp matches with the specified property; property must reference a string-value"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (match-p
          (let ((case-fold-search nil))
            (string-match
             skip-re
             (org-element-property prop (org-element-at-point))))))
    (if match-p next-headline)))

(defun org-agenda-skip-entry-if-file-path-regexp (regexp)
  "Skip entry if it is in a file on path that matches regexp"
  (let* ((path (buffer-file-name))
         (match-p (string-match regexp path)))
    (if match-p (point-max))))

;; Invert skip function
(defun org-agenda-skip-invert (skip-function &rest skip-func-args)
  "Skip headline if the SKIP-FUNCTION with SKIP-FUNC-ARGS returns nil"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (match-p (apply skip-function skip-func-args)))
    (if (not match-p) next-headline)))

;; Subtree functions
;; These sub-tree skipping functions are derived from =org-agenda-list-stuck-projects=.
(defun org-agenda-skip-subtree-if-regexp (skip-re)
  "Skip subtree if regexp matches anywhere inside subtree, not including current headline."
  ;; Skip entry if `org-agenda-skip-regexp' matches anywhere
  ;; in the subtree.
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (current-level (org-current-level))
         (match-p
          (save-restriction
            (widen)
            (save-excursion
              (let ((case-fold-search nil))
                (progn
                  ;; skip over current headline
                  (org-end-of-line nil)
                  (if (< (point) subtree-end)
                      (re-search-forward
                       skip-re subtree-end t))))))))
    (if match-p next-headline)))

(defun org-agenda-skip-subtree-if-tags (tags)
  "Skip subtree if any of the tags match.
Tags is a list of tags"
  (let* ((tags-re (cond ((null tags) nil)
                        ((member "*" tags) org-tag-line-re)
                        (tags
                         (let ((other-tags (format "\\(?:%s:\\)*" org-tag-re)))
                           (concat org-outline-regexp-bol
                                   ".*?[ \t]:"
                                   other-tags
                                   (regexp-opt tags t)
                                   ":" other-tags "[ \t]*$")))
                        (t nil)))
         (re-list (delq nil (list tags-re)))
         (skip-re
          (if (null re-list)
              (error "Missing information to identify unstuck projects")
            (mapconcat #'identity re-list "\\|"))))
    (org-agenda-skip-subtree-if-regexp skip-re)))

(defun org-agenda-skip-subtree-if-todo (todo)
  "Skip subtree if any of the todo keywords match.
todo is a list of todo keywords"
  (let* ((todo-wds
          (if (not (member "*" todo)) todo
            (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
            (org-delete-all org-done-keywords-for-agenda
                            (copy-sequence org-todo-keywords-for-agenda))))
         (todo-re (and todo
                       (format "^\\*+[ \t]+\\(%s\\)\\>"
                               (mapconcat #'identity todo-wds "\\|"))))
         (re-list (delq nil (list todo-re)))
         (skip-re
          (if (null re-list)
              (error "Missing information to identify unstuck projects")
            (mapconcat #'identity re-list "\\|"))))
    (org-agenda-skip-subtree-if-regexp skip-re)))


(defun org-agenda-skip-if-children (depth skip-function &rest skip-func-args )
  "Skip headline if any children match the SKIP-FUNCTION and SKIP-FUNC-ARGS
Any children below the DEPTH relative to the root node are ignored.
DEPTH = nil, will recursively search entire subtree
"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (current-level (org-current-level))
         (maximum-level
          (and
           depth
           (+ current-level depth)))
         (match-p
          (save-restriction
            (widen)
            (save-excursion
              (progn
                ;; skip over current headline
                (org-end-of-line nil)
                ;; Only match immediate children headlines with skip-function
                (let ((retval nil))
                  (cl-loop
                   ;; Return value or past end of subtree
                   (if
                       (or retval
                           (>= (point) subtree-end))
                       (cl-return retval))
                   (if
                       (outline-next-heading)
                       ;; Skip unless exactly 1 level deeper than current headline
                       (if
                           (or
                            (not maximum-level) ;; Accept any node if depth is nil
                            (and
                             (> (org-current-level) current-level) ;; deeper than subtree root
                             (<= (org-current-level) maximum-level) ;; not as deep as maximum level
                             ))
                           (setq retval (apply skip-function skip-func-args)))
                     ;; No more headings.. return
                     (cl-return retval)))))))))
    (if match-p next-headline)))

(defun org-agenda-skip-if-parent (skip-function &rest skip-func-args)
  "Skip headline if any immediate parents match the SKIP-FUNCTION and SKIP-FUNC-ARGS"
  (let* ((prev-headline (save-excursion (or (outline-previous-heading) (point-min))))
         (next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (current-level (org-current-level))
         (match-p
          (save-restriction
            (widen)
            (save-excursion
              (progn
                ;; Return nil if no parents
                (when (> (org-current-level) 1)
                  ;; Move to parent heading
                  (outline-up-heading 1)
                  ;; Apply skip function to immediate parent only
                  (apply skip-function skip-func-args))
                )))))
    (if match-p next-headline)))
