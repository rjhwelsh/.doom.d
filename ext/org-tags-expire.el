;;; ext/org-tags-expire.el -*- lexical-binding: t; -*-

;; org persistent tag expiry functions

;;   Expiration tags are temporary tags for grouping TODO items in a short-term
;; meaningful way. These functions quickly expunge these temporary tags in bulk, to
;; facilitate better short-term planning and turnover.

(defcustom rjh-org-tags-expiration-tags
  (mapcar 'car org-tag-persistent-alist)
  "Org-mode tags which can be removed quickly, with org-tags-expire."
  ;; The rest of this is copied from org-tag-persistent-alist
  :group 'org-tags
  :type '(repeat
          (choice
           (cons :tag "Tag with key"
                 (string    :tag "Tag name")
                 (character :tag "Access char"))
           (list :tag "Tag" (string :tag "Tag name"))
           (const :tag "Start radio group" (:startgroup))
           (const :tag "Start tag group, non distinct" (:startgrouptag))
           (const :tag "Group tags delimiter" (:grouptags))
           (const :tag "End radio group" (:endgroup))
           (const :tag "End tag group, non distinct" (:endgrouptag))
           (const :tag "New line" (:newline)))))

;;    Generic interactive function.
;;    - Will expire tags for current item
;;    - Will expire tags for entire buffer when called with a prefix.
(defun rjh-org-tags-expire (ARG)
  "Expires all expiration tags.
PREFIXARG = Expire tags in buffer if non-nil"
  (interactive "p")
  (if (>= ARG 4)
      (rjh-org-tags-expire-buffer)
    (if (org-region-active-p)
        (call-interactively 'rjh-org-tags-expire-region)
      (rjh-org-tags-expire-entry))))

(defun rjh-org-tags-expire-headline ( expiration-tags )
  "Removes all expiration tags from headline."
  (let ((newtagslist (org-get-tags nil t)))
    (unless (null newtagslist)
      (dolist (element expiration-tags)
        (when (member element newtagslist)
          (setq newtagslist (delete element newtagslist))))
      (org-set-tags newtagslist)
      (org-reveal))))

(defun rjh-org-tags-expire-entry ()
  "Expires all expiration tags in current entry."
  (interactive)
  (save-excursion
    (org-back-to-heading 't)
    (rjh-org-tags-expire-headline rjh-org-tags-expiration-tags)))

(defun rjh-org-tags-expire-buffer ()
  "Expires all expiration tags in current buffer.
Includes invisible heading lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (outline-next-heading)
      (rjh-org-tags-expire-headline rjh-org-tags-expiration-tags))))

(defun rjh-org-tags-expire-region (start end)
  "Expires all expiration tags in current region."
  (interactive "r")
  (dolist (element rjh-org-tags-expiration-tags)
    (org-change-tag-in-region start end element 'off)))

(defun rjh-org-tags-expire-agenda-headline ( expiration-tags )
  "Removes all expiration tags from an AGENDA headline."
  (dolist (element expiration-tags)
    (org-agenda-set-tags element 'off)))

(defun rjh-org-tags-expire-agenda-buffer ()
  "Removes all expiration tags from an AGENDA buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (org-agenda-next-item 1)
                (next-single-property-change (point-at-eol) 'org-marker))
      (rjh-org-tags-expire-agenda-headline rjh-org-tags-expiration-tags))))

(defun rjh-org-tags-expire-agenda (ARG)
  "Expires tags in org-agenda view.
With prefix argument will expire tags for every item in the agenda view"
  (interactive "p")
  (save-excursion
    (if (>= ARG 4)
        (rjh-org-tags-expire-agenda-buffer)
      (if (org-region-active-p)
          (call-interactively 'rjh-org-tags-expire-region)
        (rjh-org-tags-expire-agenda-headline rjh-org-tags-expiration-tags)))))
