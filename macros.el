;;; $DOOMDIR/macros.el -*- lexical-binding: nil; -*-

;; These are user-defined macros/functions to enhance the functionality of emacs

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
