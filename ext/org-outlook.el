;;; org-outlook.el - Support for links to Outlook items in Org
;;; See https://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook

(require 'org)

(defvar org-outlook-exe "C:/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE"
 "Defines the location of the outlook.exe binary.")

(defun org-outlook-open (id)
   "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
   (w32-shell-execute "open" org-outlook-exe (concat "/select " "outlook:" id)))

(org-add-link-type "outlook" 'org-outlook-open)

(provide 'org-outlook)
;;; org-outlook.el ends here
