;;; org-capture-templates.el --- Org capture templates for MWLabs
;;; Commentary: Capture templates for offers, quick notes, etc.

;;; Code:

(defun mwlabs/org-capture-offer-path ()
  "Prompt for offer name, append 'offer' and current date, return full path."
  (let* ((name (read-string "Offer name (e.g. ugani-QC-pilot): "))
         (filename (concat name "-offer-" (format-time-string "%Y-%m-%d") ".org")))
    (expand-file-name filename "~/Sync/work/mwlabs/admin/proposals/")))

(setq org-capture-templates
      '(("o" "Offer Template"
         plain
         (file+function mwlabs/org-capture-offer-path find-file)
         (file "~/Sync/work/mwlabs/admin/proposals/standard-offer-template.org"))
        
        ("n" "Quick Note"
         entry
         (file+headline "~/Sync/notes/inbox.org" "Quick Notes")
         "* %^{Title}\n%U\n\n%?"
         :empty-lines 1)))

(provide 'org-capture-templates)
;;; org-capture-templates.el ends here
