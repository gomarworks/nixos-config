;;; mwlabs/export.el --- HTML export functionality for MWLabs -*- lexical-binding: t; -*-

(require 'ox-html)

(defun mwlabs-export-to-html ()
  "Export current buffer to HTML with custom styling."
  (interactive)
  (let ((org-html-head-include-default-style nil)
        (org-html-htmlize-output-type 'css)
        (org-html-validation-link nil)
        (org-html-head
         (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                 (expand-file-name "invoice.css" (file-name-directory load-file-name))
                 "\" />")))
    (org-html-export-to-html)))

(provide 'mwlabs-export) 