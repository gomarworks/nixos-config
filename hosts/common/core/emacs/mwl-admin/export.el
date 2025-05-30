;;; mwlabs/export.el --- Export functionality for MWLabs -*- lexical-binding: t; -*-

(require 'org)
(require 'ox-html)

(defun mwlabs-export-to-html ()
  "Export the current buffer to HTML with custom styling."
  (interactive)
  (let ((org-html-head-include-default-style nil)
        (org-html-head-extra
         (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
                 (expand-file-name "~/.emacs.d/invoice.css"))))
    (org-html-export-to-html)))

(provide 'mwlabs-export) 