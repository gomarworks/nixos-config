;;; org-capture-templates.el --- Org capture templates for MWLabs
;;; Commentary:
;; Capture templates for offers, quick notes, etc.

;;; Code:

(require 'org)
(require 'org-capture)

;; Set CSS file for HTML export
(setq org-html-head-include-default-style nil)
(setq org-html-head-extra
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/invoice.css\" />"))

;; Get the last invoice number from the invoices directory
(defun mwlabs/get-last-invoice-number ()
  "Get the last invoice number from the invoices directory."
  (let ((invoices-dir "~/sync/work/mwlabs/admin/invoices/")
        (last-number 0))
    (when (file-directory-p invoices-dir)
      (dolist (file (directory-files invoices-dir nil "\\.org$"))
        (when (string-match "-invoice-\\([0-9]+\\)" file)
          (let ((num (string-to-number (match-string 1 file))))
            (when (> num last-number)
              (setq last-number num)))))
      (1+ last-number))
    1))

;; Generate sequential invoice number
(defun mwlabs/generate-invoice-number ()
  "Generate sequential invoice number."
  (format "%06d" (mwlabs/get-last-invoice-number)))

;; Prompted offer filename with auto-appended date
(defun mwlabs/org-capture-offer-path ()
  "Prompt for offer name, append 'offer' and current date, return full path."
  (let* ((name (read-string "Offer name (e.g. ugani-QC-pilot): "))
         (filename (concat name "-offer-" (format-time-string "%Y-%m-%d") ".org")))
    (expand-file-name filename "~/sync/work/mwlabs/admin/proposals/")))

;; Prompted invoice filename with auto-appended date
(defun mwlabs/org-capture-invoice-path ()
  "Prompt for invoice name, append 'invoice' and current date, return full path."
  (let* ((name (read-string "Invoice name (e.g. standard-ahz): "))
         (filename (concat name "-invoice-" (format-time-string "%Y-%m-%d") ".org")))
    (expand-file-name filename "~/sync/work/mwlabs/admin/invoices/")))

;; Capture templates
(setq org-capture-templates
      '(("o" "Offer Template"
         plain
         (file+function mwlabs/org-capture-offer-path)
         "#+TITLE: %^{Title}
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil
#+EXPORT_FILE_NAME: ../%t
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/invoice.css\" />

* MWLabs
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-left
  :END:
  - Nieuwstraat 22, 3018 Wijgmaal, Belgium
  - Website: [[http://mwlabs.nl][mwlabs.nl]], info@mwlabs.nl
  - Phone: +32 492 42 65 35
  - VAT: BE 0722.784.513

* Client Information:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - %^{Company Name}
  - %^{Company Address}
  - VAT: %^{Company VAT}

* Offer Information:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - **Offer Number**: O-%(format-time-string \"%y%m%d\")
  - **Date**: %(format-time-string \"%Y-%m-%d\")
  - **Valid Until**: %^{Valid Until|%Y-%m-%d}

* Project Description:
%^{Project Description}

* Service Details:
|----------+----------------------------------+-------------+-------|
| Quantity | Description                      | Rate        | Total |
|----------+----------------------------------+-------------+-------|
|       10 | Example service                  | €20         | €200  |
|----------+----------------------------------+-------------+-------|
|          | **excl. VAT**                    |             | €200  |
|          | **21% VAT**                      |             | €42   |
|----------+----------------------------------+-------------+-------|
|          | **Total incl. VAT**              |             | €242  |
|----------+----------------------------------+-------------+-------|

* Terms and Conditions:
  - This offer is valid for 30 days from the date of issue
  - Payment terms: 30 days from invoice date
  - All prices are in EUR and exclude VAT unless stated otherwise

* Contact Information:
  - **Contact Person**: Thibault Van den Bossche
  - **Email**: info@mwlabs.nl
  - **Phone**: +32 492 42 65 35")

        ("i" "Invoice Template"
         plain
         (file+function mwlabs/org-capture-invoice-path)
         "#+TITLE: %^{Title}
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil
#+EXPORT_FILE_NAME: ../%t
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/invoice.css\" />

* MWLabs
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-left
  :END:
  - Nieuwstraat 22, 3018 Wijgmaal, Belgium
  - Website: [[http://mwlabs.nl][mwlabs.nl]], info@mwlabs.nl
  - Phone: +32 492 42 65 35
  - VAT: BE 0722.784.513

* Bill to:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - %^{Company Name}
  - %^{Company Address}
  - VAT: %^{Company VAT}

* Invoice Information:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - **Invoice Number**: M-%(mwlabs/generate-invoice-number)
  - **Date**: %(format-time-string \"%Y-%m-%d\")

* Service notes:
%^{Service Notes}
|----------+----------------------------------+-------------+-------|
| Quantity | Description                      | Rate        | Total |
|----------+----------------------------------+-------------+-------|
|       10 | Letters unoptimized short notice | €20 (+€35+) | €200  |
|----------+----------------------------------+-------------+-------|
|          | **excl. VAT**                    |             | €200  |
|          | **21% VAT**                      |             | €42   |
|----------+----------------------------------+-------------+-------|
|          | **Total incl. VAT**              |             | €242  |
|----------+----------------------------------+-------------+-------|

* Payment Information:
  - **Account Holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857"
         :immediate-finish t
         :prepend t
         :empty-lines 1)

        ("n" "Quick Note"
         entry
         (file+headline "~/sync/notes/inbox.org" "Quick Notes")
         "* %^{Title}\n%U\n\n%?"
         :empty-lines 1)))

;; Global capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'org-capture-templates)
;;; org-capture-templates.el ends here 
