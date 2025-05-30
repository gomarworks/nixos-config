;;; mwlabs/capture/invoice.el --- Invoice capture templates -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'mwlabs-pricing)

(defun mwlabs-get-invoice-filename (title)
  "Generate filename for invoice based on TITLE."
  (let ((date (format-time-string "%Y%m%d"))
        (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" title)))
    (format "~/sync/work/mwlabs/admin/invoices/%s-%s.org" date sanitized-title)))

;; Add our template to org-capture-templates
(add-to-list 'org-capture-templates
             '("i" "Invoice"
               plain
               (file (lambda () (mwlabs-get-invoice-filename (read-string "Project title: "))))
               "#+TITLE: %1
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil

* MWLabs
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-left
  :END:
  - Thibault Van den Bossche
  - Nieuwstraat 22, 3018 Wijgmaal, Belgium
  - info@mwlabs.be – [[http://mwlabs.be][mwlabs.be]]
  - BE0722784513 – +32 492 42 65 35

* Bill to:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - %^{Company Name}
  - %^{Company Address}
  - VAT: %^{Company VAT}

* Document Details:
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: grid-item-right
  :END:
  - **Project Reference**: %^{Project Reference|}
  - **Invoice Number**: %(format-time-string \"%%y%%m%%d-%%03d\" (current-time) (random 1000))
  - **Date**: %(format-time-string \"%%d/%%m/%%Y\" (current-time))
  - **Due Date**: %(format-time-string \"%%d/%%m/%%Y\" (time-add (current-time) (days-to-time 30)))

* Service Details
|----------+----------------------------------+-------------+-------|
| Hours    | Description                      | Rate        | Total |
|----------+----------------------------------+-------------+-------|
| %^{Hours} | %^{Service Description} | €%^{Hourly Rate} | €%.2f |
|----------+----------------------------------+-------------+-------|
|          | **Subtotal**                     |             | €%.2f |
|          | **21%% VAT**                     |             | €%.2f |
|----------+----------------------------------+-------------+-------|
|          | **Total incl. VAT**              |             | €%.2f |
|----------+----------------------------------+-------------+-------|
#+TBLFM: $4=$2*$3::$5=$2*$3

* Terms & Conditions
- All prices are exclusive of VAT
- Payment terms: 30 days from invoice date
- Travel and accommodation are invoiced separately

* Payment Information
  - **Account holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857

* Notes
%^{Notes|}"))

(provide 'mwlabs-capture-invoice) 