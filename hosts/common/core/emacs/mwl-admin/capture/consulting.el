;;; mwlabs/capture/consulting.el --- Consulting capture templates -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'mwlabs-pricing)

(defun mwlabs-get-consulting-filename (title)
  "Generate filename for consulting offer based on TITLE."
  (let ((date (format-time-string "%Y%m%d"))
        (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" title)))
    (format "~/sync/work/mwlabs/admin/offers/%s-%s.org" date sanitized-title)))

;; Add our template to org-capture-templates
(add-to-list 'org-capture-templates
             '("c" "Consulting Offer"
               plain
               (file (lambda () (mwlabs-get-consulting-filename (read-string "Project title: "))))
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

* Client Information:
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
  - **Offer Number**: %(format-time-string \"%%y%%m%%d-%%03d\" (current-time) (random 1000))
  - **Date**: %(format-time-string \"%%d/%%m/%%Y\" (current-time))
  - **Validity**: until %(format-time-string \"%%d/%%m/%%Y\" (time-add (current-time) (days-to-time 30)))

* Project Description
%^{Project Description}

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
- MWLabs acts as technical consultant only (not project lead)
- All prices are exclusive of VAT
- Travel and accommodation abroad are invoiced separately
- This proposal can be included in R&D budgets or subsidy applications
- Payment terms: 30 days from invoice date
- Travel time is billed at 50% of the hourly rate
- Minimum billing period is 30 minutes

* Payment Information
  - **Account holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857

* Signature

Please confirm acceptance by returning this signed offer by email.

|                    | For Client              |
|--------------------+-------------------------|
| Name:              |                         |
| Signature:         |                         |
| Date:              |                         |"))

(provide 'mwlabs-capture-consulting) 