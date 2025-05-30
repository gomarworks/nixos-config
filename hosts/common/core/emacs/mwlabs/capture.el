;;; mwlabs/capture.el --- Capture templates for MWLabs -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'mwlabs-pricing)

(defun mwlabs-get-offer-filename (title)
  "Generate filename for offer based on TITLE."
  (let ((date (format-time-string "%Y%m%d"))
        (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" title)))
    (format "~/sync/work/mwlabs/admin/offers/%s-%s.org" date sanitized-title)))

(defun mwlabs-capture-new-offer ()
  "Start a new offer capture."
  (interactive)
  (let* ((title (read-string "Project title: "))
         (filename (mwlabs-get-offer-filename title))
         (print-time (read-number "Print time (minutes): "))
         (material-weight (read-number "Material weight (grams): "))
         (material-type (completing-read "Material type: " '("PLA" "ABS" "PETG" "TPU")))
         (object-name (read-string "Print object name: "))
         (quantity (read-number "Quantity: " 1))
         (subtotal (mwlabs-calculate-subtotal material-weight (intern material-type) print-time))
         (vat (mwlabs-calculate-vat subtotal))
         (total (mwlabs-calculate-total subtotal)))
    (org-capture-string
     (format "#+TITLE: %s
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

* Print Specifications
- **Object Name**: %s
- **Material**: %s
- **Print Time**: %d minutes
- **Material Weight**: %d grams
- **Quantity**: %d

* Service Details
|----------+----------------------------------+-------------+-------|
| Quantity | Description                      | Rate        | Total |
|----------+----------------------------------+-------------+-------|
| %d | %s (%s, %d min) | €%.2f | €%.2f |
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
- Print quality is guaranteed to meet standard 3D printing tolerances
- Delivery time depends on current queue and print complexity
- Files must be provided in STL or G-code format
- Support structures may be required and will be added at our discretion

* Payment Information
  - **Account holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857

* Signature

Please confirm acceptance by returning this signed offer by email.

|                    | For Client              |
|--------------------+-------------------------|
| Name:              |                         |
| Signature:         |                         |
| Date:              |                         |"
             title
             object-name material-type print-time material-weight quantity
             quantity object-name material-type print-time
             (/ print-time 60.0) (* (/ print-time 60.0) mwlabs-time-rate)
             subtotal vat total)
     filename)))

(provide 'mwlabs-capture) 