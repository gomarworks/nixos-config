;;; mwl/capture/3dprint.el --- 3D printing capture templates -*- lexical-binding: t; -*-

(require 'org)
(require 'org-capture)
(require 'mwl-pricing)

(defun mwl-get-offer-filename (title)
  "Generate filename for offer based on TITLE."
  (let ((date (format-time-string "%Y%m%d"))
        (sanitized-title (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" title)))
    (format "~/sync/work/mwl/admin/offers/%s-%s.org" date sanitized-title)))

(defun mwl-3dprint-calculate-table-values (quantity object material hours grams)
  "Calculate values for the 3D print offer table.
Returns a list of (rate total subtotal vat total-incl-vat)."
  (let* ((costs (mwl-3dprint-calculate-cost (string-to-number hours)
                                          (string-to-number grams)
                                          (intern material)))
         (subtotal (nth 0 costs))
         (vat (nth 1 costs))
         (total (nth 2 costs))
         (rate (/ subtotal (string-to-number hours))))
    (list rate total subtotal vat total)))

;; Add our template to org-capture-templates
(add-to-list 'org-capture-templates
             '("p" "3D Print Offer"
               plain
               (file+headline (lambda () (mwl-get-offer-filename (read-string "Project title (e.g. gear-housing): "))) "Offer")
               "#+TITLE: %^{Project Title}
#+AUTHOR: MWL
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil

* MWL
- Thibault Van den Bossche
- Nieuwstraat 22, 3018 Wijgmaal, Belgium
- info@mwl.be – [[http://mwl.be][mwl.be]]
- BE0722784513 – +32 492 42 65 35

* Client Information
- [Company Name]
- [Company Address]
- VAT: [VAT Number]

* Document Details
- **Project Reference**: [Project Reference]
- **Offer Number**: %(format-time-string \"%%y%%m%%d-%%03d\" (current-time) (random 1000))
- **Date**: %(format-time-string \"%%d/%%m/%%Y\" (current-time))
- **Validity**: until %(format-time-string \"%%d/%%m/%%Y\" (time-add (current-time) (days-to-time 30)))

* Print Specifications
- **Object Name**: %^{Object Name}
- **Material**: %^{Material Type|PLA|ABS|PETG|TPU}
- **Print Time**: %^{Print Time (hours)} hours
- **Material Weight**: %^{Material Weight (grams)} grams
- **Quantity**: %^{Quantity|1}

* Service Details
|----------+----------------------------------+-------------+-------|
| Quantity | Description                      | Rate        | Total |
|----------+----------------------------------+-------------+-------|
| %^{Quantity|1} | %^{Object Name} (%^{Material Type|PLA|ABS|PETG|TPU}, %^{Print Time (hours)}h) | %(let ((vals (mwl-3dprint-calculate-table-values \"%^{Quantity|1}\" \"%^{Object Name}\" \"%^{Material Type|PLA|ABS|PETG|TPU}\" \"%^{Print Time (hours)}\" \"%^{Material Weight (grams)}\"))) (format \"€%.2f\" (nth 0 vals))) | %(let ((vals (mwl-3dprint-calculate-table-values \"%^{Quantity|1}\" \"%^{Object Name}\" \"%^{Material Type|PLA|ABS|PETG|TPU}\" \"%^{Print Time (hours)}\" \"%^{Material Weight (grams)}\"))) (format \"€%.2f\" (nth 1 vals))) |
|----------+----------------------------------+-------------+-------|
|          | **Subtotal**                     |             | %(let ((vals (mwl-3dprint-calculate-table-values \"%^{Quantity|1}\" \"%^{Object Name}\" \"%^{Material Type|PLA|ABS|PETG|TPU}\" \"%^{Print Time (hours)}\" \"%^{Material Weight (grams)}\"))) (format \"€%.2f\" (nth 2 vals))) |
|          | **21%% VAT**                     |             | %(let ((vals (mwl-3dprint-calculate-table-values \"%^{Quantity|1}\" \"%^{Object Name}\" \"%^{Material Type|PLA|ABS|PETG|TPU}\" \"%^{Print Time (hours)}\" \"%^{Material Weight (grams)}\"))) (format \"€%.2f\" (nth 3 vals))) |
|----------+----------------------------------+-------------+-------|
|          | **Total incl. VAT**              |             | %(let ((vals (mwl-3dprint-calculate-table-values \"%^{Quantity|1}\" \"%^{Object Name}\" \"%^{Material Type|PLA|ABS|PETG|TPU}\" \"%^{Print Time (hours)}\" \"%^{Material Weight (grams)}\"))) (format \"€%.2f\" (nth 4 vals))) |
|----------+----------------------------------+-------------+-------|

* Terms & Conditions
- MWL acts as technical consultant only (not project lead)
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
| Date:              |                         |"))

(provide 'mwl-capture-3dprint) 