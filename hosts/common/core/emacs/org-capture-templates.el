;;; org-capture-templates.el --- Org capture templates for MWLabs
;;; Commentary:
;; Capture templates for offers, quick notes, etc.

;;; Code:

(require 'org)
(require 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML Export Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure HTML export settings
(setq org-html-head-include-default-style nil)  ; Don't include default CSS
(setq org-html-htmlize-output-type 'css)        ; Use CSS for syntax highlighting
(setq org-html-validation-link nil)             ; Don't include validation link
(setq org-html-head
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"~/.emacs.d/invoice.css\" />"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwlabs/format-currency (amount)
  "Format AMOUNT as currency with Euro symbol."
  (format "€%.2f" amount))

(defun mwlabs/calculate-vat (amount &optional rate)
  "Calculate VAT for AMOUNT with optional RATE (defaults to 21%)."
  (let ((rate (or rate 21)))
    (* amount (/ rate 100.0))))

(defun mwlabs/format-date-readable (date)
  "Format DATE as DD/MM/YYYY for document content."
  (format-time-string "%d/%m/%Y" date))

(defun mwlabs/format-date-iso (date)
  "Format DATE as YYYY-MM-DD for filenames."
  (format-time-string "%Y-%m-%d" date))

(defun mwlabs/get-default-validity-date ()
  "Get default validity date (30 days from now)."
  (let ((date (current-time)))
    (setf (nth 0 date) (+ (nth 0 date) (* 30 24 60 60)))
    (mwlabs/format-date-readable date)))

(defun mwlabs/get-export-filename (type)
  "Get export filename based on document title and TYPE."
  (let ((title (org-entry-get (point-min) "TITLE")))
    (format "%s-%s-%s" title type (mwlabs/format-date-iso (current-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number Generation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mwlabs/get-last-number (type)
  "Get the last number for today's date for TYPE (offer or invoice).
   Returns the next number in sequence for the given type."
  (let* ((today (format-time-string "%y%m%d"))
         (dir (expand-file-name (format "~/sync/work/mwlabs/admin/%s/" 
                                      (if (eq type 'offer) "proposals" "invoices"))))
         (last-number 0))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir nil "\\.org$"))
        (when (string-match (concat today "-" (symbol-name type) "-\\([0-9]+\\)") file)
          (let ((num (string-to-number (match-string 1 file))))
            (when (> num last-number)
              (setq last-number num)))))
      (1+ last-number))
    1))

(defun mwlabs/generate-number (type)
  "Generate sequential number with date prefix for TYPE (offer or invoice).
   Format: YYMMDD-XXX (e.g., 240315-001)"
  (format "%s-%03d" (format-time-string "%y%m%d") (mwlabs/get-last-number type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture Templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
      '(("o" "Offer Template"
         plain
         (file (lambda () (format "~/sync/work/mwlabs/admin/proposals/%s-offer-%s.org"
                                 (read-string "Title (e.g., Project X): ")
                                 (mwlabs/format-date-iso (current-time)))))
         "#+TITLE: %1
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil
#+EXPORT_FILE_NAME: %(mwlabs/get-export-filename 'offer)
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"file:///home/gomar/.emacs.d/invoice.css\" />

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
  - **Offer Number**: %(mwlabs/generate-number 'offer)
  - **Date**: %(mwlabs/format-date-readable (current-time))
  - **Validity**: until %(mwlabs/get-default-validity-date)

* Project Summary
%^{Project Description}

* Service Details
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

* Terms & Conditions
- MWLabs acts as technical consultant only (not project lead)
- All prices are exclusive of VAT
- Travel and accommodation abroad are invoiced separately
- This proposal can be included in R&D budgets or subsidy applications

* Payment Information
  - **Account holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857

* Signature

Please confirm acceptance by returning this signed offer by email.

|                    | For Client              |
|--------------------+-------------------------|
| Name:              |                         |
| Signature:         |                         |
| Date:              |                         |")

        ("i" "Invoice Template"
         plain
         (file (lambda () (format "~/sync/work/mwlabs/admin/invoices/%s-invoice-%s.org"
                                 (read-string "Title (e.g., Project X): ")
                                 (mwlabs/format-date-iso (current-time)))))
         "#+TITLE: %1
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil
#+EXPORT_FILE_NAME: %(mwlabs/get-export-filename 'invoice)
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"file:///home/gomar/.emacs.d/invoice.css\" />

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
  - **Invoice Number**: %(mwlabs/generate-number 'invoice)
  - **Date**: %(mwlabs/format-date-readable (current-time))

* Service Details
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

* Terms & Conditions
- All prices are exclusive of VAT
- Payment terms: 30 days from invoice date
- Travel and accommodation are invoiced separately

* Payment Information
  - **Account holder**: Thibault Van den Bossche
  - **IBAN**: BE09 7340 3298 4857")

        ("n" "Quick Note"
         entry
         (file+headline "~/sync/notes/inbox.org" "Quick Notes")
         "* %^{Title}\n%U\n\n%?"
         :empty-lines 1)

        ("p" "3D Print Estimate"
         plain
         (file (lambda () (format "~/sync/work/mwlabs/admin/proposals/%s-3dprint-%s.org"
                                 (read-string "Title (e.g., Model X): ")
                                 (mwlabs/format-date-iso (current-time)))))
         "#+TITLE: %1
#+AUTHOR: MWLabs
#+OPTIONS: toc:nil num:nil timestamp:nil author:nil date:nil
#+EXPORT_FILE_NAME: %(mwlabs/get-export-filename 'offer)
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"file:///home/gomar/.emacs.d/invoice.css\" />

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
  - **Offer Number**: %(mwlabs/generate-number 'offer)
  - **Date**: %(mwlabs/format-date-readable (current-time))
  - **Validity**: until %^{Valid Until|%(mwlabs/get-default-validity-date)}

* 3D Printing Details
|----------+----------------------------+---------+-------|
| Quantity | Description                | Rate    | Total |
|----------+----------------------------+---------+-------|
| %^{Print time (hours)}h | Machine time (RatRig) | €5.00/h | %(let ((h (string-to-number \"%^{Print time (hours)}\"))) (format \"€%.2f\" (* h 5.0))) |
| %^{Filament (grams)}g | Filament (PLA/PETG)        | €0.05/g | %(let ((f (string-to-number \"%^{Filament (grams)}\"))) (format \"€%.2f\" (* f 0.05))) |
|----------+----------------------------+---------+-------|
|          | **Subtotal**               |         | %(let* ((h (string-to-number \"%^{Print time (hours)}\"))
               (f (string-to-number \"%^{Filament (grams)}\"))
               (cost (+ (* h 5.0) (* f 0.05))))
          (format \"€%.2f\" cost)) |
|          | **Failure Buffer (10%)**   |         | %(let* ((h (string-to-number \"%^{Print time (hours)}\"))
               (f (string-to-number \"%^{Filament (grams)}\"))
               (cost (+ (* h 5.0) (* f 0.05)))
               (buffer (* cost 0.1)))
          (format \"€%.2f\" buffer)) |
|----------+----------------------------+---------+-------|
|          | **Total excl. VAT**        |         | %(let* ((h (string-to-number \"%^{Print time (hours)}\"))
               (f (string-to-number \"%^{Filament (grams)}\"))
               (cost (+ (* h 5.0) (* f 0.05)))
               (total (* cost 1.1)))
          (format \"€%.2f\" total)) |
|          | **21% VAT**                |         | %(let* ((h (string-to-number \"%^{Print time (hours)}\"))
               (f (string-to-number \"%^{Filament (grams)}\"))
               (cost (+ (* h 5.0) (* f 0.05)))
               (total (* cost 1.1))
               (vat (* total 0.21)))
          (format \"€%.2f\" vat)) |
|----------+----------------------------+---------+-------|
|          | **Total incl. VAT**        |         | %(let* ((h (string-to-number \"%^{Print time (hours)}\"))
               (f (string-to-number \"%^{Filament (grams)}\"))
               (cost (+ (* h 5.0) (* f 0.05)))
               (total (* cost 1.1))
               (vat (* total 0.21)))
          (format \"€%.2f\" (+ total vat))) |
|----------+----------------------------+---------+-------|

* Print Specifications
- **Model Name**: %^{Model Name}
- **Material**: %^{Material|PLA|PETG|ABS|TPU}
- **Color**: %^{Color}
- **Estimated Print Time**: %^{Print time (hours)} hours
- **Estimated Material Usage**: %^{Filament (grams)} grams

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
| Date:              |                         |")

        ("n" "Quick Note"
         entry
         (file+headline "~/sync/notes/inbox.org" "Quick Notes")
         "* %^{Title}\n%U\n\n%?"
         :empty-lines 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set global capture keybinding
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'org-capture-templates)
;;; org-capture-templates.el ends here 
