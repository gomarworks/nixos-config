;;; mwl/pricing.el --- Pricing calculations for MWL -*- lexical-binding: t; -*-

;; Service-specific rates
(defconst mwl-consulting-rate 75.0
  "Standard consulting rate per hour in euros (600/8).")

(defconst mwl-3dprint-time-rate 6.0
  "3D printing time rate per hour in euros.")

;; 3D Printing specific rates
(defconst mwl-3dprint-material-rates
  '((PLA . 0.05)
    (ABS . 0.07)
    (PETG . 0.06)
    (TPU . 0.08))
  "3D printing material rates per gram in euros.")

;; Generic calculation functions
(defun mwl-calculate-time-cost (hours rate)
  "Calculate time cost based on HOURS and RATE per hour."
  (* hours rate))

(defun mwl-calculate-daily-cost (days rate)
  "Calculate daily cost based on DAYS and RATE per day."
  (* days (* rate 8)))

(defun mwl-calculate-material-cost (amount rate)
  "Calculate material cost based on AMOUNT and RATE per unit."
  (* amount rate))

(defun mwl-calculate-subtotal (items)
  "Calculate subtotal for a list of ITEMS.
Each item should be a list of (type amount rate).
Type can be :time or :material."
  (cl-reduce #'+ (mapcar (lambda (item)
                          (pcase item
                            (`(:time ,amount ,rate) (mwl-calculate-time-cost amount rate))
                            (`(:material ,amount ,rate) (mwl-calculate-material-cost amount rate))))
                        items)))

(defun mwl-calculate-vat (amount &optional rate)
  "Calculate VAT for AMOUNT with optional RATE (defaults to 21%)."
  (* amount (or rate 0.21)))

(defun mwl-calculate-total (amount &optional vat-rate)
  "Calculate total including VAT for AMOUNT with optional VAT-RATE."
  (+ amount (mwl-calculate-vat amount vat-rate)))

;; 3D Printing specific functions
(defun mwl-3dprint-get-material-rate (material)
  "Get rate for 3D printing MATERIAL type."
  (let ((rate (cdr (assoc material mwl-3dprint-material-rates))))
    (if rate
        rate
      (error "Unknown 3D printing material type: %s" material))))

(defun mwl-3dprint-calculate-cost (hours grams material)
  "Calculate the cost of a 3D print based on time and material.
HOURS is the print time in hours.
GRAMS is the material weight in grams.
MATERIAL is the material type (PLA, ABS, PETG, or TPU)."
  (let* ((time-cost (mwl-calculate-time-cost hours mwl-3dprint-time-rate))
         (material-cost (mwl-calculate-material-cost grams (mwl-3dprint-get-material-rate material)))
         (subtotal (mwl-calculate-subtotal `((:time ,hours ,mwl-3dprint-time-rate)
                                           (:material ,grams ,(mwl-3dprint-get-material-rate material)))))
         (vat (mwl-calculate-vat subtotal))
         (total (mwl-calculate-total subtotal)))
    (list subtotal vat total)))

;; Consulting specific functions
(defun mwl-consulting-get-rate (type)
  "Get consulting rate for TYPE.
Currently only :standard is supported."
  (pcase type
    (:standard mwl-consulting-rate)
    (_ (error "Unknown consulting rate type: %s" type))))

(defun mwl-consulting-calculate-cost (hours type)
  "Calculate consulting cost based on HOURS and TYPE.
TYPE can be :standard for now."
  (let* ((rate (mwl-consulting-get-rate type))
         (subtotal (mwl-calculate-time-cost hours rate))
         (vat (mwl-calculate-vat subtotal))
         (total (mwl-calculate-total subtotal)))
    (list subtotal vat total)))

(provide 'mwl-pricing) 