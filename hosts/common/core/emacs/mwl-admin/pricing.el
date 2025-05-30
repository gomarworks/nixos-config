;;; mwlabs/pricing.el --- Pricing calculations for MWLabs -*- lexical-binding: t; -*-

(defconst mwlabs-material-rates
  '((PLA . 0.05)
    (ABS . 0.07)
    (PETG . 0.06)
    (TPU . 0.08))
  "Material rates per gram in euros.")

(defconst mwlabs-time-rate 0.10
  "Rate per minute in euros.")

(defun mwlabs-calculate-material-cost (weight material)
  "Calculate material cost based on WEIGHT in grams and MATERIAL type."
  (let ((rate (cdr (assoc material mwlabs-material-rates))))
    (if rate
        (* weight rate)
      (error "Unknown material type: %s" material))))

(defun mwlabs-calculate-time-cost (minutes)
  "Calculate time cost based on MINUTES."
  (* minutes mwlabs-time-rate))

(defun mwlabs-calculate-subtotal (weight material minutes)
  "Calculate subtotal for given WEIGHT, MATERIAL, and MINUTES."
  (+ (mwlabs-calculate-material-cost weight material)
     (mwlabs-calculate-time-cost minutes)))

(defun mwlabs-calculate-vat (amount)
  "Calculate 21% VAT for AMOUNT."
  (* amount 0.21))

(defun mwlabs-calculate-total (amount)
  "Calculate total including VAT for AMOUNT."
  (+ amount (mwlabs-calculate-vat amount)))

(provide 'mwlabs-pricing) 