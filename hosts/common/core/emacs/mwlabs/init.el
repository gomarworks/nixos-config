;;; mwlabs/init.el --- MWLabs invoice and offer generation system -*- lexical-binding: t; -*-

;; Load all modules
(require 'mwlabs-capture)
(require 'mwlabs-pricing)
(require 'mwlabs-export)

;; Set up keybindings
(define-prefix-command 'mwlabs-command-map)
(global-set-key (kbd "C-c o") 'mwlabs-command-map)

;; Offer capture
(define-key mwlabs-command-map (kbd "n") 'mwlabs-capture-new-offer)
;; Export to HTML
(define-key mwlabs-command-map (kbd "e") 'mwlabs-export-to-html)

(provide 'mwlabs-init) 