;;; mwlabs/init.el --- MWLabs invoice and offer generation system -*- lexical-binding: t; -*-

;; Get the directory where this file is located
(let ((mwlabs-dir (file-name-directory (or load-file-name buffer-file-name))))
  (message "MWLabs directory: %s" mwlabs-dir)
  
  ;; Load all modules in correct order using absolute paths
  (load-file (expand-file-name "pricing.el" mwlabs-dir))
  (load-file (expand-file-name "export.el" mwlabs-dir))
  (load-file (expand-file-name "capture/init.el" mwlabs-dir)))

;; Set up keybindings
(define-prefix-command 'mwlabs-command-map)
(global-set-key (kbd "C-c o") 'mwlabs-command-map)

;; Export to HTML
(define-key mwlabs-command-map (kbd "e") 'mwlabs-export-to-html)

(provide 'mwlabs-init) 