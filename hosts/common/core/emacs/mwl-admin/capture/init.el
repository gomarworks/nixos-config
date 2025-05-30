;;; mwlabs/capture/init.el --- Initialize all capture templates -*- lexical-binding: t; -*-

;; Get the directory where this file is located
(let ((capture-dir (file-name-directory (or load-file-name buffer-file-name))))
  (message "Loading MWLabs capture templates from: %s" capture-dir)
  
  ;; Load all capture templates
  (load-file (expand-file-name "3dprint.el" capture-dir))
  (load-file (expand-file-name "consulting.el" capture-dir))
  (load-file (expand-file-name "invoice.el" capture-dir)))

(provide 'mwlabs-capture-init) 