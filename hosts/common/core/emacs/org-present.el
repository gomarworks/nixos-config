(defun my/org-present-start ()
  ;; Enable Olivetti mode for centering the presentation
  (olivetti-mode 1)
  (visual-line-mode 1))  ;; Enable line wrapping

(defun my/org-present-end ()
  ;; Disable Olivetti mode (stop centering the document)
  (olivetti-mode 0)
  (visual-line-mode 0))  ;; Disable line wrapping

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

