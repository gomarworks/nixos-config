;;; keybindings.el --- Custom keybindings

;;; Commentary:
;; This file contains all custom keybindings for various modes,
;; including global, Org mode, and Org present mode bindings.

;;; Code:

;; Global Evil keybindings

;; Org-mode specific keybindings
(evil-define-key 'normal org-mode-map
  (kbd "<tab>") 'org-cycle
  (kbd "<S-tab>") 'org-global-cycle
  "gj" 'org-next-visible-heading
  "gk" 'org-previous-visible-heading
  "gh" 'org-up-element
  "gl" 'org-down-element)

;; Add the org-toggle-pretty-entities keybinding
(evil-define-key '(normal visual) org-mode-map
  (kbd "C-c m") 'org-toggle-pretty-entities)

;; Function to toggle org-present
(defun my/toggle-org-present ()
  "Toggle org-present mode."
  (interactive)
  (if (bound-and-true-p org-present-mode)
      (org-present-quit)
    (org-present)))

;; Add the org-present toggle keybinding
(evil-define-key '(normal visual insert) org-mode-map
  (kbd "C-c p") 'my/toggle-org-present)

(provide 'my-keybindings)
;;; my-keybindings.el ends here