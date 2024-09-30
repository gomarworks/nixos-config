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

;; Add the org-modern toggle keybinding
(evil-define-key '(normal visual) org-mode-map
  (kbd "C-c m") 'my/toggle-org-modern)

;; Org-present specific keybindings
(with-eval-after-load 'org-present
  (evil-define-key '(normal visual) org-present-mode-map
    (kbd "C-<right>") 'org-present-next
    (kbd "C-<left>") 'org-present-prev
    "q" 'my/org-present-quit-and-show-all))

(provide 'my-keybindings)
;;; my-keybindings.el ends here