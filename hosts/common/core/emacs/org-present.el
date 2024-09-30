;;; org-present.el --- Org present configuration

;;; Commentary:
;; This file contains settings for using Org mode as a presentation tool,
;; including hooks for starting and ending presentations.

;;; Code:
(require 'org-present)

;; Store the initial state of some modes and variables
(defvar-local my/org-present-initial-line-numbers-mode nil)
(defvar-local my/org-present-initial-visual-line-mode nil)
(defvar-local my/org-present-initial-org-startup-folded nil)

(defun my/org-present-start ()
  "Configure the presentation environment when org-present starts."
  ;; Store the initial state
  (setq my/org-present-initial-line-numbers-mode display-line-numbers-mode)
  (setq my/org-present-initial-visual-line-mode visual-line-mode)
  (setq my/org-present-initial-org-startup-folded org-startup-folded)
  
  ;; Enable Olivetti mode for centering the presentation
  (olivetti-mode 1)
  ;; Enable visual line mode for proper text wrapping
  (visual-line-mode 1)
  ;; Increase text size for better visibility
  (text-scale-increase 2)
  ;; Hide the mode line for a cleaner look
  (hide-mode-line-mode 1)
  ;; Disable line numbers
  (display-line-numbers-mode 0)
  ;; Center the buffer content
  (olivetti-set-width 0.4)
  ;; Show all content in the org file
  (org-show-all)
  ;; Disable org-startup-folded for the presentation
  (setq-local org-startup-folded nil))

(defun my/org-present-end ()
  "Restore the normal editing environment when org-present ends."
  ;; Disable Olivetti mode
  (olivetti-mode 0)
  ;; Reset visual line mode to its initial state
  (visual-line-mode (if my/org-present-initial-visual-line-mode 1 -1))
  ;; Reset text size
  (text-scale-set 0)
  ;; Show the mode line again
  (hide-mode-line-mode 0)
  ;; Reset line numbers to their initial state
  (display-line-numbers-mode (if my/org-present-initial-line-numbers-mode 1 -1))
  ;; Reset org-startup-folded to its initial state
  (setq-local org-startup-folded my/org-present-initial-org-startup-folded)
  ;; Show all content
  (org-show-all)
  ;; Redraw the display
  (redraw-display))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

;; Function to toggle the display of inline images
(defun my/org-present-toggle-images ()
  "Toggle the display of inline images in the current buffer."
  (interactive)
  (org-toggle-inline-images))

;; Ensure the entire document is visible after quitting org-present
(defun my/org-present-quit-and-show-all ()
  "Quit org-present and show all content."
  (interactive)
  (org-present-quit)
  (org-show-all)
  (org-remove-inline-images))

(defun my/org-present-mode-hook ()
  "Hook to run when org-present-mode is activated."
  (evil-make-overriding-map org-present-mode-map 'normal)
  (evil-normalize-keymaps))

(add-hook 'org-present-mode-hook 'my/org-present-mode-hook)

(provide 'org-present)
;;; org-present.el ends here