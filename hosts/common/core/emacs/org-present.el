;;; org-present.el --- Org present configuration

;;; Commentary:
;; This file contains settings for using Org mode as a presentation tool,
;; including hooks for starting and ending presentations.

;;; Code:
(require 'org-present)
(require 'hide-mode-line)
(require 'olivetti)

(defun my/org-present-prepare-slide (buffer-name heading)
  "Prepare slide for presentation."
  ;; Show only top-level headlines
  (org-overview)
  ;; Unfold the current entry
  (org-show-entry)
  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  "Configure the presentation environment when org-present starts."
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (org-present-read-only)
  (hide-mode-line-mode 1)
  (olivetti-mode 1)
  (olivetti-set-width 50)
  ;; Set a blank header line with matching background color
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil
                      :background (face-attribute 'default :background)
                      :height 6.9
                      :underline nil)
  ;; Switch to variable pitch font
  (variable-pitch-mode 1))                      

(defun my/org-present-end ()
  "Restore the normal editing environment when org-present ends."
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write)
  (hide-mode-line-mode -1)
  (olivetti-mode 1)
  (olivetti-set-width 0.7)
  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil)
  (variable-pitch-mode -1))

(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(provide 'org-present)
;;; org-present.el ends here